;;; This file is loaded before the generated FFI.

(in-package :hu.dwim.sdl)

(define-condition sdl-error (error)
  ())

(define-condition sdl-error/negative-return-code (simple-error sdl-error)
  ((error-code :initform (error "Must specify ERROR-CODE.")
               :accessor error-code-of
               :initarg :error-code)))

(cffi:define-foreign-type sdl-error-code (cffi::foreign-type-alias)
  ()
  (:default-initargs :actual-type (cffi::parse-type :int))
  (:simple-parser sdl-error-code))

(defmethod cffi:expand-from-foreign (value (type sdl-error-code))
  ;; NOTE: strictly speaking it should be (cffi:convert-from-foreign ,value :int), but not in this case.
  `(let ((return-code ,value))
     (if (< return-code 0)
         (error 'sdl-error/negative-return-code
                :error-code return-code
                :format-control "SDL call failed: ~S ~S"
                :format-arguments (list return-code (cl-moar-sdl2.core::sdl-get-error)))
         return-code)))

(defun ffi-name-export-predicate (symbol &key &allow-other-keys)
  (declare (ignore symbol))
  t)

;; (ffi-name-transformer "SDL_InitSubSystem" 'any)
;; (ffi-name-transformer "__u_char" 'any)
;; (ffi-name-transformer "SDL_HAPTIC_INFINITY" 'any)

;; (ffi-name-transformer "num_texture_formats" :field)
;; (ffi-name-transformer "__val" :field)
;; (ffi-name-transformer "BitsPerPixel" :field)
;; (ffi-name-transformer "Gshift" :field)

;; (ffi-name-transformer "X2" :argument)
;; (ffi-name-transformer "blendMode" :argument)
;; (ffi-name-transformer "Vplane" :argument)

;; (ffi-name-transformer "SDL_GetPlatform" :function)

;; (ffi-name-transformer "SDL_GL_UnbindTexture" :function)

(defmacro spec-process (result-sym regexp &body body)
  ;; Need `again', because this needs to be recursive as multiple passes are
  ;; required (regexps can conflict, aka SDL_GL will be sdl_GL on the first pass).
  `(labels ((again (result &optional (matched t))
              (if matched
                  (multiple-value-call #'again
                    (regex-replace-all
                     ,regexp result
                     (lambda (target-string start end match-start match-end reg-starts reg-ends)
                       (declare (ignorable start end reg-starts reg-ends))
                       (let ((str (subseq target-string match-start match-end)))
                         (declare (ignorable str))
                         ,@body))))
                  result)))
     (again ,result-sym)))

(defmacro spec-> (&rest rest)
  "Nest things, so that the first expression is embedded as the second argument
in the second expression, etc."
  (reduce (lambda (acc x)
            `(,(first x) ,acc ,@(rest x)))
          rest))

(defun caps-replace (result)
  (spec->
   result
   (spec-process "_GL[a-z]" (print str) (regex-replace-all "_GL" str "_gl_"))
   (spec-process "[^A-Z]GL[a-z]" (regex-replace-all "GL" str "_gl_"))
   (spec-process "[^A-Z]GL[A-Z][a-z]" (regex-replace-all "GL" str "_gl"))
   (spec-process "[^A-Z]WM[a-z]" (regex-replace-all "WM" str "wm_"))
   (spec-process "[^A-Z]RW[a-z]" (regex-replace-all "RW" str "rw_"))
   (spec-process "_GL|GL_" (string-downcase str))
   (spec-process "_[A-Z][a-z]" (string-downcase str))
   (spec-process "SDL_|IMG_|TTF_|GFX_" (string-downcase str))))

;; (caps-replace "SDL_GLprofile")
;; (caps-replace "SDL_GL_UnbindGLTexture") ; FAKE, a made-up function
;; (caps-replace "SDL_RWops")

(defun table-replace-p (name)
  (second (find name '(("SDL_Log" "SDL-LOG")
                       ("SDL_log" "SDL-LOG*")
                       ("SDL_TRUE" "TRUE")
                       ("SDL_FALSE" "FALSE"))
                :key #'first
                :test #'equal)))

;; (table-replace-p "SDL_Log")

;; (ffi-name-transformer "SDL_GLattr" :function)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignorable kind))
  (check-type name string)
  ;; (print (format nil "~a ~a" name kind))
  (labels ((_->- (name) (substitute #\- #\_ name))
           (from-camel (name) (cffi/c2ffi:camelcase-to-dash-separated name))
           (muff (name char) (concatenate 'string char name char))
           (downcase (name) (string-downcase name))
           (as-const (name) (muff (downcase name) "+"))
           (as-global (name) (muff (downcase name) "*"))
           (as-field (name) (from-camel name))
           (as-type (name) name)
           (as-function (name) (from-camel (caps-replace name))))
    (cffi-sys:canonicalize-symbol-name-case
     (or
      (table-replace-p name)
      (_->-
       (case kind
         (:constant                     ; aka "_BITS_TYPES_H"
          (as-const name))
         (:member    ; member of an enum, aka "SDL_ASSERTION_ABORT", "SDL_FALSE"
          (as-const name)) 
         (:variable                     ; none found
          (as-global name))
         (:field     ; "__val", "BitsPerPixel", "Gshift", "num_texture_formats" 
          (as-field name))
         (:argument                     ; "str", "X2", "blendMode", "Vplane" 
          (as-field name))
         (:function                     ; "SDL_GetNumVideoDisplays"
          (as-function name))
         (:struct                       ; "SDL_HAPTICCONDITION"
          (as-type name))
         (:union                        ; "SDL_HAPTICEFFECT"
          (as-type name))
         (:enum                         ; none found (all are anonymous)
          (as-type name))
         (:type                         ; none passed to this function
          (as-type name))
         (otherwise name)))))))

(defun ffi-type-transformer (type context &rest args &key &allow-other-keys)
  (let ((type (apply 'cffi/c2ffi:default-ffi-type-transformer type context args)))
    (cond
      ((and (consp context)
            (eq (first context) :function)
            (eq (third context) :return-type)
            (member (second context)
                    ;; TODO this list is by far not complete. see this SDL bug for details:
                    ;; https://bugzilla.libsdl.org/show_bug.cgi?id=3219
                    '("SDL_Init"
                      "TTF_Init"
                      "IMG_Init"
                      "SDL_SetRenderDrawColor"
                      "SDL_RenderClear"
                      "SDL_RenderCopyEx"
                      "SDL_RenderCopyEx"
                      "SDL_RenderDrawLines"
                      "SDL_RenderDrawLines"
                      "SDL_RenderDrawPoint"
                      "SDL_RenderDrawPoints"
                      "SDL_RenderDrawRect"
                      "SDL_RenderDrawRects"
                      "SDL_RenderFillRect"
                      "SDL_RenderFillRects"
                      "SDL_RenderReadPixels"
                      "SDL_RenderSetClipRect"
                      "SDL_RenderSetLogicalSize"
                      "SDL_RenderSetScale"
                      "SDL_RenderSetViewport"
                      )
                    :test 'equal))
       (assert (eq type :int))
       ;; this is a cffi type that automatically signals an error if the return code is negative.
       'sdl-error-code)
      #+nil
      ((equal context '(:struct "hci_dev_info" "name"))
       (assert (equal type '(:array :char 8)))
       ;; err, no, this dereferences a pointer
       :string)
      (t
       type))))
