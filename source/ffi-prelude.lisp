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
  ;; cffi/c2ffi seems to be nothing like `kind' here...  but could be reasonable
  ;; to skip the argument names.  although who knows, maybe they can be named as
  ;; other things, so can't simply check for membership in +ARGUMENT-NAMES+.
  #+nil                  ; nah, members (enum consts) wouldn't be exported then.
  (let ((*package* (find-package :cl-moar-sdl2.core)))
    (not (member symbol (append +FIELD-NAMES+
                                ;;+ARGUMENT-NAMES+
                                +CONSTANT-NAMES+
                                +TYPE-NAMES+
                                +VARIABLE-NAMES+
                                +UNION-NAMES+
                                +STRUCT-NAMES+
                                +FUNCTION-NAMES+)
                 :key #'cdr
                 :test #'eql)))
  t)

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

(defun concat (&rest rest)
  (apply #'concatenate 'string rest))

(defun process-abbrev (abbrev)
  `((spec-process ,(concat "[^_^]" abbrev "$")
        (regex-replace-all ,abbrev str ,(concat "_" (string-downcase abbrev))))
    (spec-process ,(concat "_" abbrev "[a-z]")
        (regex-replace-all ,abbrev str ,(concat (string-downcase abbrev) "_")))
    (spec-process ,(concat "[^A-Z]" abbrev "[a-z]")
        (regex-replace-all ,abbrev str ,(concat "_" (string-downcase abbrev) "_")))
    (spec-process ,(concat "[^A-Z]" abbrev "[A-Z][a-z]")
        (regex-replace-all ,abbrev str ,(concat "_" (string-downcase abbrev))))
    (spec-process ,(concat "(^|_)" abbrev "(_|$)") (string-downcase str))))

(defun caps-replace (result)
  (eval
   `(spec->
     ,result
     ;; note: have to process GUID before ID 
     ,@(append (mappend #'process-abbrev
                        (list "UNICODE" "UTF8" "GUID" "ID" "GL" "WM" "RW")))
     (spec-process "[^A-Z](GUID|RW|WM|GL|ID)$"
         (concat (subseq str 0 1) (concat "_" (string-downcase (subseq str 1)))))
     (spec-process "_[A-Z][a-z]" (string-downcase str))
     (spec-process "SDL_|IMG_|TTF_|GFX_" (string-downcase str)))))

(defun table-replace-p (name)
  (second (find name '(("SDL_Log" "SDL-LOG")
                       ("SDL_log" "SDL-LOG*")
                       ("SDL_TRUE" "TRUE")
                       ("SDL_FALSE" "FALSE"))
                :key #'first
                :test #'equal)))

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignorable kind))
  (check-type name string)
  (let ((name* (caps-replace name)))
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
           (:constant                   ; aka "_BITS_TYPES_H"
            (as-const name*))
           (:member  ; member of an enum, aka "SDL_ASSERTION_ABORT", "SDL_FALSE"
            (as-const name*)) 
           (:variable                   ; none found
            (as-global name*))
           (:field   ; "__val", "BitsPerPixel", "Gshift", "num_texture_formats" 
            (as-field name*))
           (:argument                   ; "str", "X2", "blendMode", "Vplane" 
            (as-field name*))
           (:function                   ; "SDL_GetNumVideoDisplays"
            (as-function name*))
           (:struct                     ; "SDL_HAPTICCONDITION"
            (as-type name*))
           (:union                      ; "SDL_HAPTICEFFECT"
            (as-type name*))
           (:enum                       ; none found (all are anonymous)
            (as-type name*))
           (:type                       ; none passed to this function
            (as-type name*))
           (otherwise name))))))))

(parachute:define-test+run various-transforms
  (loop for (kind input expected) in
        '((:function "SDL_InitSubSystem" "SDL-INIT-SUB-SYSTEM")
          (:field "__u_char" "--U-CHAR")
          (:constant "SDL_HAPTIC_INFINITY" "+SDL-HAPTIC-INFINITY+")
          (:field "num_texture_formats" "NUM-TEXTURE-FORMATS")
          (:field "__val" "--VAL")
          (:field "BitsPerPixel" "BITS-PER-PIXEL")
          (:argument "X2" "X2")
          (:argument "blendMode" "BLEND-MODE")
          (:argument "Vplane" "VPLANE"))
        do (parachute:is equal expected (ffi-name-transformer input kind))))

(parachute:define-test+run name-caps
  (loop for (kind input expected) in
        '((:type "SDL_GLprofile" "SDL-GL-PROFILE")
          (:function "SDL_GL_UnbindGLTexture" "SDL-GL-UNBIND-GL-TEXTURE") ; FAKE, a made-up function
          (:struct "SDL_RWops" "SDL-RW-OPS")
          (:function "SDL_GetWindowID" "SDL-GET-WINDOW-ID")
          (:function "SDL_JoystickGetGUIDFromString" "SDL-JOYSTICK-GET-GUID-FROM-STRING")
          (:function "SDL_JoystickGetDeviceGUID" "SDL-JOYSTICK-GET-DEVICE-GUID")
          (:function "SDL_JoystickGetGUIDString" "SDL-JOYSTICK-GET-GUID-STRING")
          (:field "GUID" "GUID")
          (:function "TTF_SizeUNICODE" "TTF-SIZE-UNICODE")
          (:function "TTF_SizeUTF8" "TTF-SIZE-UTF8")
          (:function "SDL_GLattr" "SDL-GL-ATTR"))
        do (parachute:is equal expected (ffi-name-transformer input kind))))

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
