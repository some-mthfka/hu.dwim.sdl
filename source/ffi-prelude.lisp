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

(defun concat (&rest rest)
  (apply #'concatenate 'string rest))

(defmacro rx-lambda (&body body)
  `(lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignorable start end reg-starts reg-ends))
     (let ((str (subseq target-string match-start match-end)))
       (declare (ignorable str))
       ,@body)))

(defparameter *abbrevs*
  ;; order matters, e.g. RGBA needs to run before RGB, so just sort by length
  (sort (list "SDL" "SDL2" "IMG" "TTF" "GFX"
              "TLS" "CAS" "CVT"
              "UNICODE" "UTF8"
              "3D" "CPU" "MMX" "RAM" "AVX" "RLE" "RDTSC"
              "(L|B)E[0-9][0-9]" "SSE[0-9]*"
              "RGBA" "RGB" "YUV"
              "GUID" "ID" "GL" "WM" "RW" "XY" "FP"
              "WAV"
              "ICO" "CUR" "BMP" "GIF" "JPG" "LBM" "PCX" "PNG" "PNM" "TIF"
              "XPM" "XCF" "XV" "WEBP" "TGA")
        #'>
        :key #'length))

(defun frame-abbrev (target-string start end match-start match-end reg-starts reg-ends)
  (declare (ignore reg-starts reg-ends))
  (labels ((legit (pos) (<= start pos (1- end)))
           (thereis (rx pos)
             (when (legit pos) 
               (cl-ppcre:scan rx target-string :start pos :end (1+ pos))))
           (cap-p (pos) (thereis "[A-Z]" pos))
           (und-p (pos) (thereis "_" pos))
           (insert-p (pos) (and (legit pos)
                                (not (cap-p pos))
                                (not (und-p pos)))))
    (concat (when (insert-p (1- match-start)) "_")
            (string-downcase (subseq target-string match-start match-end))
            (when (insert-p match-end) "_"))))

(macrolet ((nreplace (rx-sym replacement)
             `(setf input (regex-replace-all ,rx-sym input ,replacement))))
  (defun caps-replace (input)
    (loop for abbrev in *abbrevs* do (nreplace abbrev #'frame-abbrev))
    (nreplace "_[A-Z][a-z]" (rx-lambda (string-downcase str)))))

(defun table-replace-p (name)
  (second (find name '(("SDL_Log" "SDL-LOG")
                       ("SDL_log" "SDL-LOG*")
                       ("SDL_TRUE" "TRUE")
                       ("SDL_FALSE" "FALSE"))
                :key #'first
                :test #'equal)))

(defparameter *questionable-names* nil
  "If you generate names for other sdl module, make sure to check this variable
after running, it makes it easy to catch off the abbreviations.  Make sure it's
always NIL, or add exceptions to `catch-questionable-names' if approprate.  Note
that there's no automatic reset mechanism, so make sure to reevaluate this
expression when generating again.")

(defun catch-questionable-names (name)
  (when (and (cl-ppcre:scan "-.-" name)
             (not (cl-ppcre:scan "--U-(QUAD|LONG|INT|SHORT|CHAR)(-T)?" name)))
    (push name *questionable-names*))
  name)

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
      (catch-questionable-names
       (cffi-sys:canonicalize-symbol-name-case
        (or
         (table-replace-p name)
         (_->-
          (case kind
            (:constant                  ; aka "_BITS_TYPES_H"
             (as-const name*))
            (:member ; member of an enum, aka "SDL_ASSERTION_ABORT", "SDL_FALSE"
             (as-const name*)) 
            (:variable                  ; none found
             (as-global name*))
            (:field  ; "__val", "BitsPerPixel", "Gshift", "num_texture_formats" 
             (as-field name*))
            (:argument                  ; "str", "X2", "blendMode", "Vplane" 
             (as-field name*))
            (:function                  ; "SDL_GetNumVideoDisplays"
             (as-function name*))
            (:struct                    ; "SDL_HAPTICCONDITION"
             (as-type name*))
            (:union                     ; "SDL_HAPTICEFFECT"
             (as-type name*))
            (:enum                      ; none found (all are anonymous)
             (as-type name*))
            (:type                      ; none passed to this function
             (as-type name*))
            (otherwise name)))))))))

(defmacro check (kind input expected)
  `(parachute:is equal ,expected (ffi-name-transformer ,input ,kind)))

(parachute:define-test+run various-transforms
  (check :function "SDL_InitSubSystem" "SDL-INIT-SUB-SYSTEM")
  (check :field "__u_char" "--U-CHAR")
  (check :constant "SDL_HAPTIC_INFINITY" "+SDL-HAPTIC-INFINITY+")
  (check :field "num_texture_formats" "NUM-TEXTURE-FORMATS")
  (check :field "__val" "--VAL")
  (check :field "BitsPerPixel" "BITS-PER-PIXEL")
  (check :argument "X2" "X2")
  (check :argument "blendMode" "BLEND-MODE")
  (check :argument "Vplane" "VPLANE"))

(parachute:define-test+run name-caps
  (check :type "SDL_GLprofile" "SDL-GL-PROFILE")
  (check :function "SDL_GL_UnbindGLTexture" "SDL-GL-UNBIND-GL-TEXTURE") ; FAKE, a made-up function
  (check :struct "SDL_RWops" "SDL-RW-OPS")
  (check :function "SDL_GetWindowID" "SDL-GET-WINDOW-ID")
  (check :function "SDL_JoystickGetGUIDFromString" "SDL-JOYSTICK-GET-GUID-FROM-STRING")
  (check :function "SDL_JoystickGetDeviceGUID" "SDL-JOYSTICK-GET-DEVICE-GUID")
  (check :function "SDL_JoystickGetGUIDString" "SDL-JOYSTICK-GET-GUID-STRING")
  (check :field "GUID" "GUID")
  (check :function "TTF_SizeUNICODE" "TTF-SIZE-UNICODE")
  (check :function "TTF_SizeUTF8" "TTF-SIZE-UTF8")
  (check :function "SDL_GLattr" "SDL-GL-ATTR")
  (check :function "rotozoomSurfaceXY" "ROTOZOOM-SURFACE-XY")
  (check :function "SDL_imageFilterMMXdetect" "SDL-IMAGE-FILTER-MMX-DETECT")
  (check :function "SDL_WriteBE64" "SDL-WRITE-BE64")
  (check :function "SDL_WriteLE16" "SDL-WRITE-LE16")
  (check :function "SDL_HasSSE41" "SDL-HAS-SSE41")
  (check :function "SDL_GetRGBA" "SDL-GET-RGBA"))

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
