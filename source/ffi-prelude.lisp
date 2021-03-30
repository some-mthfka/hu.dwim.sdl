;;; This file is loaded before the generated FFI.

;; TODO export all error conditions

(in-package :hu.dwim.sdl)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; * return type conversion

(define-condition sdl-error (error)
  ())

(eval-always
  (defun get+clear-sdl-error ()
    "Some functions, like `sdl-get-window-from-id', won't set sdl error message,
so you will see the previous one: so make sure to keep everything cleaned."
    (let ((err (hu.dwim.sdl/core::sdl-get-error)))
      (if (emptyp err)
          "/SDL_GetError contained no error message./"
          (prog1 (princ-to-string err)
            (hu.dwim.sdl/core::sdl-clear-error))))))

;; ** negative return code error

;; To test, try:
#+nil
(hu.dwim.sdl/core:sdl-get-display-mode 0 1 (cffi:null-pointer))
;; To get: SDL call failed: -1 "Video subsystem has not been initialized"

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
     (when (< return-code 0)
       (error 'sdl-error/negative-return-code
              :error-code return-code
              :format-control "SDL call failed: ~S ~S"
              :format-arguments (list return-code (get+clear-sdl-error))))
     return-code))

;; ** booleans (no error checking)

(cffi:define-foreign-type sdl-boolean (cffi::foreign-type-alias)
  ()
  (:default-initargs :actual-type (cffi::parse-type :boolean))
  (:simple-parser sdl-boolean))

(defmethod cffi:expand-from-foreign (value (type sdl-boolean))
  `(eql value hu.dwim.sdl/core::+true+))

;; ** booleans (where SDL_FALSE means an error)

(cffi:define-foreign-type sdl-boolean-checked-type (sdl-boolean) ())

(define-condition sdl-error/false-returned (simple-error sdl-error) ())

(defmethod cffi:expand-from-foreign (value (type sdl-boolean-checked-type))
  `(let ((result (eql value hu.dwim.sdl/core::+true+)))
     (unless result
       (error 'sdl-error/false-returned
              :format-control "SDL call failed: ~S ~S"
              :format-arguments (list (get+clear-sdl-error))))
     result))

;; ** null pointer returned

;; For an easy test of this, call:
#+nil
(hu.dwim.sdl/core:sdl-haptic-open-from-mouse)
#+or
(hu.dwim.sdl/core:sdl-get-window-from-id 43434)
;; to see: SDL call failed: "Haptic: Mouse isn't a haptic device."

;; (handler-case (hu.dwim.sdl/core:sdl-haptic-open-from-mouse)
;;   (error (c) (ut:repl c)))

;; OK, the approach like with the negative return code won't exactly work here,
;; as functions return different types of pointers.  So, let's define wrappers
;; for all those types (and make them inherit from one null-checked type).

(define-condition sdl-error/null-returned (simple-error sdl-error)
  ((type-info :initform (error "Must specify TYPE-INFO.")
              :accessor type-info-of
              :initarg :type-info)))

(cffi:define-foreign-type sdl-null-checked-type (cffi::foreign-type-alias)
  ()
  (:simple-parser sdl-null-checked-type))

(defmethod cffi:expand-from-foreign (value (type sdl-null-checked-type))
  `(let ((return-value (cffi:convert-from-foreign ,value :pointer))
         (type-returned ',(class-name (class-of type))))
     (when (cffi:null-pointer-p return-value)
       (error 'sdl-error/null-returned
              :type-info type-returned
              :format-control "SDL call failed: ~S.~%~%The function returns: ~a."
              :format-arguments (list (get+clear-sdl-error) type-returned)))
     return-value))

(eval-always
  (defun get-null-checked-type-name (type-specifier)
    (symbolicate 'sdl-null-checked-type/
                 (if (symbolp type-specifier)
                     type-specifier
                     (progn (assert (eql :pointer (first type-specifier)))
                            (assert (eql (length type-specifier) 2))
                            (symbolicate (second type-specifier) '*))))))

;; OK, I know this sucks, but trying to `eval'uate `define-foreign-type' form in
;; the `ffi-type-transformer' proved to be a real time waster with trying to
;; make symbols and definitions be in the right packages and I was getting
;; unknown CFFI type errors.  Easier to mainain this little list right here.
(dolist (type-specifier
         (append '(hu.dwim.sdl/core::sdl-glcontext)
                 (mapcar
                  (lambda (x) (list :pointer (ensure-symbol x 'hu.dwim.sdl/core)))
                  '(sdl-sem sdl-mutex sdl-surface sdl-palette sdl-pixelformat
                    sdl-audiospec sdl-rw-ops sdl-cond sdl-renderer sdl-haptic
                    sdl-joystick sdl-cursor sdl-glcontex sdl-surface sdl-window
                    sdl-thread sdl-displaymode sdl-glcontext sdl-gamecontroller
                    sdl-texture))))
  (let ((ft (get-null-checked-type-name type-specifier)))
    (eval `(cffi:define-foreign-type ,ft (sdl-null-checked-type)
             ()
             (:default-initargs :actual-type (cffi::parse-type ',type-specifier))
             (:simple-parser ,ft)))))

;; * Export

(defun ffi-name-export-predicate (symbol &key &allow-other-keys)
  (declare (ignore symbol))
  ;; cffi/c2ffi seems to be nothing like `kind' here...
  t)

;; * Name Conversion

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
                       ("SDL_TRUE" "+TRUE+")
                       ("SDL_FALSE" "+FALSE+"))
                :key #'first
                :test #'equal)))

(defparameter *questionable-names* nil
  "If you generate names for other sdl modules, make sure to check this variable
after running, it makes it easy to catch off the abbreviations.  Make sure it's
always NIL, or add exceptions to `catch-questionable-names' if approprate.  Note
that there's no automatic reset mechanism, so don't forget to reevaluate this
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

;; ** Name conversion tests

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
  (check :function "SDL_GL_UnbindGLTexture" "SDL-GL-UNBIND-GL-TEXTURE") ; a FAKE made-up function
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

;; ** type transformer

(defun ffi-type-transformer (type-specifier context &rest args &key &allow-other-keys)
  (let ((type-specifier (apply 'cffi/c2ffi:default-ffi-type-transformer
                               type-specifier context args))
        (name (when (consp context) (second context))))
    (flet ((convert-function-p (conversion-list &optional &key (check-type nil))
             (when (and name
                        (eql (first context) :function)
                        (eql (third context) :return-type)
                        (member name conversion-list :test 'equal))
               (if check-type (assert (eql type-specifier check-type)) t)
               t)))
      (cond
        ((convert-function-p *negative-returned-error-list/core* :check-type :int)
         'sdl-error-code)
        ((and (convert-function-p *null-returned-error-list/core*)
              (not (member type-specifier '(:string :void (:pointer :void)) :test #'equal)))
         (let ((*package* (find-package :hu.dwim.sdl)))
           (get-null-checked-type-name type-specifier)
           #+oooh-some-symbol-fiddling-bullshit-dont-even-bother
           (let ((ft-name (get-null-checked-type-name type-specifier (find-package :hu.dwim.sdl))))
             (eval
              `(let ((*package* (find-package (find-package :hu.dwim.sdl))))
                 (cffi:define-foreign-type ,ft-name (sdl-null-checked-type)
                   ()
                   (:default-initargs :actual-type (cffi::parse-type type-specifier))
                   (:simple-parser ,ft-name)))))))
        (t type-specifier)))))
