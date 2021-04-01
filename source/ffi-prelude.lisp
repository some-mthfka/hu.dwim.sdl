;;; This file is loaded before the generated FFI.

;; TODO export all error conditions

(in-package :hu.dwim.sdl)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; * custom return types

;; ** commentary

;; Say we want to automatically check the returned value of "SDL_CreateWindow"
;; for NULL and throw a condition.  That's what defining custom return types is
;; gonna do for us, and that we do with `cffi:define-foreign-type'.  As such, it
;; requires us to specify the actual type, which, in the case of window
;; creation, is "SDL_Window *".  Thing is, one would probably want not to worry
;; about such details and instead generate things on the fly, but I found it
;; much simpler just to do the bookkeeping yourself, so don't be surprised when
;; you see lists of types below.  It's not that bad, really, and should require
;; maintenance only when new return types are defined and you have a function
;; that returns it in the conversion lists.  If you add a function with an
;; unknown return type to the conversion lists, but don't add the type in the
;; right place below, you will simply get an error.  So, that's easy enough.

;; Also: the symbols for foreign types which we define belong to this package,
;; not to the ffi packages.  You could go that way too, but that requires more
;; structure and bookkeeping for no apparent benefit.

;; CAUTION: if you want to try your hand at automating this anyway, don't try to
;; define the foreign types in `ffi-type-transformer', it's not gonna work for
;; two reasons: one, `cffi::*type-parsers*' is dynamically bound to a copy of
;; itself, and two, it would only work if the generation phase ran every time on
;; load, which is not the case.  One would have to go further: collect the
;; necessary types there, save them to a file, and then load it in some
;; `perform' :after method.  What a bother.

;; ** utils

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

#+nil
(hu.dwim.sdl/core:sdl-has-3d-now)
#+nil
(hu.dwim.sdl/core:sdl-has-sse)

(cffi:define-foreign-type sdl-boolean (cffi::foreign-type-alias)
  ()
  (:default-initargs :actual-type (cffi::parse-type :boolean))
  (:simple-parser sdl-boolean))

(defmethod cffi:expand-from-foreign (value (type sdl-boolean))
  `(eql ,value hu.dwim.sdl/core::+true+))

;; ** booleans (where SDL_FALSE means an error)

#+nil
(hu.dwim.sdl/core:sdl-remove-timer 0)

(cffi:define-foreign-type sdl-boolean-checked-type (sdl-boolean)
  ()
  (:default-initargs :actual-type (cffi::parse-type :boolean))
  (:simple-parser sdl-boolean-checked-type))

(define-condition sdl-error/false-returned (simple-error sdl-error) ())

(defmethod cffi:expand-from-foreign (value (type sdl-boolean-checked-type))
  `(let ((result (eql ,value hu.dwim.sdl/core::+true+)))
     (unless result
       (error 'sdl-error/false-returned
              :format-control "SDL call failed: ~S"
              :format-arguments (list (get+clear-sdl-error))))
     result))

;; ** enum values that signal errors

;; (defparameter *return-enum-check-invalid/core*
;;   '(("SDL_SensorGetType" "SDL_SENSOR_INVALID")
;;     ("SDL_SensorGetDeviceType" "SDL_SENSOR_INVALID")
;;     ("SDL_GetPixelFormatName" "SDL_PIXELFORMAT_UNKNOWN")
;;     ("SDL_GetWindowPixelFormat" "SDL_PIXELFORMAT_UNKNOWN")
;;     ("SDL_MasksToPixelFormatEnum" "SDL_PIXELFORMAT_UNKNOWN")
;;     ("SDL_GetScancodeFromName" "SDL_SCANCODE_UNKNOWN")
;;     ("SDL_GetKeyFromName" "SDLK_UNKNOWN")
;;     ("SDL_JoystickCurrentPowerLevel" "SDL_JOYSTICK_POWER_UNKNOWN") ; enum
;;     ("SDL_GameControllerGetBindForButton" "SDL_CONTROLLER_BINDTYPE_NONE")
;;     ("SDL_GameControllerGetBindForAxis" "SDL_CONTROLLER_BINDTYPE_NONE")
;;     ("SDL_GameControllerGetButtonFromString" "SDL_CONTROLLER_AXIS_INVALID")
;;     ("SDL_GameControllerGetAxisFromString" "SDL_CONTROLLER_AXIS_INVALID")))

;; (cffi:define-foreign-type sdl-enum-value-checked-type (simple-error sdl-error)
;;   ()
;;   (:default-initargs :actual-type (cffi::parse-type :boolean))
;;   (:simple-parser sdl-boolean-checked-type))

;; (eval-always
;;   (defun get-enum-value-checked-type-name (type-specifier)
;;     (symbolicate 'sdl-enum-value-checked-type/ type-specifier)))

;; (dolist (type-specifier
;;          (mapcar (lambda (x) (ensure-symbol x 'hu.dwim.sdl/core))
;;                  (remove-duplicates (mapcar #'second *return-enum-check-invalid/core*)
;;                                     :test #'equal)))
;;   (let ((ft (get-enum-value-checked-type-name type-specifier)))
;;     (eval `(cffi:define-foreign-type ,ft (sdl-enum-value-checked-checked-type)
;;              ()
;;              (:default-initargs :actual-type (cffi::parse-type ',type-specifier))
;;              (:simple-parser ,ft)))))

;; ** null pointer returned is error

;; For an easy test of this, call:
#+nil
(hu.dwim.sdl/core:sdl-haptic-open-from-mouse)
#+or
(hu.dwim.sdl/core:sdl-get-window-from-id 43434)
;; to see: SDL call failed: "Haptic: Mouse isn't a haptic device."

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

;; (ql:quickload 'ut)

;; Note: trying to define foreign type for some reason requires a let wrapping
;; where you bind the target *package*, but here we are ok with the current
;; package, so we don't need it.

(dolist (type-specifier
         (append '(hu.dwim.sdl/core::sdl-gl-context) ; this is a pointer type
                 '(:string (:pointer :void))
                 (mapcar ; pointer generation
                  (lambda (x) (list :pointer (ensure-symbol x 'hu.dwim.sdl/core)))
                  '(sdl-sem sdl-mutex sdl-surface sdl-palette sdl-pixel-format
                    sdl-audio-spec sdl-rw-ops sdl-cond sdl-renderer sdl-haptic
                    sdl-joystick sdl-cursor sdl-surface sdl-window
                    sdl-thread sdl-display-mode sdl-game-controller
                    sdl-texture sdl-finger))))
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
always NIL, or add exceptions to `catch-questionable-names' if approprate.")

(defun member* (x list) (member x list :test #'equal))

(defun catch-questionable-names (name)
  (when (and (cl-ppcre:scan "-.-" name)
             (not (cl-ppcre:scan "--U-(QUAD|LONG|INT|SHORT|CHAR)(-T)?" name))
             (not (member* name *questionable-names*))) ; easier than adding hooks to perform
    (push name *questionable-names*))
  name)

(defparameter *unknown-names* nil)

(defun catch-unknown-names (name)
  (unless (some (curry #'member* name) *all-conversion-lists*)
    ;; (member* name *unknown-names*)
    (push name *unknown-names*)))

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
             (as-type (name) (from-camel (caps-replace name)))
             (as-enum (name) name)
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
             (as-enum name*))
            (:union                     ; "SDL_HAPTICEFFECT"
             (as-enum name*))
            (:enum                      ; none found (all are anonymous)
             (as-enum name*))
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

(defparameter *string-thing* nil)
(defparameter *bool-thing* nil)

(defun ffi-type-transformer (type-specifier context &rest args &key &allow-other-keys)
  (let ((type-specifier (apply 'cffi/c2ffi:default-ffi-type-transformer
                               type-specifier context args))
        (name (when (consp context) (second context))))
    (when (and (eql (first context) :function)
               (eql (third context) :return-type))
      (catch-unknown-names name)
      (when (eql type-specifier 'sdl-bool)
        (push name *bool-thing*)))
    (flet ((convert-p (conversion-list &optional &key (check-type nil))
             (when (and name
                        (eql (first context) :function)
                        (eql (third context) :return-type)
                        (member name conversion-list :test 'equal))
               (if check-type (assert (member (make-keyword type-specifier) check-type)))
               t)))
      (cond
        ((convert-p *negative-returned-error-list/core*
                    :check-type '(:int :sdl-audio-device-id :sdl-joystick-id))
         'sdl-error-code)
        ((convert-p *return-boolean-no-errors/all*)
         'sdl-boolean)
        ((convert-p *return-boolean-check-errors/all*)
         'sdl-boolean-checked-type)
        ((convert-p *return-null-on-failure/all*)
         (get-null-checked-type-name type-specifier))
        (t type-specifier)))))
