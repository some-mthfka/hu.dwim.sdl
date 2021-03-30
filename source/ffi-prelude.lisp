;;; This file is loaded before the generated FFI.

;; TODO export all error conditions

(in-package :hu.dwim.sdl)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; * sdl errors

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
                :format-arguments (list return-code (get+clear-sdl-error)))
         return-code)))

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
  (ut:repl (cffi::actual-type type))
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
                       ("SDL_TRUE" "TRUE")
                       ("SDL_FALSE" "FALSE"))
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

;; * Type conversion

;; ** Commentary

;; [DK] I used SDL wiki search to identify various error return possibilities.
;; See https://wiki.libsdl.org/SGFunctions for some format details.
;; These aren't religiously followed by them, though.
;; And here's the search page: https://wiki.libsdl.org/FindPage
;; Below, I brace each query in slashes, don't put them in the actual search bar.

;; There's gotta be an easier way than just copying page results and running an
;; emacs macro on them.  Because this is like _super_ *lame*, dude.

;; ttf docs here:
;; http://sdl.beuc.net/sdl.wiki/SDL_ttf
;; gfx docs here:
;; https://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html

;; Query: /"Returns SDL_TRUE on success, SDL_FALSE on error."/
;; Date queried: 30 March 2021. 2 results.
;; SDL_Vulkan_GetInstanceExtensions
;; SDL_Vulkan_CreateSurface

;; ** Lists

;; *** Negative Return Code

(defparameter *negative-return-code-conversion-list*
  ;; TODO this list is by far not complete. see this SDL bug for details:
  ;; https://bugzilla.libsdl.org/show_bug.cgi?id=3219
  ;; See the docstring for info.
  '("TTF_Init"
    "IMG_Init"
    ;; SDL Core
    ;; [DK] One could say that the list below is probably still incomplete, but
    ;; I guess someone will have to find out the hard way.
    ;; Query: /regex:"Returns 0 on success".*SDL_GetError/
    ;; Date queried: 30 March 2021. 121 results.
    ;; Exceptions:
    "SDL_GL_UnbindTexture"              ; wiki forgot to mention SDL_GetError
    ;; Results:
    "SDL_CreateWindowAndRenderer"
    "SDL_VideoInit"
    "SDL_GL_SetAttribute"
    "SDL_SaveBMP"
    "SDL_RWclose"
    "SDL_Init"
    "SDL_GetDisplayMode"
    "SDL_GetDisplayBounds"
    "SDL_GetDesktopDisplayMode"
    "SDL_GetCurrentDisplayMode"
    "SDL_iPhoneSetAnimationCallback"
    "SDL_WinRTRunApp"
    "SDL_WarpMouseGlobal"
    "SDL_Vulkan_LoadLibrary"
    "SDL_UpdateYUVTexture"
    "SDL_UpdateWindowSurfaceRects"
    "SDL_UpdateWindowSurface"
    "SDL_UpdateTexture"
    "SDL_UnlockMutex"
    "SDL_TLSSet"
    "SDL_ShowSimpleMessageBox"
    "SDL_ShowMessageBox"
    "SDL_SetWindowOpacity"
    "SDL_SetWindowModalFor"
    "SDL_SetWindowInputFocus"
    "SDL_SetWindowHitTest"
    "SDL_SetWindowGammaRamp"
    "SDL_SetWindowFullscreen"
    "SDL_SetWindowDisplayMode"
    "SDL_SetWindowBrightness"
    "SDL_SetThreadPriority"
    "SDL_SetTextureColorMod"
    "SDL_SetTextureBlendMode"
    "SDL_SetTextureAlphaMod"
    "SDL_SetSurfaceRLE"
    "SDL_SetSurfacePalette"
    "SDL_SetSurfaceColorMod"
    "SDL_SetSurfaceBlendMode"
    "SDL_SetSurfaceAlphaMod"
    "SDL_SetRenderTarget"
    "SDL_SetRenderDrawColor"
    "SDL_SetRenderDrawBlendMode"
    "SDL_SetRelativeMouseMode"
    "SDL_SetPixelFormatPalette"
    "SDL_SetPaletteColors"
    "SDL_SetColorKey"
    "SDL_SetClipboardText"
    "SDL_SemWait"
    "SDL_SemPost"
    "SDL_SaveBMP_RW"
    "SDL_RenderSetViewport"
    "SDL_RenderSetScale"
    "SDL_RenderSetLogicalSize"
    "SDL_RenderSetIntegerScale"
    "SDL_RenderSetClipRect"
    "SDL_RenderReadPixels"
    "SDL_RenderFillRects"
    "SDL_RenderFillRect"
    "SDL_RenderDrawRects"
    "SDL_RenderDrawRect"
    "SDL_RenderDrawPoints"
    "SDL_RenderDrawPoint"
    "SDL_RenderDrawLines"
    "SDL_RenderDrawLine"
    "SDL_RenderCopyEx"
    "SDL_RenderCopy"
    "SDL_RenderClear"
    "SDL_RedetectInputDevices"
    "SDL_QueueAudio"
    "SDL_QueryTexture"
    "SDL_OpenURL"
    "SDL_LowerBlitScaled"
    "SDL_LowerBlit"
    "SDL_LockTexture"
    "SDL_LockSurface"
    "SDL_LockMutex"
    "SDL_JoystickGetBall"
    "SDL_InitSubSystem"
    "SDL_HapticUpdateEffect"
    "SDL_HapticUnpause"
    "SDL_HapticStopEffect"
    "SDL_HapticStopAll"
    "SDL_HapticSetGain"
    "SDL_HapticSetAutocenter"
    "SDL_HapticRunEffect"
    "SDL_HapticRumbleStop"
    "SDL_HapticRumblePlay"
    "SDL_HapticRumbleInit"
    "SDL_HapticPause"
    "SDL_GetWindowOpacity"
    "SDL_GetWindowGammaRamp"
    "SDL_GetWindowDisplayMode"
    "SDL_GetWindowBordersSize"
    "SDL_GetTextureColorMod"
    "SDL_GetTextureBlendMode"
    "SDL_GetTextureAlphaMod"
    "SDL_GetSurfaceColorMod"
    "SDL_GetSurfaceBlendMode"
    "SDL_GetSurfaceAlphaMod"
    "SDL_GetRendererOutputSize"
    "SDL_GetRendererInfo"
    "SDL_GetRenderDriverInfo"
    "SDL_GetRenderDrawColor"
    "SDL_GetRenderDrawBlendMode"
    "SDL_GetNumInputDevices"
    "SDL_GetDisplayUsableBounds"
    "SDL_GetDisplayDPI"
    "SDL_GetColorKey"
    "SDL_GL_SetSwapInterval"
    "SDL_GL_MakeCurrent"
    "SDL_GL_LoadLibrary"
    "SDL_GL_GetAttribute"
    "SDL_GL_BindTexture"
    "SDL_FillRects"
    "SDL_FillRect"
    "SDL_ConvertPixels"
    "SDL_CondSignal"
    "SDL_CondBroadcast"
    "SDL_CaptureMouse"
    "SDL_BlitScaled"
    "SDL_AudioInit")
  "These are the functions that return a negative number when an error occurs,
and you can call SDL_GetError to get info on it.  The return type for these is
converted to `sdl-error-code', which is a cffi type that automatically signals
an error if the return code is negative. See `ffi-type-transformer'.")

;; *** 

(defparameter *null-on-failure-conversion-list*
  '(;; SDL Core
    ;; Query: /regex:NULL.*SDL_GetError/
    ;; Date queried: 30 March 2021. 59 results.
    "SDL_WinRTGetFSPathUTF8"
    "SDL_WinRTGetFSPathUNICODE"
    "SDL_TLSGet"
    "SDL_RenderGetD3D9Device"
    "SDL_RWFromMem"
    "SDL_RWFromFile"
    "SDL_RWFromFP"
    "SDL_RWFromConstMem"
    "SDL_LoadWAV_RW"
    "SDL_LoadObject"
    "SDL_LoadFunction"
    "SDL_LoadBMP_RW"
    "SDL_JoystickOpen"
    "SDL_JoystickNameForIndex"
    "SDL_JoystickName"
    "SDL_JoystickFromInstanceID"
    "SDL_HapticOpenFromMouse"
    "SDL_HapticOpenFromJoystick"
    "SDL_HapticOpen"
    "SDL_HapticName"
    "SDL_GetWindowSurface"
    "SDL_GetWindowFromID"
    "SDL_GetRenderer"
    "SDL_GetDisplayName"
    "SDL_GetClosestDisplayMode"
    "SDL_GetClipboardText"
    "SDL_GetBasePath"
    "SDL_GameControllerOpen"
    "SDL_GameControllerMappingForGUID"
    "SDL_GameControllerMapping"
    "SDL_GameControllerFromInstanceID"
    "SDL_GL_GetCurrentWindow"
    "SDL_GL_GetCurrentContext"
    "SDL_GL_CreateContext"
    "SDL_DestroyWindow"
    "SDL_CreateWindowFrom"
    "SDL_CreateWindow"
    "SDL_CreateThread"
    "SDL_CreateTextureFromSurface"
    "SDL_CreateTexture"
    "SDL_CreateSystemCursor"
    "SDL_CreateSoftwareRenderer"
    "SDL_CreateSemaphore"
    "SDL_CreateRenderer"
    "SDL_CreateRGBSurfaceWithFormatFrom"
    "SDL_CreateRGBSurfaceWithFormat"
    "SDL_CreateRGBSurfaceFrom"
    "SDL_CreateRGBSurface"
    "SDL_CreateMutex"
    "SDL_CreateCursor"
    "SDL_CreateCond"
    "SDL_CreateColorCursor"
    "SDL_ConvertSurfaceFormat"
    "SDL_ConvertSurface"
    "SDL_AndroidGetInternalStoragePath"
    "SDL_AndroidGetExternalStoragePath"
    "SDL_AllocRW"
    "SDL_AllocPalette"
    "SDL_AllocFormat")
  "These are the functions that return NULL on error, and you can call
SDL_GetError to get info on it.  The return type for these is converted to
`sdl-null-checked-type', which is a cffi type that automatically signals an error if the
return value is NULL. See `ffi-type-transformer'.")

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
        ((convert-function-p *negative-return-code-conversion-list* :check-type :int)
         'sdl-error-code)
        ((and (convert-function-p *null-on-failure-conversion-list*)
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
