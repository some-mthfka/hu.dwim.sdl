(in-package :hu.dwim.sdl)

;; * Name Conversion

(defun concat (&rest rest)
  (apply #'concatenate 'string rest))

(defmacro rx-lambda (&body body)
  `(lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignorable start end reg-starts reg-ends))
     (let ((str (subseq target-string match-start match-end)))
       (declare (ignorable str))
       ,@body)))

(defun without-prefix (name)
  (regex-replace "^(SDL|IMG|TTF|GFX)_" name ""))

(defparameter *abbrevs*
  ;; order matters, e.g. RGBA needs to run before RGB, so just sort by length
  (sort (list "SDL" "SDL2" "IMG" "TTF" "GFX"
              "TLS" "CAS" "CVT"
              "UNICODE" "UTF8"
              "3D" "CPU" "MMX" "RAM" "AVX" "AVX2" "AVX512F" "RLE" "RDTSC"
              "3DNow" "ARMSIMD" "AltiVec" "NEON"
              "(L|B)E[0-9][0-9]" "SSE[0-9]*"
              "RGBA" "RGB" "YUV"
              "GUID" "ID" "GL" "WM" "RW" "XY" "FP"
              "WAV"
              "DUMMY" "ANON" "UNION" "STRUCT" "ENUM"
              "OS" "FILE"
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
  (second (find name '(("SDL_Log" "LOG")
                       ("SDL_log" "LOG*"))
                :key #'first
                :test #'equal)))

(defun member* (x list) (member x list :test #'equal))

(defparameter *questionable-names* nil
  "If you generate names for other sdl modules, make sure to check this variable
after running, it makes it easy to catch off the abbreviations.  Best kept
always NIL, or add exceptions to `catch-questionable-names' if approprate.")

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
  (let ((name* (caps-replace (without-prefix (substitute #\_ #\- name)))))
    (labels ((_->- (name) (substitute #\- #\_ name))
             (from-camel (name) (cffi/c2ffi:camelcase-to-dash-separated name))
             (muff (name char) (concatenate 'string char name char))
             (downcase (name) (string-downcase name))
             (as-const (name) (muff (downcase name) "+"))
             (as-global (name) (muff (downcase name) "*"))
             (as-field (name) (from-camel name))
             (as-type (name) (from-camel (caps-replace name)))
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
            (:variable                  ; none
             (as-global name*))
            (:field  ; "__val", "BitsPerPixel", "Gshift", "num_texture_formats" 
             (as-field name*))
            (:argument                  ; "str", "X2", "blendMode", "Vplane" 
             (as-field name*))
            (:function                  ; "SDL_GetNumVideoDisplays"
             (as-function name*))
            (:struct                    ; "SDL_Surface", "ANON-STRUCT-1"
             (as-type name*))
            (:union                     ; "SDL_HapticEffect"
             (as-type name*))
            (:enum                      ; "ANON-ENUM-52"
             (as-type name*))
            (:type                      ; "Uint32"
             (as-type name*))
            (otherwise name)))))))))

;; ** Tests

(defmacro check (kind input expected)
  `(parachute:is equal ,expected (ffi-name-transformer ,input ,kind)))

(parachute:define-test+run various-transforms
  (check :function "SDL_InitSubSystem" "INIT-SUB-SYSTEM")
  (check :field "__u_char" "--U-CHAR")
  (check :constant "SDL_HAPTIC_INFINITY" "+HAPTIC-INFINITY+")
  (check :field "num_texture_formats" "NUM-TEXTURE-FORMATS")
  (check :field "__val" "--VAL")
  (check :field "BitsPerPixel" "BITS-PER-PIXEL")
  (check :argument "X2" "X2")
  (check :argument "blendMode" "BLEND-MODE")
  (check :argument "Vplane" "VPLANE"))

(parachute:define-test+run name-caps
  (check :type "SDL_GLprofile" "GL-PROFILE")
  (check :function "SDL_GL_UnbindGLTexture" "GL-UNBIND-GL-TEXTURE") ; a FAKE made-up function
  (check :struct "SDL_RWops" "RW-OPS")
  (check :function "SDL_GetWindowID" "GET-WINDOW-ID")
  (check :function "SDL_JoystickGetGUIDFromString" "JOYSTICK-GET-GUID-FROM-STRING")
  (check :function "SDL_JoystickGetDeviceGUID" "JOYSTICK-GET-DEVICE-GUID")
  (check :function "SDL_JoystickGetGUIDString" "JOYSTICK-GET-GUID-STRING")
  (check :field "GUID" "GUID")
  (check :function "TTF_SizeUNICODE" "SIZE-UNICODE")
  (check :function "TTF_SizeUTF8" "SIZE-UTF8")
  (check :function "SDL_GLattr" "GL-ATTR")
  (check :function "rotozoomSurfaceXY" "ROTOZOOM-SURFACE-XY")
  (check :function "SDL_imageFilterMMXdetect" "IMAGE-FILTER-MMX-DETECT")
  (check :function "SDL_WriteBE64" "WRITE-BE64")
  (check :function "SDL_WriteLE16" "WRITE-LE16")
  (check :function "SDL_HasSSE41" "HAS-SSE41")
  (check :function "SDL_GetRGBA" "GET-RGBA")
  (check :struct "ANON-STRUCT-1" "ANON-STRUCT-1")
  (check :enum "ANON-ENUM-10" "ANON-ENUM-10"))
