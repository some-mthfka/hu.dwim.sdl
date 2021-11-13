(in-package :hu.dwim.sdl)

(defparameter *auto-passed-return-value+suppress-return/all*
  '(("SDL_GetWindowSize" * - -)
    ("TTF_SizeText" * * - -)
    ("TTF_SizeUnicode" * * - -)
    ("TTF_SizeUTF8" * * - -))
  "Exercise caution. Only suitable for values that are converted to lisp
values (such as ints), and don't need further deallocation by the user. If it's
something that would require deallocation by the user, like `enclose-points',
which fills an array of points as its result, it's better that the user employs
a suitable with macro and does the passing himself.")

(defparameter *auto-passed-return-value+pass-return/all*
  '(("SDL_GetMouseState" - -)
    ("SDL_GetGlobalMouseState" - -)
    ("SDL_GetRelativeMouseState" - -))
  "Same purpose as `*auto-passed-return-value+suppress-return/all*', but the
original return value is returned as well.")

;; Remove Create. In case of a name not starting with Create, add it to README.
(defparameter *with-function-list* 
  '(("SDL_CreateWindowFrom" "SDL_DestroyWindow" window-from)
    ("SDL_CreateWindow" "SDL_DestroyWindow" window)
    ("SDL_CreateShapedWindow" "SDL_DestroyWindow" shaped-window)
    ("SDL_CreateTextureFromSurface" "SDL_DestroyTexture" texture-from-surface)
    ("SDL_CreateTexture" "SDL_DestroyTexture" texture)
    ("SDL_CreateSoftwareRenderer" "SDL_DestroyRenderer" software-renderer)
    ("SDL_CreateRenderer" "SDL_DestroyRenderer" renderer)
    ("SDL_CreateSemaphore" "SDL_DestroySemaphore" semaphore)
    ("SDL_CreateRGBSurfaceWithFormatFrom" "SDL_FreeSurface" rgb-surface-with-format-from)
    ("SDL_CreateRGBSurfaceWithFormat" "SDL_FreeSurface" rgb-surface-with-format)
    ("SDL_CreateRGBSurfaceFrom" "SDL_FreeSurface" rgb-surface-from)
    ("SDL_CreateRGBSurface" "SDL_FreeSurface" rgb-surface)
    ("SDL_CreateMutex" "SDL_DestroyMutex" mutex)
    ("SDL_CreateCursor" "SDL_FreeCursor" cursor)
    ("SDL_CreateColorCursor" "SDL_FreeCursor" color-cursor)
    ("SDL_CreateSystemCursor" "SDL_FreeCursor" system-cursor)
    ("SDL_CreateCond" "SDL_DestroyCond" cond)
    ;; not starting with Create
    ("SDL_GL_CreateContext" "SDL_GL_DeleteContext" gl-context)
    ("SDL_Metal_CreateView" "SDL_Metal_DestroyView" metal-view)
    ;; value-passed:
    ;; ("SDL_Vulkan_CreateSurface" "")
    ;; ("SDL_CreateWindowAndRenderer" "")
    ;; nothing for these:
    ;; ("SDL_CreateThread" "")
    ;; ("SDL_CreateThreadWithStackSize" "")
    ;; ("SDL_TLSCreate" "")
    ))
