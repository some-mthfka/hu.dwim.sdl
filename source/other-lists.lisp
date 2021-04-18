(in-package :hu.dwim.sdl)

(defparameter *auto-passed-return-value+suppress-return/all*
  '(("SDL_GetWindowSize" * - -)
    ("TTF_SizeText" * * - -)
    ("TTF_SizeUnicode" * * - -)
    ("TTF_SizeUTF8" * * - -)))

(defparameter *auto-passed-return-value+pass-return/all* nil)

(defparameter *with-function-list*
  '(("SDL_GL_CreateContext" "SDL_GL_DeleteContext")
    ("SDL_CreateWindowFrom" "SDL_DestroyWindow")
    ("SDL_CreateWindow" "SDL_DestroyWindow")
    ("SDL_CreateShapedWindow" "SDL_DestroyWindow")
    ("SDL_CreateTextureFromSurface" "SDL_DestroyTexture")
    ("SDL_CreateTexture" "SDL_DestroyTexture")
    ("SDL_CreateSoftwareRenderer" "SDL_DestroyRenderer")
    ("SDL_CreateRenderer" "SDL_DestroyRenderer")
    ("SDL_CreateSemaphore" "SDL_DestroySemaphore")
    ("SDL_CreateRGBSurfaceWithFormatFrom" "SDL_FreeSurface")
    ("SDL_CreateRGBSurfaceWithFormat" "SDL_FreeSurface")
    ("SDL_CreateRGBSurfaceFrom" "SDL_FreeSurface")
    ("SDL_CreateRGBSurface" "SDL_FreeSurface")
    ("SDL_CreateMutex" "SDL_DestroyMutex")
    ("SDL_CreateCursor" "SDL_FreeCursor")
    ("SDL_CreateColorCursor" "SDL_FreeCursor")
    ("SDL_CreateSystemCursor" "SDL_FreeCursor")
    ("SDL_CreateCond" "SDL_DestroyCond")
    ("SDL_Metal_CreateView" "SDL_Metal_DestroyView")
    ;; value-passed:
    ;; ("SDL_Vulkan_CreateSurface" "")
    ;; ("SDL_CreateWindowAndRenderer" "")
    ;; nothing for these:
    ;; ("SDL_CreateThread" "")
    ;; ("SDL_CreateThreadWithStackSize" "")
    ;; ("SDL_TLSCreate" "")
    ))
