(in-package :hu.dwim.sdl)

;; This file is for listing return type conversions for functions.

;; ttf docs here:
;; http://sdl.beuc.net/sdl.wiki/SDL_ttf
;; gfx docs here:
;; https://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html

;; There's gotta be an easier way than just copying page results and running an
;; emacs macro on them.  Because this is like _super_ *lame*, dude.

;; * SDL Core (ONLY)

;; ** Commentary

;; [DK] I used SDL wiki search to identify various error return possibilities.
;; See https://wiki.libsdl.org/SGFunctions for some format details.
;; These aren't religiously followed by them, though.
;; And here's the search page: https://wiki.libsdl.org/FindPage
;; Below, I brace each query in slashes, don't put them in the actual search bar.

;; Query: /"Returns SDL_TRUE on success, SDL_FALSE on error."/
;; Date queried: 30 March 2021. 2 results.
;; SDL_Vulkan_GetInstanceExtensions
;; SDL_Vulkan_CreateSurface

;; *** Negative Return Code is error

;; These are the functions that return a negative number when an error occurs,
;; and you can call SDL_GetError to get info on it.  The return type for these is
;; converted to `sdl-error-code', which is a cffi type that automatically signals
;; an error if the return code is negative. See `ffi-type-transformer'.

;; TODO this list is by far not complete. see this SDL bug for details:
;; https://bugzilla.libsdl.org/show_bug.cgi?id=3219

(defparameter *negative-return-code-conversion-list/core*
  '(;; [DK] One could say that the list below is probably still incomplete, but
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
    "SDL_AudioInit"))

;; *** SDL_FALSE is error

;; These are the functions that return NULL on error, and you can call
;; SDL_GetError to get info on it.  The return type for these is converted to
;; `sdl-null-checked-type', which is a cffi type that automatically signals an
;; error if the return value is NULL. See `ffi-type-transformer'.

(defparameter *null-on-failure-conversion-list/core*
  '(;; Query: /regex:NULL.*SDL_GetError/
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
    "SDL_AllocFormat"))

(defparameter *all-conversion-lists*
  (list *negative-return-code-conversion-list/core*
        *null-on-failure-conversion-list/core*))

(loop for x in *all-conversion-lists*
      do (loop for y in *all-conversion-lists*
               when (not (eq x y))
                 do (assert (null (intersection x y :test #'equal)))))

;; total function count
(reduce #'+ (mapcar #'length *all-conversion-lists*))
