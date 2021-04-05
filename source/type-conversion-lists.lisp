(in-package :hu.dwim.sdl)

;; This file is for listing return type conversions for functions.

;; gfx docs here:
;; https://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html

;; There's gotta be an easier way than just copying page results and running an
;; emacs macro on them.  Because this is like _super_ *lame*, dude.

(defun diff (a b) (set-difference a b :test #'equal))

(defun check-repeats-helper (list)
  (assert (not (member (car list) (cdr list) :test #'equal))
          nil "Repeats: ~a" (car list))
  (when list (check-repeats (cdr list))))

(defun check-repeats (list)
  (check-repeats-helper list)
  list)

(defmacro defl (name &rest rest)
  `(defparameter ,name (check-repeats (list ,@rest))))

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

;; ** core / negative return code is error

(defl *return-negative-on-failure/core*
  ;; Query: /regex:"Returns 0 on success".*SDL_GetError/
  ;; Date queried: 30 March 2021. 121 results.
  "SDL_CreateWindowAndRenderer"
  "SDL_VideoInit"
  "SDL_JoystickIsHaptic"                ; boolean, negative on error TODO
  "SDL_HapticRumbleSupported"           ; boolean, negative on error
  "SDL_HapticEffectSupported"           ; boolean, negative on error
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
  "SDL_RenderSetIntegerScale" ; SDL_FALSE if not and on failure;
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
  "SDL_GL_UnbindTexture"                ; wiki forgot to mention SDL_GetError
  "SDL_FillRects"
  "SDL_FillRect"
  "SDL_ConvertPixels"
  "SDL_CondBroadcast"
  "SDL_CaptureMouse"
  "SDL_BlitScaled"
  "SDL_AudioInit"
  "SDL_CondWait"
  "SDL_CondWaitTimeout"
  "SDL_CondSignal"
  ;; Google query: "SDL_GetError" "negative" OR "-1" site:wiki.libsdl.org
  ;; minus the stuff above and some structs
  "SDL_NumJoysticks"
  "SDL_GetWindowDisplayIndex"
  "SDL_JoystickNumHats"
  "SDL_GetNumVideoDrivers"
  "SDL_BuildAudioCVT"
  "SDL_GameControllerAddMappingsFromRW"
  "SDL_ConvertAudio"
  "SDL_JoystickInstanceID"
  "SDL_HapticNumAxes"
  "SDL_HapticGetEffectStatus"
  "SDL_HapticNumEffects"
  "SDL_PushEvent"
  "SDL_GameControllerAddMapping"
  "SDL_GameControllerAddMappingsFromFile"
  "SDL_RWsize"
  "SDL_HapticNumEffectsPlaying"
  "SDL_JoystickNumButtons"
  "SDL_GetNumRenderDrivers"
  "SDL_ShowCursor"
  "SDL_TryLockMutex"
  "SDL_JoystickNumAxes"
  "SDL_JoystickEventState"
  "SDL_GetNumVideoDisplays"
  "SDL_GetNumDisplayModes"
  "SDL_OpenAudio"
  "SDL_OpenAudioDevice"
  ;; checked the following manually on wiki:
  "SDL_RWtell"
  "SDL_RWseek"
  "SDL_SensorGetNonPortableType"
  "SDL_SensorGetInstanceID"
  "SDL_SensorGetDeviceNonPortableType"
  "SDL_SensorGetDeviceInstanceID"
  "SDL_SensorGetData"
  "SDL_SemWaitTimeout"
  "SDL_SemTryWait"
  "SDL_RenderFillRectsF"
  "SDL_RenderFillRectF"
  "SDL_RenderDrawRectsF"
  "SDL_RenderDrawRectF"
  "SDL_RenderDrawPointsF"
  "SDL_RenderDrawPointF"
  "SDL_RenderDrawLinesF"
  "SDL_RenderDrawLineF"
  "SDL_RenderCopyF"
  "SDL_RenderCopyExF"
  "SDL_PeepEvents"
  "SDL_NumSensors"       ; wiki says nothing, but this can't be negative, right?
  "SDL_NumHaptics"
  "SDL_JoystickNumBalls"
  "SDL_GetNumTouchDevices"
  "SDL_GetNumAudioDrivers"
  "SDL_HapticNewEffect"
  "SDL_HapticIndex"
  "SDL_Direct3D9GetAdapterIndex"
  "SDL_LoadDollarTemplates" ; could be 0, check yourself: negative error code (or 0) on failure
  "SDL_BlitSurface"
  "SDL_UpperBlitScaled"
  "SDL_UpperBlit"
  "SDL_SoftStretch")

;; ** core / null is error

;; These are the functions that return NULL on error, and you can call
;; SDL_GetError to get info on it.  The return type for these is converted to
;; `sdl-null-checked-type', which is a cffi type that automatically signals an
;; error if the return value is NULL. See `ffi-type-transformer'.

(defl *return-null-on-failure/core*
  ;; Query (on old SDL wiki): /regex:NULL.*SDL_GetError/
  "SDL_WinRTGetFSPathUTF8"
  "SDL_WinRTGetFSPathUNICODE"
  "SDL_TLSGet"
  "SDL_RenderGetD3D9Device"
  "SDL_RWFromMem"
  "SDL_RWFromFile"
  "SDL_RWFromFP"
  "SDL_RWFromConstMem"
  "SDL_LoadWAV"
  "SDL_LoadWAV_RW"
  "SDL_LoadObject"
  "SDL_LoadFunction"
  "SDL_LoadBMP"
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
  "SDL_AllocFormat"
  ;; checked all of these manually on wiki:
  "SDL_Vulkan_GetVkInstanceProcAddr"
  "SDL_SensorOpen"
  "SDL_SensorGetName"
  "SDL_SensorGetDeviceName"
  "SDL_SensorFromInstanceID"
  "SDL_GetTouchFinger"
  "SDL_GetVideoDriver"
  "SDL_GetPrefPath"
  "SDL_GetMouseFocus"  ; nullptr if nothing found, but wiki doesn't say anything
  "SDL_GetKeyboardFocus" ; nullptr if nothing found, but wiki doesn't say anything
  "SDL_GetInputDeviceName" ; wiki shows gibberish, but I am pretty certain this should be here
  "SDL_GetHint"
  "SDL_GetGrabbedWindow"
  "SDL_GetDefaultCursor"
  "SDL_GetCursor"
  "SDL_GetCurrentVideoDriver"
  "SDL_GetCurrentAudioDriver"
  "SDL_GetAudioDriver"
  "SDL_GetAudioDeviceName"
  "SDL_GameControllerNameForIndex"
  "SDL_GameControllerName"
  "SDL_GameControllerGetStringForButton"
  "SDL_GameControllerGetStringForAxis"
  "SDL_AndroidGetActivity")

;; ** core / void

(defl *return-void/core*              ; generated this in `ffi-type-transformer'
  "SDL_Quit" "SDL_QuitSubSystem" "SDL_GetVersion" "SDL_Delay"
  "SDL_DestroyRenderer" "SDL_DestroyTexture" "SDL_RenderPresent"
  "SDL_RenderGetScale" "SDL_RenderGetClipRect" "SDL_RenderGetViewport"
  "SDL_RenderGetLogicalSize" "SDL_UnlockTexture" "SDL_LogSetOutputFunction"
  "SDL_LogGetOutputFunction" "SDL_LogMessage" "SDL_LogCritical" "SDL_LogError"
  "SDL_LogWarn" "SDL_LogInfo" "SDL_LogDebug" "SDL_LogVerbose" "SDL_Log"
  "SDL_LogResetPriorities" "SDL_LogSetPriority" "SDL_LogSetAllPriority"
  "SDL_UnloadObject" "SDL_ClearHints" "SDL_DelHintCallback"
  "SDL_AddHintCallback" "SDL_HapticDestroyEffect" "SDL_HapticClose"
  "SDL_FilterEvents" "SDL_DelEventWatch" "SDL_AddEventWatch"
  "SDL_SetEventFilter" "SDL_FlushEvents" "SDL_FlushEvent" "SDL_PumpEvents"
  "SDL_GameControllerClose" "SDL_GameControllerUpdate" "SDL_JoystickClose"
  "SDL_JoystickUpdate" "SDL_JoystickGetGUIDString" "SDL_FreeCursor"
  "SDL_SetCursor" "SDL_WarpMouseInWindow" "SDL_SetTextInputRect"
  "SDL_StopTextInput" "SDL_StartTextInput" "SDL_SetModState"
  "SDL_GL_DeleteContext" "SDL_GL_SwapWindow" "SDL_GL_GetDrawableSize"
  "SDL_GL_ResetAttributes" "SDL_GL_UnloadLibrary" "SDL_DisableScreenSaver"
  "SDL_EnableScreenSaver" "SDL_DestroyWindow" "SDL_SetWindowGrab"
  "SDL_RestoreWindow" "SDL_MinimizeWindow" "SDL_MaximizeWindow"
  "SDL_RaiseWindow" "SDL_HideWindow" "SDL_ShowWindow" "SDL_SetWindowBordered"
  "SDL_GetWindowMaximumSize" "SDL_SetWindowMaximumSize"
  "SDL_GetWindowMinimumSize" "SDL_SetWindowMinimumSize" "SDL_GetWindowSize"
  "SDL_SetWindowSize" "SDL_GetWindowPosition" "SDL_SetWindowPosition"
  "SDL_SetWindowIcon" "SDL_SetWindowTitle" "SDL_VideoQuit" "SDL_GetClipRect"
  "SDL_UnlockSurface" "SDL_FreeSurface" "SDL_UnionRect" "SDL_CalculateGammaRamp"
  "SDL_GetRGBA" "SDL_GetRGB" "SDL_FreePalette" "SDL_FreeFormat"
  "SDL_CloseAudioDevice" "SDL_CloseAudio" "SDL_UnlockAudioDevice"
  "SDL_UnlockAudio" "SDL_LockAudioDevice" "SDL_LockAudio" "SDL_MixAudioFormat"
  "SDL_MixAudio" "SDL_FreeWAV" "SDL_PauseAudioDevice" "SDL_PauseAudio"
  "SDL_AudioQuit" "SDL_FreeRW" "SDL_DetachThread" "SDL_WaitThread"
  "SDL_DestroyCond" "SDL_DestroySemaphore" "SDL_DestroyMutex" "SDL_ClearError"
  "SDL_AtomicUnlock" "SDL_AtomicLock" "SDL_ResetAssertionReport"
  "SDL_SetAssertionHandler" "SDL_SetMainReady" "SDL_qsort" "SDL_free"
  ;; also void, but i have old specs, so they didn't show up in `ffi-type-transformer'
  "SDL_Vulkan_UnloadLibrary" "SDL_Vulkan_GetDrawableSize" "SDL_TriggerBreakpoint"
  "SDL_SetWindowsMessageHook" "SDL_SensorUpdate" "SDL_SensorClose"
  "SDL_LogMessageV" "SDL_ClearQueuedAudio" "SDL_SetWindowResizable"
  "SDL_iPhoneSetEventPump")

;; ** core / booleans (no error checking)

(defl *return-boolean-no-errors/core*
  ;; Google query: "SDL_TRUE" -"negative" -"-1" -"SDL_GetError" site:wiki.libsdl.org
  ;; look through all of these manually on the wiki:
  "SDL_Has3DNow"
  "SDL_HasSSE42"
  "SDL_HasMMX"
  "SDL_HasAVX"
  "SDL_HasAVX2"
  "SDL_HasSSE2"
  "SDL_HasSSE"
  "SDL_HasSSE41"
  "SDL_HasSSE3" 
  "SDL_HasEvent"
  "SDL_HasEvents"
  "SDL_HasScreenKeyboardSupport"
  "SDL_HasClipboardText"
  "SDL_HasIntersection"
  "SDL_HasAltiVec"
  "SDL_HasRDTSC"
  "SDL_IsTextInputActive"
  "SDL_IsScreenKeyboardShown"
  "SDL_IsScreenSaverEnabled"
  "SDL_IsGameController" ; SDL_FALSE if it isn't or it's an invalid index.
  "SDL_GameControllerGetAttached"
  "SDL_QuitRequested"
  "SDL_PointInRect"
  "SDL_RectEquals"
  "SDL_RectEmpty"
  "SDL_GetRelativeMouseMode"
  "SDL_SetHint"
  "SDL_GetHintBoolean"
  "SDL_SetHintWithPriority"
  "SDL_GL_ExtensionSupported"
  "SDL_IntersectRect"
  "SDL_IntersectRectAndLine"
  "SDL_SetClipRect"
  "SDL_AtomicCAS"
  "SDL_AtomicCASPtr"
  "SDL_AtomicTryLock"
  "SDL_AtomicDecRef"
  "SDL_RenderTargetSupported"
  "SDL_GetWindowGrab"
  "SDL_EnclosePoints"
  ;; Google Query: "SDL_TRUE" "SDL_GetError" -"negative" -"-1" site:wiki.libsdl.org
  ;; These ones give extra info with SDL_Error, but I guess they shouldn't
  ;; throw a condition, so they are a part of this list.
  ;; checked all of these manually on wiki:
  "SDL_JoystickGetAttached" ; call SDL_GetError() for more information.
  "SDL_RenderIsClipEnabled" ; SDL_TRUE if clipping is enabled or SDL_FALSE if not; call SDL_GetError() for more information.
  "SDL_RenderGetIntegerScale" ; "SDL_TRUE if integer scales are forced or SDL_FALSE if not and on failure"
  )

;; ** core / boolean check errors (on SDL_FALSE)

(defl *return-boolean-check-errors/core* ; checked all of these manually on wiki
  ;; Google Query: "SDL_TRUE" "SDL_GetError" -"negative" -"-1" site:wiki.libsdl.org
  ;; manually verified all of these
  "SDL_GetEventFilter" ; SDL_FALSE if there is no event filter set.
  "SDL_RemoveTimer" ; SDL_FALSE if the timer wasn't found
  "SDL_Vulkan_GetInstanceExtensions" ; Returns SDL_TRUE on success, SDL_FALSE on error
  "SDL_Vulkan_CreateSurface" ; Returns SDL_TRUE on success, SDL_FALSE on error.
  "SDL_GetWindowWMInfo"; SDL_FALSE if the information could not be retrieved;
  "SDL_DXGIGetOutputInfo" ; Returns SDL_TRUE on success or SDL_FALSE on failure;
  "SDL_PixelFormatEnumToMasks") ; Returns SDL_TRUE on success or SDL_FALSE if the conversion wasn't possible;

;; ** core / errors on 0

(defl *return-0-on-failure/core*    ; checked all of these manually on wiki
  ;; checked all of these manually on wiki:
  "SDL_SaveDollarTemplate"
  "SDL_RecordGesture"
  "SDL_WaitEventTimeout"
  "SDL_WaitEvent"
  "SDL_WriteBE16"
  "SDL_WriteBE32"
  "SDL_WriteBE64"
  "SDL_WriteLE16"
  "SDL_WriteLE32"
  "SDL_WriteLE64"
  "SDL_WriteU8"
  "SDL_RWread"
  "SDL_TLSCreate"
  "SDL_SaveAllDollarTemplates"
  "SDL_GetNumTouchFingers"
  ;; "SDL_JoystickGetGUID" ; TODO returns struct
  ;; "SDL_JoystickGetDeviceGUID" ; TODO returns struct
  "SDL_HapticQuery" ; Returns a list of supported haptic features in bitwise manner (OR'd), or 0 on failure; 
  "SDL_GetWindowID"
  "SDL_GetTouchDevice"
  "SDL_GameControllerGetButton" ; returns SDL_RELEASED when unpressed, not 0 like wiki says
  "SDL_AndroidGetJNIEnv"
  "SDL_AndroidGetExternalStorageState"
  "SDL_AddTimer")

;; ** core / return enum error check

(defparameter *return-enum-check-invalid/core*
  '(("SDL_SensorGetType" "SDL_SENSOR_INVALID")
    ("SDL_SensorGetDeviceType" "SDL_SENSOR_INVALID")
    ("SDL_GetPixelFormatName" "SDL_PIXELFORMAT_UNKNOWN")
    ("SDL_GetWindowPixelFormat" "SDL_PIXELFORMAT_UNKNOWN")
    ("SDL_MasksToPixelFormatEnum" "SDL_PIXELFORMAT_UNKNOWN")
    ("SDL_GetScancodeFromName" "SDL_SCANCODE_UNKNOWN")
    ("SDL_GetKeyFromName" "SDLK_UNKNOWN")
    ("SDL_JoystickCurrentPowerLevel" "SDL_JOYSTICK_POWER_UNKNOWN")
    ;; ("SDL_GameControllerGetBindForButton" "SDL_CONTROLLER_BINDTYPE_NONE") ; TODO uncomment (used to be a struct)
    ;; ("SDL_GameControllerGetBindForAxis" "SDL_CONTROLLER_BINDTYPE_NONE") ; TODO uncomment (used to be a struct)
    ("SDL_GameControllerGetButtonFromString" "SDL_CONTROLLER_AXIS_INVALID")
    ("SDL_GameControllerGetAxisFromString" "SDL_CONTROLLER_AXIS_INVALID")))

;; ** core / return constants indicating errors

(defparameter *return-constant-on-failure/core*
  (check-repeats
   (append
    (mapcar (lambda (x) (list x 0)) *return-0-on-failure/core*)
    `(("SDL_RegisterEvents" ,(1- (expt 2 32))))))) ; (Uint32)-1 if there are not enough user-defined events left.

;; ** core / skip

(defl *skip/core*                       ; checked all of these manually on wiki
  ;; requires a manual check
  "SDL_RWwrite" ; Returns the number of objects written, which will be less than **num** on error
  "SDL_DequeueAudio" ; Returns number of bytes dequeued, which could be less than requested; call SDL_GetError()
  ;; low-level stuff
  "SDL_SwapLE64" "SDL_SwapLE32" "SDL_SwapLE16" "SDL_SwapFloatLE"
  "SDL_SwapFloatBE" "SDL_SwapFloat" "SDL_SwapBE64" "SDL_SwapBE32" "SDL_SwapBE16"
  "SDL_Swap64" "SDL_Swap32" "SDL_Swap16"
  "SDL_ReadU8"     ; wiki says this returns 0 on failure, but that's plain wrong
  "SDL_ReadLE64" "SDL_ReadLE32" "SDL_ReadLE16" "SDL_ReadBE64" "SDL_ReadBE32"
  "SDL_ReadBE16"
  "SDL_MostSignificantBitIndex32"
  "SDL_MapRGBA"
  "SDL_MapRGB"
  ;; arithmetic operations
  "SDL_acos" "SDL_asin" "SDL_atan" "SDL_atan2" "SDL_sin" "SDL_cos" "SDL_pow"
  "SDL_log"
  ;; no error checking needed for sure
  "SDL_GetTicks"
  "SDL_WasInit" ; not a bool, "returns the initialization status of the specified subsystems"
  "SDL_GetThreadID"
  "SDL_ThreadID"
  "SDL_SetError"
  "SDL_SemValue"
  "SDL_GetRevisionNumber"
  "SDL_GetPerformanceFrequency"
  "SDL_LogGetPriority"
  "SDL_JoystickGetHat"
  "SDL_JoystickGetGUIDFromString"       ; Performs no error checking.
  "SDL_GetWindowTitle"
  "SDL_GetWindowFlags"
  "SDL_GetWindowBrightness"
  "SDL_SetWindowData"                   ; returns void*
  "SDL_GetThreadName"                   ; NULL if it doesn't have a name.
  "SDL_GetSystemRAM"
  "SDL_GetScancodeName" ; If the scancode doesn't have a name this function returns an empty string ("").
  "SDL_GetScancodeFromKey"
  "SDL_GetRevision"
  "SDL_GetRenderTarget"                 ; NULL for the default render target.
  "SDL_GetPowerInfo"
  "SDL_GetPerformanceCounter"
  "SDL_GetMouseState"
  "SDL_GetModState"
  "SDL_GetKeyName" ;  If the key doesn't have a name, this function returns an empty string ("")
  "SDL_GetKeyboardState"
  "SDL_GetGlobalMouseState"
  "SDL_GetEventState"
  "SDL_GetError"
  "SDL_GetDefaultAssertionHandler"
  "SDL_GetCPUCount"
  "SDL_GetCPUCacheLineSize"
  "SDL_GetAssertionReport"
  "SDL_GetAssertionHandler"
  "SDL_GameControllerGetAxis" ; Returns axis state (including 0) on success or 0 (also) on failure; call SDL_GetError() f
  "SDL_GameControllerEventState"
  "SDL_CompilerBarrier"
  "SDL_AtomicSetPtr"
  "SDL_AtomicSet"
  "SDL_AtomicIncRef"
  "SDL_AtomicGetPtr"
  "SDL_AtomicGet"
  "SDL_AtomicAdd"
  "SDL_Error"                        ; internal use https://wiki.libsdl.org/ToDo
  "SDL_ReportAssertion"              ; internal use https://wiki.libsdl.org/ToDo
  ;; these are bool-like, but since they return int, should probably stay that way in case SDL api extends on them
  "SDL_EventState"                 ; uint8 Returns `SDL_DISABLE` or `SDL_ENABLE`
  "SDL_JoystickGetButton" ; uint8, Returns 1 if the specified button is pressed, 0 otherwise.
  "SDL_PollEvent" ; int, Returns 1 if there is a pending event or 0 if there are none available.
  "SDL_MouseIsHaptic" ; int, Returns SDL_TRUE if the mouse is haptic or SDL_FALSE if it isn't.
  ;; no error checking needed, but I ain't that certain, or the wiki isn't
  "SDL_GetWindowData"
  "SDL_GetNumAudioDevices" ; A return value of -1 does not necessarily mean an error condition.
  "SDL_JoystickGetAxis" ; wat: Returns a 16-bit signed integer representing the current position of the axis or 0 on failure
  "SDL_GetRelativeMouseState"
  "SDL_IsDeviceDisconnected"            ; wiki shows some gibberish
  "SDL_GetKeyFromScancode"
  "SDL_GetAudioStatus"                  ; wiki shows gibberish
  "SDL_GetAudioDeviceStatus"            ; wiki shows gibberish
  "SDL_GameControllerGetJoystick" ; wiki unclear: Returns a SDL_Joystick object; call SDL_GetError() for more information.
  "SDL_ComposeCustomBlendMode"    ; no checks apparently
  ;; don't know about these, but probably better leave unchecked
  "SDL_HapticOpened" ; Returns 1 if it has been opened, 0 if it hasn't or on failure;
  "SDL_GetQueuedAudioSize" ; uint32 Returns the number of bytes (not samples!) of queued audio.
  "SDL_GetPlatform" ; If the correct platform name is not available, returns a string beginning with the text "Unknown".
  "SDL_GL_GetSwapInterval"
  "SDL_GL_GetProcAddress"
  ;; well, imma just ignore these, alright? also see https://wiki.libsdl.org/ToDo
  ;; some of these do their own arithmetic reporting
  "SDL_iconv_string" "SDL_iconv" "SDL_iconv_close" "SDL_iconv_open" "SDL_sqrt"
  "SDL_sinf" "SDL_scalbn" "SDL_floor" "SDL_fabs" "SDL_cosf" "SDL_copysign"
  "SDL_ceil" "SDL_snprintf" "SDL_sscanf" "SDL_strncasecmp" "SDL_strcasecmp"
  "SDL_strncmp" "SDL_strcmp" "SDL_strtod" "SDL_strtoull" "SDL_strtoll"
  "SDL_strtoul" "SDL_strtol" "SDL_atof" "SDL_atoi" "SDL_ulltoa" "SDL_lltoa"
  "SDL_ultoa" "SDL_ltoa" "SDL_uitoa" "SDL_itoa" "SDL_strstr" "SDL_strrchr"
  "SDL_strchr" "SDL_strlwr" "SDL_strupr" "SDL_strrev" "SDL_strdup" "SDL_strlcat"
  "SDL_utf8strlcpy" "SDL_strlcpy" "SDL_strlen" "SDL_wcslcat" "SDL_wcslcpy"
  "SDL_wcslen" "SDL_memcmp" "SDL_memmove" "SDL_memcpy" "SDL_memset"
  "SDL_tolower" "SDL_toupper" "SDL_isspace" "SDL_isdigit" "SDL_abs" "SDL_setenv"
  "SDL_getenv" "SDL_realloc" "SDL_calloc" "SDL_malloc" "strerror")

;; * SDL TTF

;; I looked up all the functions in this sections here:
;; https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html
;; "TTF_GetError: This is really a defined macro for SDL_GetError"
;; "TTF_SetError: This is really a defined macro for SDL_SetError"

;; ** ttf / negative return code is error

(defl *return-negative-on-failure/ttf*
  "TTF_Init" ; Returns: 0 on success, -1 on any error
  "TTF_SizeUNICODE"
  "TTF_SizeUTF8"
  "TTF_SizeText"
  "TTF_GlyphMetrics")

;; ** ttf / null is error

(defl *return-null-on-failure/ttf*
  "TTF_OpenFont"
  "TTF_OpenFontRW"
  "TTF_OpenFontIndex" 
  "TTF_OpenFontIndexRW"
  "TTF_RenderGlyph_Blended"
  "TTF_RenderUNICODE_Blended_Wrapped"
  "TTF_RenderUTF8_Blended_Wrapped"
  "TTF_RenderText_Blended_Wrapped"
  "TTF_RenderUNICODE_Blended"
  "TTF_RenderUTF8_Blended"
  "TTF_RenderText_Blended"
  "TTF_RenderGlyph_Shaded"
  "TTF_RenderUNICODE_Shaded"
  "TTF_RenderUTF8_Shaded"
  "TTF_RenderText_Shaded"
  "TTF_RenderGlyph_Solid"
  "TTF_RenderUNICODE_Solid"
  "TTF_RenderUTF8_Solid"
  "TTF_RenderText_Solid")

;; ** ttf / void

(defl *return-void/ttf*
  "TTF_Quit"
  "TTF_CloseFont"
  "TTF_SetFontKerning"
  "TTF_SetFontHinting"
  "TTF_SetFontOutline"
  "TTF_SetFontStyle"
  "TTF_ByteSwappedUNICODE")

;; ** ttf / bool-like ints

;; TODO no errors, 0 means false
"TTF_WasInit" ; 1 if already initialized, 0 if not initialized.
"TTF_FontFaceIsFixedWidth" ; >0 if font is a fixed width font. 0 if not a fixed width font.

;; ** ttf / errors on 0

(defl *return-0-on-failure/ttf*
  "TTF_GlyphIsProvided" ; "0 for an undefined character code", not a bool-like, returns an index
  )

;; ** ttf / constant on failure

(defparameter *return-constant-on-failure/ttf*
  (mapcar (lambda (x) (list x 0)) *return-0-on-failure/ttf*))

;; ** ttf / skip

(defl *skip/ttf*
  ;; don't need any processing for sure
  "TTF_Linked_Version"
  "TTF_FontFaces"
  "TTF_FontLineSkip"
  "TTF_FontDescent"
  "TTF_FontAscent"
  "TTF_FontHeight"
  "TTF_GetFontHinting"
  "TTF_GetFontOutline"
  "TTF_GetFontStyle"
  ;; not so sure about
  "TTF_GetFontKerning" ; "0 if kerning is disabled" I guess a 0 could be useful in calculations as is, so no error? bool-like?
  "TTF_FontFaceStyleName" ; The current style name of of the face of the font, or NULL perhaps.
  "TTF_FontFaceFamilyName" ; like TTF_FontFaceStyleName
  )

;; * SDL GFX

;; https://www.ferzkopp.net/Software/SDL2_gfx/Docs/html/index.html

;; ** gfx / negative return code is error

(defl *return-negative-on-failure/gfx*
  ;; primitives, all return -1 on failure
  "pixelColor"
  "pixelRGBA"
  "hlineColor"
  "hlineRGBA"
  "vlineColor"
  "vlineRGBA"
  "rectangleColor"
  "rectangleRGBA"
  "roundedRectangleColor"
  "roundedRectangleRGBA"
  "boxColor"
  "boxRGBA"
  "roundedBoxColor"
  "roundedBoxRGBA"
  "lineColor"
  "lineRGBA"
  "aalineColor"
  "aalineRGBA"
  "thickLineColor"
  "thickLineRGBA"
  "circleColor"
  "circleRGBA"
  "arcColor"
  "arcRGBA"
  "aacircleColor"
  "aacircleRGBA"
  "filledCircleColor"
  "filledCircleRGBA"
  "ellipseColor"
  "ellipseRGBA"
  "aaellipseColor"
  "aaellipseRGBA"
  "filledEllipseColor"
  "filledEllipseRGBA"
  "pieColor"
  "pieRGBA"
  "filledPieColor"
  "filledPieRGBA"
  "trigonColor"
  "trigonRGBA"
  "aatrigonColor"
  "aatrigonRGBA"
  "filledTrigonColor"
  "filledTrigonRGBA"
  "polygonColor"
  "polygonRGBA"
  "aapolygonColor"
  "aapolygonRGBA"
  "filledPolygonColor"
  "filledPolygonRGBA"
  "texturedPolygon"
  "bezierColor"
  "bezierRGBA"
  "characterColor"
  "characterRGBA"
  "stringColor"
  "stringRGBA"
  ;; framerate
  "SDL_getFramerate"
  "SDL_getFramecount"
  "SDL_setFramerate"
  ;; image filter
  "SDL_imageFilterAdd"
  "SDL_imageFilterMean"
  "SDL_imageFilterSub"
  "SDL_imageFilterAbsDiff"
  "SDL_imageFilterMult"
  "SDL_imageFilterMultNor"
  "SDL_imageFilterMultDivby2"
  "SDL_imageFilterMultDivby4"
  "SDL_imageFilterBitAnd"
  "SDL_imageFilterBitOr"
  "SDL_imageFilterDiv"
  "SDL_imageFilterBitNegation"
  "SDL_imageFilterAddByte"
  "SDL_imageFilterAddUint"
  "SDL_imageFilterAddByteToHalf"
  "SDL_imageFilterSubByte"
  "SDL_imageFilterSubUint"
  "SDL_imageFilterShiftRight"
  "SDL_imageFilterShiftRightUint"
  "SDL_imageFilterMultByByte"
  "SDL_imageFilterShiftRightAndMultByByte"
  "SDL_imageFilterShiftLeftByte"
  "SDL_imageFilterShiftLeftUint"
  "SDL_imageFilterShiftLeft"
  "SDL_imageFilterBinarizeUsingThreshold"
  "SDL_imageFilterClipToRange"
  "SDL_imageFilterNormalizeLinear")

;; ** gfx / null is error

(defl *return-null-on-failure/gfx*
  "shrinkSurface"
  "rotateSurface90Degrees"
  ;; these return new surfaces and don't explicitly state NULL on errors, let it be this way in case of changes:
  "zoomSurface"
  "rotozoomSurface" 
  "rotozoomSurfaceXY")

;; ** gfx / void

(defl *return-void/gfx*
  "zoomSurfaceSize"
  "rotozoomSurfaceSizeXY"
  "rotozoomSurfaceSize"
  "SDL_imageFilterMMXon"
  "SDL_imageFilterMMXoff"
  "SDL_initFramerate"
  "gfxPrimitivesSetFontRotation"
  "gfxPrimitivesSetFont")

;; ** gfx / bool-like ints

;; bool like (false on 0), no errors
"SDL_imageFilterMMXdetect" ; 1 of MMX was detected, 0 otherwise.

;; ** gfx / skip

(defl *skip/gfx*
  "SDL_framerateDelay")

;; * SDL IMAGE

;; https://www.libsdl.org/projects/SDL_image/docs/SDL_image.html

;; ** image / negative return code is error

(defl *return-negative-on-failure/image*
  ;; these weren't in the docs, the sources have -1 on error
  "IMG_SavePNG_RW"
  "IMG_SavePNG")

;; ** image / null is error

(defl *return-null-on-failure/image*
  "IMG_Load"
  "IMG_Load_RW"
  "IMG_LoadTyped_RW"
  "IMG_LoadBMP_RW"
  "IMG_LoadCUR_RW"
  "IMG_LoadGIF_RW"
  "IMG_LoadICO_RW"
  "IMG_LoadJPG_RW"
  "IMG_LoadLBM_RW"
  "IMG_LoadPCX_RW"
  "IMG_LoadPNG_RW"
  "IMG_LoadPNM_RW"
  "IMG_LoadTGA_RW"
  "IMG_LoadTIF_RW"
  "IMG_LoadXCF_RW"
  "IMG_LoadXPM_RW"
  "IMG_LoadXV_RW"
  "IMG_ReadXPMFromArray"
  ;; these weren't in the docs, assuming the same:
  "IMG_LoadWEBP_RW"
  "IMG_LoadTextureTyped_RW"
  "IMG_LoadTexture_RW"
  "IMG_LoadTexture")

;; ** image / bool-like ints

;; bool-like ints, 0 for false, 1 for true, not errors
'("IMG_isBMP"
  "IMG_isCUR"
  "IMG_isGIF"
  "IMG_isICO"
  "IMG_isJPG"
  "IMG_isLBM"
  "IMG_isPCX"
  "IMG_isPNG"
  "IMG_isPNM"
  "IMG_isTIF"
  "IMG_isXCF"
  "IMG_isXPM"
  "IMG_isXV"
  "IMG_isWEBP")

;; ** image / void

(defl *return-void/image*
  "IMG_Quit")

;; ** image / skip

(defl *skip/image*
  ;; not in the docs, looked these up in the sources, not surprises here 
  "IMG_Init" 
  "IMG_Linked_Version")

;; * all

;; ** all / negative checks

(defparameter *return-negative-on-failure/all*
  (append *return-negative-on-failure/core*
          *return-negative-on-failure/ttf*
          *return-negative-on-failure/gfx*
          *return-negative-on-failure/image*))

;; ** all / null returns

(defparameter *return-null-on-failure/all*
  (append *return-null-on-failure/core*
          *return-null-on-failure/ttf*
          *return-null-on-failure/gfx*
          *return-null-on-failure/image*))

;; ** all / void

(defparameter *return-void/all*
  (append *return-void/core*
          *return-void/ttf*
          *return-void/gfx*
          *return-void/image*))

;; ** all / boolean (no errors)

(defparameter *return-boolean-no-errors/all*
  (append *return-boolean-no-errors/core*))

;; ** all / boolean (with errors)

(defparameter *return-boolean-check-errors/all*
  (append *return-boolean-check-errors/core*))

;; ** all / enum checks

(defparameter *return-enum-check-invalid/all*
  (append *return-enum-check-invalid/core*))

(defparameter *return-enum-check-invalid-names/all*
  (mapcar #'first *return-enum-check-invalid/all*))

;; ** all / constant checks

(defparameter *return-constant-on-failure/all*
  (append *return-constant-on-failure/core*
          *return-constant-on-failure/ttf*))

(defparameter *return-constant-on-failure-names/all*
  (mapcar #'first *return-constant-on-failure/all*))

;; ** all / skip

(defparameter *skip/all*
  (append *skip/core* *skip/ttf* *skip/gfx* *skip/image*))

;; * checks

(progn
  (defparameter *all-conversion-lists*
    (list *return-negative-on-failure/all*
          *return-null-on-failure/all*
          *return-boolean-no-errors/all*
          *return-boolean-check-errors/all*
          *return-enum-check-invalid-names/all*
          *return-constant-on-failure-names/all*
          *return-void/all*
          *skip/all*))
  (check-repeats (apply #'append *all-conversion-lists*))
  (loop for x in *all-conversion-lists*
        do (loop for y in *all-conversion-lists*
                 when (not (eq x y))
                   do (assert (null (intersection x y :test #'equal)))))
  ;; total function count
  (reduce #'+ (mapcar #'length *all-conversion-lists*)))

;; No varargs support.  And this is not at its best, you shouldn't have to
;; specify any types yourself, but instead ask cffi what defcfun's arglist and
;; types are.  But I don't know how to do that.
(defmacro defun-with-passed-return-values (fn &rest desc)
  "Define a fn* which calls fn, where * in DESC says to leave argument as is,
and :typename says to pass a generated foreign object of the same type in its
place and, after the function is called, return it as a value.  The order of the
returned values is the same as they appear in the original arglist."
  (let ((arglist (second (function-lambda-expression (fdefinition fn)))))
    (print arglist)
    (assert (eql (length desc) (length arglist)))
    (loop for type-or-skip in desc
          for argname in arglist
          when (eql type-or-skip '*)
            collect argname into new-arglist
          when (keywordp type-or-skip)
            collect (list argname type-or-skip) into foreign-objects
          finally (return
                    `(defun ,(symbolicate fn '*) ,new-arglist
                       (cffi:with-foreign-objects ,foreign-objects
                         (,fn ,@arglist)
                         (values ,@(mapcar (lambda (x) `(cffi:mem-aref ,@x)) foreign-objects))))))))

;; (hu.dwim.sdl/ttf:ttf-init)

;; (defparameter font (hu.dwim.sdl/ttf:ttf-open-font "/usr/share/fonts/TTF/DejaVuSansMono-BoldOblique.ttf" 10))

;; (hu.dwim.sdl/ttf:ttf-font-faces font)

;; ;; TODO
;; ;; "TTF_SizeUNICODE"
;; ;; "TTF_SizeUTF8"
;; (defun-with-passed-return-values hu.dwim.sdl/ttf:ttf-size-text * * :int :int)
;; ;; (defun-with-passed-return-values hu.dwim.sdl/ttf:ttf-size- * * :int :int)

;; (ttf-size-text* font "Hello World!")


