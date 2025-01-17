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

;; * SDL Core

;; ** Commentary

;; [DK] I used SDL wiki search to identify various error return possibilities.
;; See https://wiki.libsdl.org/SGFunctions for some format details.
;; These aren't religiously followed by them, though.
;; And here's the search page: https://wiki.libsdl.org/FindPage
;; Below, I brace each query in slashes, don't put them in the actual search bar.

;; ** core / negative return code is error

(defl *return-negative-on-failure/core*
  ;; Query: /regex:"Returns 0 on success".*SDL_GetError/
  ;; Date queried: 30 March 2021. 121 results.
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
  "SDL_RenderSetIntegerScale" ; Returns 0 on success or a negative error code on failure;
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
  "SDL_NumSensors"  ; int, wiki says nothing, but this can't be negative, right?
  "SDL_NumHaptics"
  "SDL_JoystickNumBalls"
  "SDL_GetNumTouchDevices"
  "SDL_GetNumAudioDrivers"
  "SDL_HapticNewEffect"
  "SDL_HapticIndex"
  "SDL_Direct3D9GetAdapterIndex"
  "SDL_LoadDollarTemplates" ; could be 0, check yourself: "negative error code (or 0) on failure"
  "SDL_BlitSurface"
  "SDL_UpperBlitScaled"
  "SDL_UpperBlit"
  "SDL_SoftStretch"
  "SDL_LinuxSetThreadPriority"
  "SDL_AudioStreamPut"
  "SDL_AudioStreamAvailable"            ; int, # of bytes, no error info on wiki
  "SDL_AudioStreamGet"
  "SDL_RenderFlush"
  "SDL_LockTextureToSurface"
  "SDL_GetTextureScaleMode"
  "SDL_SetTextureScaleMode"
  "SDL_GameControllerGetSensorData"         ; -1 if an error occurred.
  "SDL_GameControllerSetSensorEnabled"      ; -1 if an error occurred.
  "SDL_GameControllerGetNumTouchpadFingers" ; no error info on wiki
  "SDL_GameControllerGetNumTouchpads"       ; no error info on wiki
  "SDL_GameControllerNumMappings" ; int, no error info, guessing negative for errors
  "SDL_JoystickSetVirtualHat"     ; -1 on error.
  "SDL_JoystickSetVirtualButton"  ; -1 on error.
  "SDL_JoystickSetVirtualAxis"    ; -1 on error.
  "SDL_JoystickDetachVirtual"     ; -1 if an error occurred.
  "SDL_JoystickAttachVirtual"     ; -1 if an error occurred.
  "SDL_JoystickGetDeviceInstanceID" ; If the index is out of range, this function will return -1.
  "SDL_JoystickGetDevicePlayerIndex" ; "-1 if it's not available", but probably like other joystick functions taking device_id
  "SDL_RenderGeometry"
  "SDL_RenderGeometryRaw"
  "SDL_SetTextureUserData"
  "SDL_SetPrimarySelectionText" ; Returns 0 on success or a negative error code on failure;
  "SDL_GameControllerGetSensorDataWithTimestamp" ; Return 0 or -1 if an error occurred.
  "SDL_SensorGetDataWithTimestamp" ; Returns 0 or -1 if an error occurred.
  "SDL_JoystickAttachVirtualEx" ; Returns the joystick's device index, or -1 if an error occurred.
  "SDL_FlashWindow" ; Returns 0 on success or a negative error code on failure;
  "SDL_hid_set_nonblocking" ; Returns 0 on success and -1 on error.
  "SDL_hid_exit" ; Returns 0 on success and -1 on error.
  "SDL_hid_init" ; Returns 0 on success and -1 on error.
  "SDL_UpdateNVTexture" ; Return 0 on success, or -1 if the texture is not valid.
  "SDL_RenderSetVSync" ; "Returns a 0 int on success, or non-zero on failure", manual check: -1 on error
  "SDL_hid_get_indexed_string" ; Returns 0 on success and -1 on error.
  "SDL_hid_get_manufacturer_string" ; Returns 0 on success and -1 on error.
  "SDL_hid_get_product_string" ; Returns 0 on success and -1 on error.
  "SDL_hid_get_serial_number_string" ; Returns 0 on success and -1 on error.
  "SDL_hid_send_feature_report" ; Returns the actual number of bytes written and -1 on error.
  "SDL_hid_read" ; Returns the actual number of bytes read and -1 on error.
  "SDL_hid_read_timeout" ; Returns the actual number of bytes read and -1 on error.
  "SDL_hid_write" ; Returns the actual number of bytes written and -1 on error.
  "SDL_hid_get_feature_report" ; number of bytes, -1 on error
  "SDL_GetDefaultAudioInfo" ; Returns 0 on success, nonzero on error, checked with sources to be SDL_error
  "SDL_GetAudioDeviceSpec" ; Returns 0 on success, nonzero on error
  "SDL_DestroyWindowSurface" ; Returns 0 on success or a negative error code on failure;
  "SDL_GetRectDisplayIndex" ; negative error code on failure
  "SDL_GetPointDisplayIndex" ; negative error code on failure
  "SDL_PremultiplyAlpha" ; Returns 0 on success or a negative error code on failure
  "SDL_LinuxSetThreadPriorityAndPolicy"
  "SDL_SetWindowMouseRect" ; negative error code on failure;
  )

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
  "SDL_CreateThreadWithStackSize"
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
  "SDL_DuplicateSurface"
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
  "SDL_GetAudioDeviceName"               ; NULL on error.
  "SDL_GameControllerGetStringForButton" ; NULL if an invalid axis is specified
  "SDL_GameControllerGetStringForAxis"   ; NULL if an invalid axis is specified
  "SDL_AndroidGetActivity"
  "SDL_GetWindowData" ; the sources indicate this that NULL is an error (when name is not found or NULL)
  "SDL_GameControllerGetJoystick" ; wiki unclear, sources: return NULL when the controller is NULL
  "SDL_GetPreferredLocales"
  "SDL_SIMDRealloc"
  "SDL_SIMDAlloc"
  "SDL_LoadFile"
  "SDL_LoadFile_RW"
  "SDL_NewAudioStream"           ; wiki is wrong on this, sources: NULL on error
  "SDL_CreateShapedWindow"
  "SDL_Metal_CreateView"         ; returns void *
  "SDL_Metal_GetLayer"           ; void *, though wiki says nothing about errors
  "SDL_RenderGetMetalLayer"      
  "SDL_RenderGetMetalCommandEncoder"
  "SDL_JoystickFromPlayerIndex"         ; NULL on failure;
  "SDL_GameControllerFromPlayerIndex" ; guessing it's like "SDL_JoystickFromPlayerIndex"
  "SDL_GameControllerMappingForIndex" ; NULL if the index is out of range.
  "SDL_AndroidGetJNIEnv" ; void *, "0 on error"
  "SDL_GetTextureUserData" ; void *, "NULL if the texture is not valid"
  "SDL_GetTouchName" ; NULL if the index is invalid
  "SDL_hid_enumerate" ; NULL in the case of failure
  "SDL_hid_open" ; NULL on failure.
  "SDL_hid_open_path" ; NULL on failure
  "SDL_RenderGetWindow" ; NULL on failure
  "SDL_GameControllerPath" ; NULL if there is no path or the identifier passed is invalid.
  "SDL_GameControllerPathForIndex" ; NULL if there is no path or the index is invalid.
  "SDL_JoystickPath" ;  If no path can be found, this function returns NULL; call SDL_GetError() for more information.
  "SDL_JoystickPathForIndex" ;  If no path can be found, this function returns NULL; call SDL_GetError() for more information.
  "SDL_GetWindowMouseRect" ; Returns a pointer to the mouse confinement rectangle of a window, or NULL if there isn't one.
  "SDL_GetWindowICCProfile" ; NULL on failure; call SDL_GetError()
  )

;; ** core / void

(defl *return-void/core*              ; generated this in `ffi-type-transformer'
  "SDL_Quit" "SDL_QuitSubSystem" "SDL_GetVersion" "SDL_Delay"
  "SDL_OnApplicationDidBecomeActive" "SDL_OnApplicationWillEnterForeground"
  "SDL_OnApplicationDidEnterBackground" "SDL_OnApplicationWillResignActive"
  "SDL_OnApplicationDidReceiveMemoryWarning" "SDL_OnApplicationWillTerminate"
  "SDL_DestroyRenderer" "SDL_DestroyTexture" "SDL_RenderPresent"
  "SDL_RenderGetScale" "SDL_RenderGetClipRect" "SDL_RenderGetViewport"
  "SDL_RenderGetLogicalSize" "SDL_UnlockTexture" "SDL_Metal_GetDrawableSize"
  "SDL_Metal_DestroyView" "SDL_LogSetOutputFunction" "SDL_LogGetOutputFunction"
  "SDL_LogMessage" "SDL_LogCritical" "SDL_LogError" "SDL_LogWarn" "SDL_LogInfo"
  "SDL_LogDebug" "SDL_LogVerbose" "SDL_Log" "SDL_LogResetPriorities"
  "SDL_LogSetPriority" "SDL_LogSetAllPriority" "SDL_UnloadObject"
  "SDL_ClearHints" "SDL_DelHintCallback" "SDL_AddHintCallback"
  "SDL_HapticDestroyEffect" "SDL_HapticClose" "SDL_FilterEvents"
  "SDL_DelEventWatch" "SDL_AddEventWatch" "SDL_SetEventFilter" "SDL_FlushEvents"
  "SDL_FlushEvent" "SDL_PumpEvents" "SDL_GameControllerClose"
  "SDL_GameControllerUpdate" "SDL_GameControllerSetPlayerIndex"
  "SDL_SensorUpdate" "SDL_SensorClose" "SDL_UnlockSensors" "SDL_LockSensors"
  "SDL_JoystickClose" "SDL_JoystickUpdate" "SDL_JoystickGetGUIDString"
  "SDL_JoystickSetPlayerIndex" "SDL_UnlockJoysticks" "SDL_LockJoysticks"
  "SDL_FreeCursor" "SDL_SetCursor" "SDL_WarpMouseInWindow"
  "SDL_SetTextInputRect" "SDL_StopTextInput" "SDL_StartTextInput"
  "SDL_SetModState" "SDL_GL_DeleteContext" "SDL_GL_SwapWindow"
  "SDL_GL_GetDrawableSize" "SDL_GL_ResetAttributes" "SDL_GL_UnloadLibrary"
  "SDL_DisableScreenSaver" "SDL_EnableScreenSaver" "SDL_DestroyWindow"
  "SDL_SetWindowGrab" "SDL_RestoreWindow" "SDL_MinimizeWindow"
  "SDL_MaximizeWindow" "SDL_RaiseWindow" "SDL_HideWindow" "SDL_ShowWindow"
  "SDL_SetWindowResizable" "SDL_SetWindowBordered" "SDL_GetWindowMaximumSize"
  "SDL_SetWindowMaximumSize" "SDL_GetWindowMinimumSize"
  "SDL_SetWindowMinimumSize" "SDL_GetWindowSize" "SDL_SetWindowSize"
  "SDL_GetWindowPosition" "SDL_SetWindowPosition" "SDL_SetWindowIcon"
  "SDL_SetWindowTitle" "SDL_VideoQuit" "SDL_SetYUVConversionMode"
  "SDL_GetClipRect" "SDL_UnlockSurface" "SDL_FreeSurface" "SDL_UnionRect"
  "SDL_CalculateGammaRamp" "SDL_GetRGBA" "SDL_GetRGB" "SDL_FreePalette"
  "SDL_FreeFormat" "SDL_SIMDFree" "SDL_CloseAudioDevice" "SDL_CloseAudio"
  "SDL_UnlockAudioDevice" "SDL_UnlockAudio" "SDL_LockAudioDevice"
  "SDL_LockAudio" "SDL_ClearQueuedAudio" "SDL_MixAudioFormat" "SDL_MixAudio"
  "SDL_FreeAudioStream" "SDL_AudioStreamClear" "SDL_FreeWAV"
  "SDL_PauseAudioDevice" "SDL_PauseAudio" "SDL_AudioQuit" "SDL_FreeRW"
  "SDL_DetachThread" "SDL_WaitThread" "SDL_DestroyCond" "SDL_DestroySemaphore"
  "SDL_DestroyMutex" "SDL_ClearError" "SDL_MemoryBarrierAcquireFunction"
  "SDL_MemoryBarrierReleaseFunction" "SDL_AtomicUnlock" "SDL_AtomicLock"
  "SDL_ResetAssertionReport" "SDL_SetAssertionHandler" "SDL_SetMainReady"
  "SDL_qsort" "SDL_GetMemoryFunctions" "SDL_free"
  "SDL_GetJoystickGUIDInfo"
  "SDL_GUIDToString" "SDL_ClearComposition" "SDL_ResetKeyboard"
  "SDL_GetOriginalMemoryFunctions" "SDL_SetWindowKeyboardGrab"
  "SDL_SetWindowMouseGrab" "SDL_ResetHints" "SDL_hid_ble_scan" "SDL_hid_close"
  "SDL_hid_free_enumeration" "SDL_RenderLogicalToWindow"
  "SDL_RenderWindowToLogical"
  "SDL_SetWindowAlwaysOnTop"
  "SDL_UnionFRect"
  "SDL_GetWindowSizeInPixels"
  "SDL_TLSCleanup"
  )

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
  "SDL_IsGameController"       ; SDL_FALSE if it isn't or it's an invalid index.
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
  "SDL_JoystickGetAttached"          ; call SDL_GetError() for more information.
  "SDL_RenderIsClipEnabled" ; SDL_TRUE if clipping is enabled or SDL_FALSE if not; call SDL_GetError() for more information.
  "SDL_RenderGetIntegerScale" ; "SDL_TRUE if integer scales are forced or SDL_FALSE if not and on failure"
  "SDL_IsDevicePresent"  ; looked this up in the sources, wiki has nothing on it
  ;; also, checked on wiki:
  "SDL_HasColorKey"
  "SDL_HasSurfaceRLE"
  "SDL_HasNEON"
  "SDL_HasARMSIMD"
  "SDL_HasAVX512F"
  "SDL_IsTablet"
  "SDL_IsShapedWindow"
  "SDL_GameControllerHasLED"
  "SDL_JoystickHasLED"
  "SDL_GameControllerHasSensor"
  "SDL_GameControllerHasButton"
  "SDL_GameControllerHasAxis"
  "SDL_GameControllerIsSensorEnabled"
  "SDL_JoystickGetAxisInitialState"
  "SDL_JoystickIsVirtual"
  "SDL_JoystickHasRumble"
  "SDL_IsTextInputShown"
  "SDL_GetWindowMouseGrab"
  "SDL_GameControllerHasRumble"
  "SDL_JoystickHasRumbleTriggers"
  "SDL_ResetHint"
  "SDL_GameControllerHasRumbleTriggers"
  "SDL_GetWindowKeyboardGrab"
  "SDL_EncloseFPoints"
  "SDL_IntersectFRectAndLine"
  "SDL_HasWindowSurface"
  "SDL_HasPrimarySelectionText"
  "SDL_HasLSX"
  "SDL_HasLASX"
  "SDL_HasIntersectionF"
  "SDL_IntersectFRect"
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

;; ** core / bool-like ints, 0 for false

;; these return int or uint, but are bool-like, so let's track them seperately, just in case
;; for now, they are simply merged with the sdl_bool list later on

(defl *return-bool-like-0-for-false/core*
  "SDL_JoystickGetButton" ; uint8, Returns 1 if the specified button is pressed, 0 otherwise.
  "SDL_GameControllerGetButton" ; returns SDL_RELEASED (0) when unpressed
  "SDL_PollEvent" ; int, Returns 1 if there is a pending event or 0 if there are none available.
  "SDL_MouseIsHaptic" ; int, Returns SDL_TRUE if the mouse is haptic or SDL_FALSE if it isn't.
  "SDL_HapticOpened" ; Returns 1 if it has been opened, 0 if it hasn't or on failure;
  "SDL_GameControllerSendEffect" ; Returns 0, or -1 if this controller or driver doesn't support effect packets
  )

;; ** core / bool-like ints, negative on errors

(defl *return-bool-like-0-for-false-negative-for-errors/core*
  "SDL_JoystickIsHaptic"
  "SDL_HapticRumbleSupported"
  "SDL_HapticEffectSupported"
  )

;; ** core / errors on 0

(defl *return-0-on-failure/core*       ; checked all of these manually on wiki
  ;; checked all of these manually on wiki:
  "SDL_WriteU8" ; size_t, "Returns 1 on success or 0 on failure; call SDL_GetError() for more information."
  "SDL_WriteBE16"
  "SDL_WriteBE32"
  "SDL_WriteBE64"
  "SDL_WriteLE16"
  "SDL_WriteLE32"
  "SDL_WriteLE64"
  "SDL_WaitEventTimeout" ; int, "Returns 1 on success or 0 if there was an error while waiting for events; call"
  "SDL_WaitEvent" ; int, "Returns 1 on success or 0 if there was an error while waiting for events; call"
  "SDL_RecordGesture" ; int, "Returns 1 on success or 0 if the specified device could not be found"
  "SDL_TLSCreate" ; SDL_TLSID, "Returns the newly created thread local storage identifier or 0 on error."
  "SDL_SaveDollarTemplate" ; int, "Returns 1 on success or 0 on failure; call SDL_GetError() for more information."
  "SDL_SaveAllDollarTemplates" ; int, "0 on failure;"
  "SDL_GetNumTouchFingers" ; Returns the number of active fingers for a given touch device on success or 0 on failure;
  "SDL_HapticQuery" ; Returns a list of supported haptic features in bitwise manner (OR'd), or 0 on failure; 
  "SDL_GetWindowID" ; uint32, "0 on failure"
  "SDL_GetTouchDevice" ; SDL_TouchID, "0 if the index is invalid;"
  "SDL_AndroidGetExternalStorageState" ; int, "0 on failure"
  "SDL_AddTimer" ; Returns a timer ID or 0 if an error occurs; call SDL_GetError() for more information.
  "SDL_GetKeyFromScancode" ; sources indicate that if scancode is not found, 0 is returned and error is set
  "SDL_JoystickGetDeviceProductVersion" ; If called on an invalid index, this function returns zero
  "SDL_JoystickGetDeviceProduct" ; If called on an invalid index, this function returns zero
  "SDL_JoystickGetDeviceVendor" ; If called on an invalid index, this function returns zero
  ;; this one is kind of questionable, but you can probably keep track where you are and avoid this error:
  "SDL_RWread" ; Returns the number of objects read, or 0 at error or end of file; call SDL_GetError() for more information.
  )

;; ** core / string starts with error

(defparameter *return-string-on-failure/core*
  '(("SDL_GetPixelFormatName" "SDL_PIXELFORMAT_UNKNOWN")
    ("SDL_GetPlatform" "Unknown") ; If the correct platform name is not available, returns a string beginning with the text "Unknown".
    )) ; "SDL_PIXELFORMAT_UNKNOWN" (string) if the format isn't recognized

;; ** core / return enum error check

;; multiple wrong return values are supported (member is used for checking)
(defparameter *return-enum-check-invalid/core*
  '(("SDL_SensorGetType" "SDL_SENSOR_INVALID") ; "`SDL_SENSOR_INVALID` if `sensor` is NULL."
    ("SDL_SensorGetDeviceType" "SDL_SENSOR_INVALID") ; "if `device_index` is out of range."
    ;; for the following two, also see "SDL_GetPixelFormatName":
    ("SDL_GetWindowPixelFormat" "SDL_PIXELFORMAT_UNKNOWN") ; call SDL_GetError() for more information.
    ("SDL_MasksToPixelFormatEnum" "SDL_PIXELFORMAT_UNKNOWN") ; "if the conversion wasn't possible."
    ("SDL_GetScancodeFromName" "SDL_SCANCODE_UNKNOWN") ; "if the name wasn't recognized; call SDL_GetError()"
    ("SDL_GetKeyFromName" "SDLK_UNKNOWN")              ; for more information. 
    ("SDL_JoystickCurrentPowerLevel" "SDL_JOYSTICK_POWER_UNKNOWN") ; a bit questionable that this should be here really
    ;; "On failure (like the given Controller axis doesn't exist on the device) [...] SDL_CONTROLLER_BINDTYPE_NONE"
    ("SDL_GameControllerGetButtonFromString" "SDL_CONTROLLER_AXIS_INVALID") ; "if no match was found."
    ("SDL_GameControllerGetAxisFromString" "SDL_CONTROLLER_AXIS_INVALID") ; "if no match was found."
    ;; Return 0 on success, SDL_INVALID_SHAPE_ARGUMENT on an invalid shape argument, or SDL_NONSHAPEABLE_WINDOW if the SDL_Window given does not reference a valid shaped window.
    ("SDL_SetWindowShape" "SDL_INVALID_SHAPE_ARGUMENT" "SDL_NONSHAPEABLE_WINDOW")
    ("SDL_JoystickGetDeviceType" "SDL_JOYSTICK_TYPE_UNKNOWN") ; "If called on an invalid index"
    ))

;; ** core / return constants indicating errors

(defparameter *return-constant-on-failure/core*
  (check-repeats
   (append
    (mapcar (lambda (x) (list x 0)) *return-0-on-failure/core*)
    `(("SDL_RegisterEvents" ,(1- (expt 2 32))))))) ; (Uint32)-1 if there are not enough user-defined events left.

;; ** core / skip

;; Some things on this list are commented so that the user gets a warning about them.
(defl *skip/core*                       ; checked all of these manually on wiki
  ;; requires a different sort of check - redefined these in core-extras.lisp
  "SDL_RWwrite" ; See README Returns the number of objects written, which will be less than **num** on error
  "SDL_DequeueAudio" ; See README, Returns number of bytes dequeued, which could be less than requested; call SDL_GetError()
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
  ;; no error checking needed for sure (all manually checked)
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
  "SDL_GetErrorMsg" ;; allows the caller to copy the error <...> but otherwise operates exactly the same as SDL_GetError().
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
  "SDL_EventState"
  "SDL_GL_GetProcAddress" ; "Some drivers return NULL if a function isn't supported, but you can't count on this behavior."
  "SDL_GL_GetSwapInterval" ; "If [...] can't determine [...] isn't a valid current context [...] return 0 as a safe default."
  "SDL_GetRelativeMouseState" ; wiki says nothing about errors, sources have no error handling
  "SDL_ComposeCustomBlendMode"   ; no checks apparently, sources indicate so too
  "SDL_JoystickGetAxis" ; Returns a 16-bit signed integer representing the current position of the axis or 0 on failure
  "SDL_GetNumAudioDevices" ; A return value of -1 does not necessarily mean an error condition.
  "SDL_SetMemoryFunctions"
  "SDL_GetNumAllocations"               ; int, takes no args
  "SDL_GetShapedWindowMode"
  "SDL_GameControllerGetPlayerIndex"    ; -1 if it's not available.
  "SDL_JoystickGetPlayerIndex"          ; -1 if it's not available.
  "SDL_GameControllerRumbleTriggers"    ; -1 if trigger rumble isn't supported
  "SDL_GameControllerRumble"            ; -1 if rumble isn't supported
  "SDL_JoystickSendEffect" ; Returns 0, or -1 if this joystick or driver doesn't support effect packets
  "SDL_JoystickRumbleTriggers"
  "SDL_JoystickRumble"
  "SDL_GameControllerSetLED"
  "SDL_JoystickSetLED"
  "SDL_GameControllerGetProductVersion" ; "zero if unavailable"
  "SDL_GameControllerGetProduct"        ; "zero if unavailable"
  "SDL_GameControllerGetVendor"         ; "zero if unavailable"
  "SDL_JoystickGetSerial"
  "SDL_JoystickGetProductVersion"
  "SDL_JoystickGetProduct"
  "SDL_JoystickGetVendor"
  "SDL_GetTouchDeviceType"
  "SDL_GameControllerMappingForDeviceIndex" ; NULL if no mapping is available.
  "SDL_GameControllerNameForIndex" ; NULL if there is no name or the index is invalid.
  "SDL_GameControllerName" ; NULL if there is no name or the identifier passed is invalid.
  "SDL_GameControllerGetType"
  "SDL_GameControllerGetSerial"
  "SDL_GameControllerTypeForIndex"      ; no error info
  "SDL_GUIDFromString"                  ; Returns a ::SDL_GUID structure
  "SDL_GameControllerGetAppleSFSymbolsNameForButton" ; const char*, NULL if the name can't be found
  "SDL_GameControllerGetAppleSFSymbolsNameForAxis" ; const char*, NULL if the name can't be found
  "SDL_hid_device_change_count" ; Returns a change counter [...] or 0 if device change detection isn't available.
  "SDL_GetTicks64"
  ;; "SDL_AudioStreamFlush"
  "SDL_GetAudioDeviceStatus" ; Returns the SDL_AudioStatus of the specified audio device.
  "SDL_SoftStretchLinear"
  "SDL_GameControllerGetFirmwareVersion" ; Return the controller firmware version, or zero if unavailable.
  "SDL_JoystickGetType"
  "SDL_JoystickGetFirmwareVersion" ; If the firmware version isn't available this function returns 0.
  "SDL_SIMDGetAlignment"
  ;; some enums (not all, and no error checking):
  "SDL_GetAudioStatus" ; wiki shows gibberish, sources: returns enum, no error checks
  "SDL_GetDisplayOrientation"   ; SDL_ORIENTATION_UNKNOWN if it isn't available.
  "SDL_GetYUVConversionModeForResolution" ; no details on wiki, source: no apparent fatal errors
  "SDL_GetYUVConversionMode" ; no details on wiki, source: no apparent fatal errors
  ;; no error checking needed, but I ain't that certain, or the wiki isn't
  "SDL_GetQueuedAudioSize" ; uint32 Returns 0 by default or if device not found, but the size can be 0 too, couldn't it?
  "SDL_GameControllerGetSensorDataRate" ; Return the data rate, or 0.0f if the data rate is not available.
  ;; "SDL_GameControllerGetTouchpadFinger" ; int, but wiki gives no info on return value
  ;; "SDL_IsDeviceDisconnected" ; wiki shows gibberish, can't find the source definition for this, see SDL_IsDevicePresent
  ;; "SDL_GetPrimarySelectionText" ; Returns the primary selection text on success or an empty string on failure;
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
  "SDL_getenv" "SDL_realloc" "SDL_calloc" "SDL_malloc" "strerror" "SDL_tanf"
  "SDL_tan" "SDL_sqrtf" "SDL_scalbnf" "SDL_powf" "SDL_log10f" "SDL_log10"
  "SDL_logf" "SDL_fmodf" "SDL_fmod" "SDL_truncf" "SDL_trunc" "SDL_floorf"
  "SDL_fabsf" "SDL_expf" "SDL_exp" "SDL_copysignf" "SDL_ceilf" "SDL_atan2f"
  "SDL_atanf" "SDL_asinf" "SDL_acosf" "SDL_utf8strlen" "SDL_strtokr"
  "SDL_wcsncasecmp" "SDL_wcscasecmp" "SDL_wcsncmp" "SDL_wcscmp" "SDL_wcsstr"
  "SDL_wcsdup" "SDL_crc32" "SDL_islower" "SDL_isupper"
  "SDL_lroundf" "SDL_lround" "SDL_roundf" "SDL_round" "SDL_asprintf"
  "SDL_strcasestr" "SDL_isgraph" "SDL_isprint" "SDL_ispunct" "SDL_isxdigit"
  "SDL_iscntrl" "SDL_isblank" "SDL_isalnum" "SDL_isalpha" "SDL_utf8strnlen"
  "SDL_crc16" "SDL_bsearch"
  )

;; * SDL TTF

;; I looked up all the functions in this sections here:
;; https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html
;; "TTF_GetError: This is really a defined macro for SDL_GetError"
;; "TTF_SetError: This is really a defined macro for SDL_SetError"

;; ** ttf / negative return code is error

(defl *return-negative-on-failure/ttf*
  "TTF_Init"                            ; Returns: 0 on success, -1 on any error
  "TTF_SizeUNICODE"
  "TTF_SizeUTF8"
  "TTF_SizeText"
  "TTF_GlyphMetrics"
  "TTF_GlyphMetrics32"
  "TTF_GetFontKerningSizeGlyphs" ; sources: -1 on error
  "TTF_GetFontKerningSizeGlyphs32"
  "TTF_SetFontSizeDPI"
  "TTF_SetFontSize"
  "TTF_SetFontScriptName"
  "TTF_SetFontDirection"
  "TTF_SetScript" ; -1 if SDL_ttf is not compiled with HarfBuzz support.
  "TTF_SetDirection" ; -1 if SDL_ttf is not compiled with HarfBuzz support.
  "TTF_MeasureText"
  "TTF_MeasureUTF8"
  "TTF_MeasureUNICODE"
  "TTF_GetFontWrappedAlign" ; checked with sources
  "TTF_SetFontSDF" ; checked with sources
  "TTF_GetFontSDF" ; checked with sources
  )

;; ** ttf / null is error

(defl *return-null-on-failure/ttf*
  "TTF_OpenFont"
  "TTF_OpenFontRW"
  "TTF_OpenFontDPI"
  "TTF_OpenFontDPIRW"
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
  "TTF_RenderText_Solid"
  "TTF_RenderGlyph32_LCD"
  "TTF_RenderGlyph_LCD"
  "TTF_RenderUNICODE_LCD_Wrapped"
  "TTF_RenderUTF8_LCD_Wrapped"
  "TTF_RenderText_LCD_Wrapped"
  "TTF_RenderUNICODE_LCD"
  "TTF_RenderUTF8_LCD"
  "TTF_RenderText_LCD"
  "TTF_RenderGlyph32_Blended"
  "TTF_RenderGlyph32_Shaded"
  "TTF_RenderUNICODE_Shaded_Wrapped"
  "TTF_RenderUTF8_Shaded_Wrapped"
  "TTF_RenderText_Shaded_Wrapped"
  "TTF_RenderGlyph32_Solid"
  "TTF_RenderUNICODE_Solid_Wrapped"
  "TTF_RenderUTF8_Solid_Wrapped"
  "TTF_RenderText_Solid_Wrapped"
  "TTF_OpenFontIndex"
  "TTF_OpenFontIndexDPI"
  "TTF_OpenFontIndexDPIRW"
  "TTF_OpenFontIndexRW"
  )

;; ** ttf / void

(defl *return-void/ttf*
  "TTF_Quit"
  "TTF_CloseFont"
  "TTF_SetFontKerning"
  "TTF_SetFontHinting"
  "TTF_SetFontOutline"
  "TTF_SetFontStyle"
  "TTF_GetFontKerningSize" ; deprecated, not exporting it, as it can yield wrong results
  "TTF_SetFontWrappedAlign"
  "TTF_ByteSwappedUNICODE")

;; ** ttf / bool-like ints, 0 for false

(defl *return-bool-like-0-for-false/ttf*
  "TTF_WasInit" ; 1 if already initialized, 0 if not initialized.
  "TTF_FontFaceIsFixedWidth") ; >0 if font is a fixed width font. 0 if not a fixed width font

;; ** ttf / errors on 0

(defl *return-0-on-failure/ttf*
  "TTF_GlyphIsProvided" ; "0 for an undefined character code", not a bool-like, returns an index
  "TTF_GlyphIsProvided32"
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
  "TTF_FontFaceStyleName" ; The current style name of the face of the font, or NULL perhaps.
  "TTF_FontFaceFamilyName" ; like TTF_FontFaceStyleName
  "TTF_GetHarfBuzzVersion"
  "TTF_GetFreeTypeVersion"
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

;; ** gfx / bool-like ints, 0 for false

(defl *return-bool-like-0-for-false/gfx*
  "SDL_imageFilterMMXdetect") ; 1 of MMX was detected, 0 otherwise.

;; ** gfx / skip

(defl *skip/gfx*
  "SDL_framerateDelay")

;; * SDL Image

;; https://www.libsdl.org/projects/SDL_image/docs/SDL_image.html

;; ** image / negative return code is error

(defl *return-negative-on-failure/image*
  ;; these weren't in the docs, the sources have -1 on error
  "IMG_SaveJPG"
  "IMG_SaveJPG_RW"
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
  "IMG_LoadGIFAnimation_RW"
  "IMG_LoadAnimationTyped_RW"
  "IMG_LoadAnimation_RW"
  "IMG_LoadAnimation"
  "IMG_LoadSizedSVG_RW"
  "IMG_LoadQOI_RW"
  "IMG_LoadJXL_RW"
  "IMG_LoadAVIF_RW"
  "IMG_ReadXPMFromArray"
  "IMG_ReadXPMFromArrayToRGB888"
  "IMG_LoadSVG_RW" ; looked up in the sources
  ;; these weren't in the docs, assuming the same:
  "IMG_LoadWEBP_RW"
  "IMG_LoadTextureTyped_RW"
  "IMG_LoadTexture_RW"
  "IMG_LoadTexture")

;; ** image / bool-like ints, 0 for false

;; bool-like ints, 0 for false, 1 for true, no errors
(defl *return-bool-like-0-for-false/image*
  "IMG_isSVG"
  "IMG_isQOI"
  "IMG_isJXL"
  "IMG_isAVIF"
  "IMG_isBMP"
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
  "IMG_FreeAnimation"
  "IMG_Quit")

;; ** image / skip

(defl *skip/image*
  ;; not in the docs, looked these up in the sources, no surprises here 
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

;; ** all / bool-like 0 for false

(defparameter *return-bool-like-0-for-false/all*
  (append *return-bool-like-0-for-false/core*
          *return-bool-like-0-for-false/gfx*
          *return-bool-like-0-for-false/ttf*
          *return-bool-like-0-for-false/image*))

;; ** all / boolean (no errors)

(defparameter *return-boolean-no-errors/all*
  (append *return-boolean-no-errors/core*
          *return-bool-like-0-for-false/all*))

;; ** all / boolean (with errors)

(defparameter *return-boolean-check-errors/all*
  (append *return-boolean-check-errors/core*))

;; ** all / bool-like ints, negative on errors

(defparameter *return-bool-like-0-for-false-negative-for-errors/all*
  (append *return-bool-like-0-for-false-negative-for-errors/core*))

;; ** all / string starts with error checks

(defparameter *return-string-on-failure/all*
  (append *return-string-on-failure/core*))

(defparameter *return-string-on-failure-names/all*
  (mapcar #'first *return-string-on-failure/all*))

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
          *return-bool-like-0-for-false-negative-for-errors/all*
          *return-string-on-failure-names/all*
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

;; (hu.dwim.sdl/ttf:ttf-init)

;; (defparameter font (hu.dwim.sdl/ttf:ttf-open-font "/usr/share/fonts/TTF/DejaVuSansMono-BoldOblique.ttf" 10))

;; (hu.dwim.sdl/ttf:ttf-font-faces font)

;; ;; TODO
;; ;; "TTF_SizeUNICODE"
;; ;; "TTF_SizeUTF8"
;; (defun-with-passed-return-values hu.dwim.sdl/ttf:ttf-size-text * * :int :int)
;; ;; (defun-with-passed-return-values hu.dwim.sdl/ttf:ttf-size- * * :int :int)

;; (ttf-size-text* font "Hello World!")
