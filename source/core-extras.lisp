(in-package :hu.dwim.sdl/core)

;; (hu.dwim.sdl::defun-with-passed-return-values*
;;   (t get-window-size * :int :int))

;; "SDL_RWwrite"

;; these functions may not exist on old systems (actually, they aren't even in the current specs)

;; The following functions aren't in the current specs, so they are *untested*.

;; wiki: "Returns number of bytes dequeued, which could be less than requested; call SDL_GetError()"
(cl:handler-case 
    (cl:progn
      (cl:setf (cl:fdefinition '%dequeue-audio)
               (cl:fdefinition 'dequeue-audio))
      (hu.dwim.sdl::define-condition dequeue-audio-error (hu.dwim.sdl::sdl-error) ())
      (cl:export '(%dequeue-audio dequeue-audio-error))
      (cl:defun dequeue-audio (dev data len)
        (cl:let ((result (cl:funcall '%dequeue-audio dev data len)))
          (cl:when (cl:< len result)
            (cl:error 'dequeue-audio-error
                      :format-control "SDL call failed: ~S.~%~%~a dequeued less than expected (~a < ~a)."
                      :format-arguments (cL:list (hu.dwim.sdl::get+clear-sdl-error) 'dequeue-audio result len)))
          result)))
  (common-lisp:undefined-function ()))

;; wiki: "Returns the number of objects written, which will be less than **num** on error; call SDL_GetError()"
(cl:handler-case 
    (cl:progn
      (cl:setf (cl:fdefinition '%rw-write)
               (cl:fdefinition 'rw-write))
      (hu.dwim.sdl::define-condition rw-write-error (hu.dwim.sdl::sdl-error) ())
      (cl:export '(%rw-write rw-write-error))
      (cl:defun rw-write (context ptr size num)
        (cl:let ((result (cl:funcall '%rw-write context ptr size num)))
          (cl:when (cl:< num result)
            (cl:error 'rw-write-error
                      :format-control "SDL call failed: ~S.~%~%~a dequeued less than expected (~a < ~a)."
                      :format-arguments (cL:list (hu.dwim.sdl::get+clear-sdl-error) 'rw-write result num)))
          result)))
  (common-lisp:undefined-function ()))
