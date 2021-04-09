(in-package :hu.dwim.sdl)

(defparameter *auto-passed-return-value+suppress-return/all*
  '(("SDL_GetWindowSize" * - -)
    ("TTF_SizeText" * * - -)
    ("TTF_SizeUnicode" * * - -)
    ("TTF_SizeUTF8" * * - -)))

(defparameter *auto-passed-return-value+pass-return/all* nil)
