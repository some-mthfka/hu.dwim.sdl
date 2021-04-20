(uiop:define-package :hu.dwim.sdl
  (:use #:cl
        #:alexandria
        #:cffi
        #:cl-ppcre)
  (:import-from #:cl-ppcre #:regex-replace-all)
  (:export
   #:sdl-error
   #:ignore-sdl-errors
   #:with-sdl-slots
   #:with-sdl-slots*
   ;; NOTE: it's not possible to re-export stuff from HU.DWIM.SDL.FFI by directly referencing the symbols,
   ;; because that changes the behavior of CL:SHADOW and thus breaks things.
   ;; http://paste.lisp.org/+3D97
   ;; 2015-10-19 #sbcl
   ;; (22:12:53) Xof: "If it is accessible as an internal symbol via use-package, it is first imported into package, then exported."
   ;; (22:12:59) Xof: (CLHS EXPORT)
   ;; (22:13:17) Xof: so since the symbol is exported from B, it is present (not inherited) in B, so shadow has no effect
   ;; (22:13:28) stassats: attila_lendvai: you need to use the :shadow option in defpackage
   ;; (22:14:36) attila_lendvai: ooh. managed to code in cl for a decade without having a clue... thanks guys!
   ;; (22:14:54) stassats: yeah, the order is important
   ;; (22:14:56) specbot: http://www.lispworks.com/reference/HyperSpec/Body/m_defpkg.htm
   ;; (22:15:13) stassats: right before the Examples section it lists the order
   ))

;; We don't even want to :use COMMON-LISP here, to avoid any possible name clashes.
(uiop:define-package :hu.dwim.sdl/core (:use))
(uiop:define-package :hu.dwim.sdl/gfx (:use))
(uiop:define-package :hu.dwim.sdl/ttf (:use))
(uiop:define-package :hu.dwim.sdl/image (:use))
