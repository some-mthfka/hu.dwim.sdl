;;; This file is loaded before the generated FFI.

;; TODO export all error conditions

(in-package :hu.dwim.sdl)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; * custom return types

;; ** commentary

;; Say we want to automatically check the returned value of "SDL_CreateWindow"
;; for NULL and throw a condition.  That's what defining custom return types is
;; gonna do for us, and that we do with `cffi:define-foreign-type'.  As such, it
;; requires us to specify the actual type (which, in the case of window
;; creation, is "SDL_Window *").

;; ** preliminaries

(define-condition sdl-error (error) ())

(defun get+clear-sdl-error ()
  "Some functions, like `sdl-get-window-from-id', won't set sdl error message,
so you will see the previous one: so make sure to keep everything cleaned."
  (let ((err (hu.dwim.sdl/core::sdl-get-error)))
    (if (emptyp err)
        "/SDL_GetError() had no error message./"
        (prog1 (princ-to-string err)
          (hu.dwim.sdl/core::sdl-clear-error)))))

;; ** generation shared stuff

(defmacro define-sdl-condition (name-postfix)
  `(define-condition ,(symbolicate 'sdl-error/ name-postfix) (simple-error sdl-error) ()))

(defun get-new-type-name (kind function-name actual-type)
  (symbolicate function-name '/ kind '/
               (if (symbolp actual-type)
                   actual-type                    
                   (progn (assert (eql :pointer (first actual-type)))
                          (assert (eql (length actual-type) 2))
                          (symbolicate (second actual-type) '*)))))

(defun generate-condition-definer (condition-name condition-name-postfix)
  (when condition-name-postfix
    `(define-condition ,condition-name
         (,(ensure-symbol (symbolicate 'sdl-error/ condition-name-postfix)
                          (find-package :hu.dwim.sdl))) ())))

(defun generate-custom-type-definer (actual-type custom-type)
  `(cffi:define-foreign-type ,custom-type (cffi::foreign-type-alias)
     ()
     (:default-initargs :actual-type (cffi::parse-type ',actual-type))
     (:simple-parser ,custom-type)))

(defmacro def-custom-type-setup-macro (name condition-name-postfix)
  `(defmacro ,(symbolicate 'custom-type-setup/ name)
       (original-function-name new-function-name actual-type custom-type)
     (let ((condition-name (symbolicate new-function-name '-error)))
       `(progn
          ,(generate-condition-definer condition-name ',condition-name-postfix)
          ,(generate-custom-type-definer actual-type custom-type)
          ,(,(symbolicate 'generate-type-expand-defmethod/ name)
            actual-type custom-type original-function-name new-function-name condition-name)))))

(defmacro def-type-conversion-processor (name)
  `(defun ,(symbolicate 'process/ name) (original-function-name actual-type)
     (let* ((new-function-name (ffi-name-transformer original-function-name :function))
            (custom-type (get-new-type-name new-function-name ',name actual-type)))
       ;; so glad I came up with this hack instead of prepending to a file ( ͡° ͜ʖ ͡°)
       (cffi/c2ffi::output/code
        `(,',(ensure-symbol (symbolicate 'custom-type-setup '/ name)
                            (find-package :hu.dwim.sdl))
          ,original-function-name ,new-function-name ,actual-type ,custom-type))
       custom-type)))

;; ** null pointer returned is error

#+nil
(hu.dwim.sdl/core:sdl-haptic-open-from-mouse)
#+or
(hu.dwim.sdl/core:sdl-get-window-from-id 43434)

(defun generate-type-expand-defmethod/null-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value (cffi:convert-from-foreign ,value ',',actual-type)))
        (when (cffi:null-pointer-p return-value)
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned NULL (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         ',',actual-type)))
        return-value)))

(define-sdl-condition null-returned)

(def-custom-type-setup-macro null-checked null-returned)

(def-type-conversion-processor null-checked)

;; ** enum values that signal errors

#+nil
(hu.dwim.sdl/core:sdl-get-scancode-from-name "A") ; should return 4
#+nil
(hu.dwim.sdl/core:sdl-get-scancode-from-name "AA") ; error

(defun generate-type-expand-defmethod/enum-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     ;; Enumerations can either be signed or unsigned, shouldn't convert here
     ;; probably.  And the actual type is an enum, which is nor right to convert
     ;; to either (yields a symbol, I am not sure how that works exactly).
     `(let ((return-value ,value)) ; (cffi:convert-from-foreign ,value ',',actual-type)
        (when (eql return-value
                   ;; bake the value of the constant right in here, not even the symbol:
                   ,,(symbolicate (ffi-name-transformer
                                   (second (assoc original-function-name
                                                  *return-enum-check-invalid/all*
                                                  :test #'equal))
                                   :constant)))
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(define-sdl-condition enum-invalid-code)

(def-custom-type-setup-macro enum-checked enum-invalid-code)

(def-type-conversion-processor enum-checked)

;; ** negative return code error

#+nil
(hu.dwim.sdl/core:sdl-get-display-mode 0 1 (cffi:null-pointer))

(defun generate-type-expand-defmethod/negative-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  ;; NOTE: strictly speaking it should be (cffi:convert-from-foreign ,value :int), but not in this case.
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value (cffi:convert-from-foreign ,value ',',actual-type)))
        (when (< return-value 0)
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(define-sdl-condition negative-return-code)

(def-custom-type-setup-macro negative-checked negative-return-code)

(def-type-conversion-processor negative-checked)

;; ** bool conversion (no error checking)

#+nil
(hu.dwim.sdl/core:sdl-has-3dnow)
#+nil
(hu.dwim.sdl/core:sdl-has-sse)

(defun generate-type-expand-defmethod/bool-conversion
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore actual-type original-function-name new-function-name condition-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(eql ,value hu.dwim.sdl/core::+true+)))

(def-custom-type-setup-macro bool-conversion nil)

(def-type-conversion-processor bool-conversion)

;; ** booleans (where SDL_FALSE means an error)

#+nil
(hu.dwim.sdl/core:sdl-remove-timer 0)

(defun generate-type-expand-defmethod/checked-bool-conversion
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value ,value))
        (when (eql return-value hu.dwim.sdl/core::+false+)
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        t))) ; assumes +TRUE+ is the only other option besides +FALSE+

(define-sdl-condition false-returned)

(def-custom-type-setup-macro checked-bool-conversion false-returned)

(def-type-conversion-processor checked-bool-conversion)

;; * type transformer

(defparameter *string-thing* nil)
(defparameter *bool-thing* nil)
(defparameter *bool-thing2* nil) ; SDL_BOOL
(defparameter *negative-error-types* nil) ;  => (SDL-JOYSTICK-ID SDL-AUDIO-DEVICE-ID :INT)

(defparameter *null-checked-function-info* nil)

(defparameter *custom-type-code* nil)

(defun ffi-type-transformer (type-specifier context &rest args &key &allow-other-keys)
  (let ((type-specifier (apply 'cffi/c2ffi:default-ffi-type-transformer
                               type-specifier context args))
        (name (when (consp context) (second context))))
    (when (and (eql (first context) :function)
               (eql (third context) :return-type))
      (catch-unknown-names name)
      (when (eql type-specifier 'sdl-bool)
        (push name *bool-thing*)))
    (flet ((convert-p (conversion-list &optional &key (check-type nil) (key #'identity))
             (when (and name
                        (eql (first context) :function)
                        (eql (third context) :return-type)
                        (member name conversion-list :test 'equal :key key))
               (if check-type (assert (member (make-keyword type-specifier) check-type)))
               t)))
      ;; let ((*package* (find-package :hu.dwim.sdl))) ; for correct type name generation
      (cond
        ((convert-p *return-null-on-failure/all*)
         (process/null-checked name type-specifier))
        ((convert-p *return-enum-check-invalid/all* :key #'first)
         (process/enum-checked name type-specifier))
        ((convert-p *negative-returned-error-list/core*)
         (process/negative-checked name type-specifier))
        ((convert-p *return-boolean-no-errors/all*)
         (process/bool-conversion name type-specifier))
        ((convert-p *return-boolean-check-errors/all*)
         (process/checked-bool-conversion name type-specifier))
        (t type-specifier)))))
