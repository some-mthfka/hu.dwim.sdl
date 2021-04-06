;;; This file is loaded before the generated FFI.

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

(define-condition sdl-error (simple-error) ())

(defun get+clear-sdl-error ()
  "Some functions, like `sdl-get-window-from-id', won't set sdl error message,
so you will see the previous one: so make sure to keep everything cleaned."
  (let ((err (hu.dwim.sdl/core::sdl-get-error)))
    (if (emptyp err)
        "/SDL_GetError() had no error message./"
        (prog1 (princ-to-string err)
          (hu.dwim.sdl/core::sdl-clear-error)))))

;; ** generation shared stuff

(defun get-new-type-name (kind function-name actual-type)
  (symbolicate function-name '/ kind '/
               (if (symbolp actual-type)
                   actual-type                    
                   (progn (assert (eql :pointer (first actual-type)))
                          (assert (eql (length actual-type) 2))
                          (symbolicate (second actual-type) '*)))))

(defun generate-condition-definer (condition-name)
  `(progn
     (define-condition ,condition-name (hu.dwim.sdl::sdl-error) ())
     (export '(,condition-name))))

(defun generate-custom-type-definer (actual-type custom-type)
  `(cffi:define-foreign-type ,custom-type (cffi::foreign-type-alias)
     ()
     (:default-initargs :actual-type (cffi::parse-type ',actual-type))
     (:simple-parser ,custom-type)))

(defmacro def-custom-type-setup-macro (name  &optional (error-checked t))
  `(defmacro ,(symbolicate 'custom-type-setup/ name)
       (original-function-name new-function-name actual-type custom-type)
     (let ((condition-name (symbolicate new-function-name '-error)))
       `(progn
          ,,(when error-checked
              ``,(generate-condition-definer condition-name))
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

(def-custom-type-setup-macro null-checked)

(def-type-conversion-processor null-checked)

;; ** enum values that signal errors

#+nil
(hu.dwim.sdl/core:sdl-get-scancode-from-name "A") ; should return an integer
#+nil
(hu.dwim.sdl/core:sdl-get-scancode-from-name "AA") ; shoudl signal a condition

(defun generate-type-expand-defmethod/enum-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     ;; Enumerations can either be signed or unsigned, shouldn't convert here
     ;; probably.  And the actual type is an enum, which is not right to convert
     ;; to either (yields a symbol).
     `(let ((return-value ,value))
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
                                         (cffi:convert-from-foreign return-value ',',actual-type) ; enum symbol
                                         ',',actual-type)))
        return-value)))

(def-custom-type-setup-macro enum-checked)

(def-type-conversion-processor enum-checked)

;; ** string starts that that signal errors

#+nil
(hu.dwim.sdl/core:sdl-get-platform) ; "Linux"
#+nil
(hu.dwim.sdl/core:sdl-get-pixel-format-name 0) ; error

(defun generate-type-expand-defmethod/checked-string-failure
    (actual-type custom-type original-function-name new-function-name condition-name)
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value (cffi:convert-from-foreign ,value ',',actual-type)))
        (when (and (not (cffi:null-pointer-p return-value))
                   (alexandria:starts-with-subseq ,,(second (assoc original-function-name
                                                                   *return-string-on-failure/all*
                                                                   :test #'equal))
                                                  return-value))
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(def-custom-type-setup-macro checked-string-failure)

(def-type-conversion-processor checked-string-failure)

;; ** constant checked

#+nil
(hu.dwim.sdl/core:sdl-get-window-id (cffi:null-pointer))
#+nil
(hu.dwim.sdl/core:sdl-tls-create) ; works
#+nil
(hu.dwim.sdl/ttf:ttf-was-init)

(defun generate-type-expand-defmethod/constant-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value (cffi:convert-from-foreign ,value ',',actual-type))) ; constants may be of any time
        (when (eql return-value
                   ;; bake in the value of the constant:
                   ,,(second (assoc original-function-name
                                    *return-constant-on-failure/all*
                                    :test #'equal)))
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(def-custom-type-setup-macro constant-checked)

(def-type-conversion-processor constant-checked)

;; ** negative return code error

#+nil
(hu.dwim.sdl/core:sdl-get-display-mode 0 1 (cffi:null-pointer))

(defun generate-type-expand-defmethod/negative-checked
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     ;; NOTE: strictly speaking it should be (cffi:convert-from-foreign ,value :int), but not in this case.
     `(let ((return-value ,value))
        (when (< return-value 0)
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(def-custom-type-setup-macro negative-checked)

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
     `(not (eql ,value 0)))) ; we check non SDL_bools here as well, so can't simply use +true+

(def-custom-type-setup-macro bool-conversion nil)

(def-type-conversion-processor bool-conversion)

;; ** booleans (where SDL_FALSE means an error)

#+nil
(hu.dwim.sdl/core:sdl-remove-timer 0)

(defun generate-type-expand-defmethod/checked-bool-conversion
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value (not (eql ,value 0)))) ; SDL_FALSE is 0 in the sources
        (unless return-value 
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        return-value)))

(def-custom-type-setup-macro checked-bool-conversion)

(def-type-conversion-processor checked-bool-conversion)

;; ** bool-like integers with conversion and negative return on errors

#+nil
(hu.dwim.sdl/core:sdl-joystick-is-haptic (cffi:null-pointer))

(defun generate-type-expand-defmethod/checked-bool-like-negative-failure
    (actual-type custom-type original-function-name new-function-name condition-name)
  (declare (ignore original-function-name))
  `(defmethod cffi:expand-from-foreign (value (type ,custom-type))
     `(let ((return-value ,value)) ; conversion is probably not necessary
        (when (< return-value 0)
          (error ',',condition-name
                 :format-control "SDL call failed: ~S.~%~%~a returned ~a (of type ~a)."
                 :format-arguments (list (get+clear-sdl-error) ,',new-function-name
                                         return-value ',',actual-type)))
        (not (eql return-value 0)))))

(def-custom-type-setup-macro checked-bool-like-negative-failure)

(def-type-conversion-processor checked-bool-like-negative-failure)

;; * type transformer

(defun ffi-type-transformer (type-specifier context &rest args &key &allow-other-keys)
  (let ((type-specifier (apply 'cffi/c2ffi:default-ffi-type-transformer
                               type-specifier context args))
        (name (when (consp context) (second context))))
    (when (and (eql (first context) :function)
               (eql (third context) :return-type))
      ;; (when (eql type-specifier :void) (push name *void-stuff*))
      (catch-unknown-names name))
    (flet ((convert-p (conversion-list &optional &key (check-type nil) (key #'identity))
             (when (and name
                        (eql (first context) :function)
                        (eql (third context) :return-type)
                        (member name conversion-list :test 'equal :key key))
               (if check-type (assert (member (make-keyword type-specifier) check-type)))
               t)))
      (cond
        ((convert-p *return-null-on-failure/all*)
         (process/null-checked name type-specifier))
        ((convert-p *return-enum-check-invalid/all* :key #'first)
         (process/enum-checked name type-specifier))
        ((convert-p *return-constant-on-failure/all* :key #'first)
         (process/constant-checked name type-specifier))
        ((convert-p *return-negative-on-failure/all*)
         (process/negative-checked name type-specifier))
        ((convert-p *return-boolean-no-errors/all*)
         (process/bool-conversion name type-specifier))
        ((convert-p *return-boolean-check-errors/all*)
         (process/checked-bool-conversion name type-specifier))
        ((convert-p *return-bool-like-0-for-false-negative-for-errors/all*)
         (process/checked-bool-like-negative-failure name type-specifier))
        ((convert-p *return-string-on-failure/all* :key #'first)
         (process/checked-string-failure name type-specifier))
        (t type-specifier)))))
