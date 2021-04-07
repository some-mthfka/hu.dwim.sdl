(in-package :hu.dwim.sdl)

;; * Convenience functions for passed return values

;; see readme for details on this

;; No varargs support.  And this is not at its best, you shouldn't have to
;; specify any types yourself, but instead ask cffi what defcfun's arglist and
;; types are.  But I don't know how to do that.
(defmacro defun-with-passed-return-values (fn &rest desc)
  "Define a fn* which calls fn, where * in DESC says to leave argument as is,
and :typename says to pass a generated foreign object of the same type in its
place and, after the function is called, return it as a value.  The order of the
returned values is the same as they appear in the original arglist."
  (let ((arglist (second (function-lambda-expression (fdefinition fn)))))
    (assert (eql (length desc) (length arglist)))
    (loop for type-or-skip in desc
          for argname in arglist
          do (assert (or (equal (symbol-name type-or-skip) "*") (keywordp type-or-skip)))
          when (equal (symbol-name type-or-skip) "*")
            collect argname into new-arglist
          when (keywordp type-or-skip)
            collect (list argname type-or-skip) into foreign-objects
          finally (return
                    `(defun ,(symbolicate fn '*) ,new-arglist
                       (cffi:with-foreign-objects ,foreign-objects
                         (,fn ,@arglist)
                         (values ,@(mapcar (lambda (x) `(cffi:mem-aref ,@x)) foreign-objects))))))))


(defmacro defun-with-passed-return-values* (&body rest)
  `(progn ,@(loop for desc in rest
                  collect `(defun-with-passed-return-values ,@desc) ; progn keeps it a top level form
                  collect `(export ',(symbolicate (car desc) '*)))))

;; * Useful macros

(defmacro ignore-sdl-errors (&body body)
  "Wraps the body in a handler case, catching `sdl-error' and doing nothing about it."
  `(handler-case (progn ,@body)
     (sdl-error ())))

;; * Export

(defun ffi-name-export-predicate (symbol &key &allow-other-keys)
  (declare (ignore symbol))
  ;; unlike with `ffi-name-transformer', theres seems to be nothing like `kind' here...
  t)

;; * Prologue: import symbols from core

(defun prologue-from-core ()
  (unless (eql *package* (find-package :hu.dwim.sdl/core))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((skip-list (list (ffi-name-transformer "SDL_Quit" :function)
                              (ffi-name-transformer "SDL_Init" :function)
                              (ffi-name-transformer "SDL_WasInit" :function)
                              (ffi-name-transformer "SDL_PATCHLEVEL" :constant))))
         (do-symbols (symbol (find-package :hu.dwim.sdl/core))
           ;; to avoid function-pointer error, or if anything else like that
           ;; gets introduced before the prologue:
           (let ((name (symbol-name symbol)))
             (unless (or (find-symbol name *package*)
                         (member name skip-list :test #'equal))
               (import symbol)))))))) 

;; * Epilogue: unknown/questionable names warnings

(defun epilogue-unknown-names ()
  (prog1 (when *unknown-names*
           `(warn "The following functions are not specified in the type
conversion lists, so automatic error signaling for them will not work, and the
boolean values will not be automatically converted.  File an issue or add them
to the conversion lists yourself (and don't forget to remove the generated files
manually): ~a." ',*unknown-names*))
    (setf *unknown-names* nil))) ; reset for the next package

(defun epilogue-questionable-names ()
  (prog1 (when *questionable-names*
           `(warn "The following functions have questionable names, probably
because they contain unknown abbreviations.  File an issue or add exceptions to
`catch-questionable-names': ~a." ',*questionable-names*))
    (setf *questionable-names* nil)))

;; * make-<struct-name> and with-<struct-name>

;; Produce make-<struct-name> and with-<struct-name> macros for structs and for
;; types that point to those structs.  Anon structs are skipped (but not types
;; that point to them).  If there are no slots, with macros are skipped Keywords
;; are used in case the original definition or the ordering in it changes.

;; I don't know how to cajole the known info about the generated struct out of
;; cffi, so I resort to keeping track of this manually.
(defparameter *known-struct-defs* nil) ; No real need to setf to nil on every generation.

(defmacro def-make-struct-macro (name type-name slot-names)
  `(defmacro ,name (&key ,@slot-names)
     (with-gensyms (ptr)
       `(let ((,ptr (cffi:foreign-alloc ',',type-name)))
          (with-foreign-slots (,',slot-names ,ptr ,',type-name)
            (setf ,,@(loop for sn in slot-names appending `(',sn ,sn))))
          ,ptr))))

(defmacro def-with-struct-macro (name type-name slot-names)
  `(defmacro ,name ((name &key ,@slot-names) &body body)
     `(cffi:with-foreign-object (,name ',',type-name)
        (with-foreign-slots (,',slot-names ,name ,',type-name)
          (setf ,,@(loop for sn in slot-names appending `(',sn ,sn))))
        ,@body)))

(defun struct-info-from-defcstruct (form)
  (values
   ;; struct name
   (let ((name (first (second form))))
     (setf (getf *known-struct-defs* name) form)
     (unless (cl-ppcre:scan "ANON-STRUCT-" (symbol-name name))
       name))
   ;; name
   (first (second form))
   ;; slot-names
   (mapcar #'first (cddr form))))

(defun struct-info-from-defctype (form)
  (let ((typespec (third form)))
    (when (listp typespec)
      (values
       ;; struct name
       (when (eql :struct (first typespec))
         (second typespec))
       ;; name
       (second form)
       ;; stot names
       (mapcar #'first (cddr (getf *known-struct-defs* (second typespec))))))))

(defparameter *maybe* nil)

(defun maybe-generate-maker-and-with-forms (form)
  (when (member (first form) '(cffi:defctype cffi:defcstruct))
    (multiple-value-bind (struct-name name slot-names)
        (case (first form)
          (defcstruct (struct-info-from-defcstruct form))
          (defctype (struct-info-from-defctype form)))
      (when (and struct-name
                 ;; don't redefine struct of the same name:
                 (not (and (eql (first form) 'cffi:defctype) (eql name struct-name)))) 
        (let ((make-macro-name (symbolicate 'make- name))
              (with-macro-name (symbolicate 'with- name)))
          (list*
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (fmakunbound ',make-macro-name)
              (fmakunbound ',with-macro-name))
           `(def-make-struct-macro ,make-macro-name ,(list :struct struct-name) ,slot-names)
           `(export ',make-macro-name)
           (when slot-names 
             (list
              `(def-with-struct-macro ,with-macro-name ,(list :struct struct-name) ,slot-names)
              `(export ',with-macro-name)))))))))

#+nil
(cffi:foreign-free (hu.dwim.sdl/core:make-rect :x 0 :y 0 :w 10 :h 10))
#+nil
(hu.dwim.sdl/core:with-rect (data :x 0 :y 0 :w 10 :h 10) data)

;; * Callback setup
(ql:quickload 'ut)

(defun form-callback (form &key &allow-other-keys)
  (remove nil (maybe-generate-maker-and-with-forms form)))

(defun prologue-callback (&key &allow-other-keys)
  (remove nil (list (prologue-from-core))))

(defun epilogue-callback (&key &allow-other-keys)
  (remove nil (list (epilogue-unknown-names)
                    (epilogue-questionable-names))))

(defun callback-factory (&key &allow-other-keys)
  (values #'form-callback #'epilogue-callback #'prologue-callback))
