(in-package :hu.dwim.sdl)

;; * Convenience functions for passed return values

;; see readme for details on this

(defmacro defun-with-passed-return-values (fn* fn suppress-original-return arglist desc)
  "Define a fn* which calls fn, where * in DESC says to leave argument as is,
and anything else (a type specification like :int or (:struct smth)) says to
pass a generated foreign object of the type in its place and, after the function
is called, return it as a value (the :pointer is ommited from the type info for
these). If SUPPRESS-ORIGINAL-RETURN is t, what the function returns is
discarded, otherwise returned as the first value.  The order of the returned
values is the same as they appear in the original arglist. ARGLIST is for the
slot specification as it appears in defcfun. You shouldn't use this macro
manually."
  (loop with argnames = (mapcar #'first arglist)
        for argname in argnames
        for argtype in (mapcar #'second arglist)
        for designator in (mapcar #'symbol-name desc)
        do (assert (member designator '("*" "-") :test #'equal))
        when (equal designator "*")
          collect argname into new-arglist
        else collect (list argname `',(second argtype)) into foreign-objects ; omit :pointer
             and do (assert (eql :pointer (first argtype)))
        finally (return
                  `(defun ,fn* ,new-arglist
                     (cffi:with-foreign-objects ,foreign-objects
                       ,(if suppress-original-return `(,fn ,@argnames))
                       (values
                        ,@(unless suppress-original-return `((,fn ,@argnames)))
                        ,@(mapcar (lambda (x) `(cffi:mem-ref ,@x)) foreign-objects)))))))

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
    ;; import everything from core (to the current package), because other
    ;; packages rely on its definitions
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((skip-list (list (ffi-name-transformer "SDL_Quit" :function)
                              (ffi-name-transformer "SDL_Init" :function)
                              (ffi-name-transformer "SDL_WasInit" :function)
                              (ffi-name-transformer "SDL_LoadBMP_RW" :function)
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

(defun sym (name exclude)
  (if (member name exclude) (gensym (symbol-name name)) name))

(ensure-symbol 'a (find-package :hu.dwim.sdl/core))

;; name and slot-names are for error checking
(defun scan-keys (supplied-keys name slot-names optional-p)
  (loop for (key value) on supplied-keys by #'cddr
        for key* = (ensure-symbol key (symbol-package (first slot-names)))
        collect key* into supplied-slots
        collect (list key* value) into key-value-pairs
        finally (progn
                  (unless optional-p ; supplied keys check
                    (let ((diff (set-difference slot-names supplied-slots)))
                      (when diff (error (concat "These keys weren't supplied: ~a. "
                                                "Use ~a if you don't want this check.")
                                        diff (regex-replace "^WITH-"
                                                            (regex-replace
                                                             "^MAKE-" (symbol-name name) "MAKE*")
                                                            "WITH*")))))
                  (return (values supplied-slots key-value-pairs)))))

(defmacro def-make-struct-macro (name type-name slot-names optional-p)
  (with-gensyms (ptr supplied-slots pairs)
    (let ((rest-sym (sym '_ slot-names)))
      `(defmacro ,name (&rest ,rest-sym &key ,@slot-names)
         (declare (ignorable ,@slot-names)) ; we access them through ,rest-sym
         (multiple-value-bind (,supplied-slots ,pairs)
             (scan-keys ,rest-sym ',name ',slot-names ,optional-p)
           `(let ((,',ptr (cffi:foreign-alloc ',',type-name)))
              (with-foreign-slots (,,supplied-slots ,',ptr ,',type-name)
                (setf ,@(loop for (sn sv) in ,pairs
                              appending `(,sn ,sv))))
              ,',ptr))))))

(defmacro def-with-struct-macro (name type-name slot-names optional-p)
  (with-gensyms (supplied-slots pairs)
    ;; we are making no assumptions about the slot names here
    (let ((var-sym (sym 'var slot-names))
          (rest-sym (sym '_ slot-names))
          (body-sym (sym 'body slot-names)))
      `(defmacro ,name ((,var-sym &rest ,rest-sym &key ,@slot-names) &body ,body-sym)
         (declare (ignorable ,@slot-names)) ; we access them through ,rest-sym
         (multiple-value-bind (,supplied-slots ,pairs)
             (scan-keys ,rest-sym ',name ',slot-names ,optional-p)
           `(cffi:with-foreign-object (,,var-sym ',',type-name)
              ,(when ,supplied-slots
                 `(with-foreign-slots (,,supplied-slots ,,var-sym ,',type-name)
                    (setf ,@(loop for (sn sv) in ,pairs
                                  appending `(,sn ,sv)))))
              nil
              ,@,body-sym))))))

(defmacro def-multi-with-macro (macro-name* macro-name slot-names)
  (let ((var-sym (sym 'var slot-names))
        (rest-sym (sym '_ slot-names))
        (other-bindings-sym (sym 'rest-of-bindings slot-names))
        (body-sym (sym 'body slot-names)))
    `(defmacro ,macro-name* (((,var-sym &rest ,rest-sym &key ,@slot-names)
                              &rest ,other-bindings-sym)
                             &body ,body-sym)
       (declare (ignorable ,@slot-names))
       `(,',macro-name (,,var-sym ,@,rest-sym)
                       ,@(if rest-of-bindings
                             `((,',macro-name* (,@rest-of-bindings) ,@,body-sym))
                             ,body-sym)))))

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

(defun maybe-generate-maker-and-with-macros (form)
  (when (and (member (first form) '(cffi:defctype cffi:defcstruct))
             ;; avoiding redefinition warnings (maybe build a dedicated list for this):
             (not (tree-equal form '(cffi:defcstruct (hu.dwim.sdl/core::audio-cvt :size 0))
                              :test #'equal)))
    (multiple-value-bind (struct-name name slot-names)
        (case (first form)
          (defcstruct (struct-info-from-defcstruct form))
          (defctype (struct-info-from-defctype form)))
      (when (and struct-name
                 ;; don't redefine struct of the same name:
                 (not (and (eql (first form) 'cffi:defctype) (eql name struct-name)))) 
        (let* ((make-macro-name (symbolicate 'make- name))
               (make*macro-name (symbolicate 'make* name))
               (with-macro-name (symbolicate 'with- name))
               (with-macro-name* (symbolicate with-macro-name '*))
               (with*macro-name (symbolicate 'with* name))
               (with*macro-name* (symbolicate with*macro-name '*))
               (all-macros (list make-macro-name make*macro-name
                                 with-macro-name with-macro-name*
                                 with*macro-name with*macro-name*)))
          (list
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              (mapcar #'fmakunbound ',all-macros)) ; avoid redefinition warnings
           `(def-make-struct-macro ,make-macro-name ,(list :struct struct-name) ,slot-names nil)
           `(def-make-struct-macro ,make*macro-name ,(list :struct struct-name) ,slot-names t)
           `(def-with-struct-macro ,with-macro-name ,(list :struct struct-name) ,slot-names nil)
           `(def-multi-with-macro ,with-macro-name* ,with-macro-name ,slot-names)
           `(def-with-struct-macro ,with*macro-name ,(list :struct struct-name) ,slot-names t)
           `(def-multi-with-macro ,with*macro-name* ,with*macro-name ,slot-names)
           `(export ',all-macros)))))))

(defun resolve-type-spec (type-spec package)
  (flet ((ensure (x) (or (find-symbol (symbol-name x) package)
                         (error "Symbol ~a was not found in ~a." x package))))
    (cond ((keywordp type-spec) type-spec)
          ((symbolp type-spec) (ensure type-spec))
          ((listp type-spec) (mapcar (rcurry #'resolve-type-spec package) type-spec))
          (t (error "Don't know how to place ~a in a package." type-spec)))))

;; we have shadowed cl version of with-slots to allow sdl:with-slots
(defmacro with-sdl-slots ((vars ptr type) &body body)
  (let ((type* (if (listp type) (second type) type)))
    (flet ((type-in-p (package) (when (find-symbol (symbol-name type*)
                                                   (find-package package))
                                  (find-package package))))
      (let* ((package (or (type-in-p :hu.dwim.sdl/core)
                          (type-in-p :hu.dwim.sdl/ttf)
                          (type-in-p :hu.dwim.sdl/gfx)
                          (type-in-p :hu.dwim.sdl/image)
                          (error "Type ~a was not found in any sdl package" type)))
             (native-vars (mapcar (lambda (var)
                                    (list var (ensure-symbol var package)))
                                  vars)))
        `(cffi:with-foreign-slots (,(mapcar #'second native-vars)
                                   ,ptr ,(resolve-type-spec type package))
           (symbol-macrolet (,@native-vars)
             ,@body))))))

#+nil
(hu.dwim.sdl/core:with-point (a :x 0 :y 5)
  (hu.dwim.sdl::with-sdl-slots ((x y) a (:struct rect))
    (values x y)))
#+nil
(hu.dwim.sdl/core:with-rect (clip-rect :x 0 :y 0 :w 10 :h 10)
  (hu.dwim.sdl/core:with-point* ((a :x 2 :y 2)
                                 (b :x 7 :y 7))
    (let* ((type '(:struct hu.dwim.sdl/core:point))
           (points (foreign-alloc type :count 2)))
      (setf (mem-aref points type 0) (mem-ref a type)
            (mem-aref points type 1) (mem-ref b type))
      (hu.dwim.sdl/core:with*rect (result) ; `with*' because we don't want to pass some arguments (all in this case)
        (hu.dwim.sdl/core:enclose-points points 2 clip-rect result)
        (mem-ref result '(:struct hu.dwim.sdl/core:rect))))))
#+nil
(cffi:foreign-free (hu.dwim.sdl/core:make-rect :x 0 :y 0 :w 10 :h 10))
#+nil
(hu.dwim.sdl/core:with-rect (data :x 0 :y 0 :w 10 :h 10) data)

;; * Passed return values macro generation

(defun maybe-generate-passed-return-value-macro (form)
  (when (eql 'cffi:defcfun (first form))
    (let* ((c-name (first (second form)))
           (name (second (second form)))
           (name* (symbolicate name '*))
           (desc-suppress-return
             (assoc c-name *auto-passed-return-value+suppress-return/all* :test #'equal))
           (desc-pass-return
             (assoc c-name *auto-passed-return-value+pass-return/all* :test #'equal))
           (desc (or desc-suppress-return desc-pass-return)))
      (when desc
        (list
         `(fmakunbound ',name*)
         `(hu.dwim.sdl::defun-with-passed-return-values ,name*
            ,name         
            ,(when desc-suppress-return t)
            ,(cdddr form)
            ,(rest desc))
         `(export ',name*))))))

;; * Callback setup

(defun form-callback (form &key &allow-other-keys)
  (remove nil (append (maybe-generate-passed-return-value-macro form)
                      (maybe-generate-maker-and-with-macros form))))

(defun prologue-callback (&key &allow-other-keys)
  (remove nil (list (prologue-from-core))))

(defun epilogue-callback (&key &allow-other-keys)
  (remove nil (list (epilogue-unknown-names)
                    (epilogue-questionable-names))))

(defun callback-factory (&key &allow-other-keys)
  (values #'form-callback #'epilogue-callback #'prologue-callback))
