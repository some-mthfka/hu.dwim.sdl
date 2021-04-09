(in-package :hu.dwim.sdl)

;; * Convenience functions for passed return values

;; see readme for details on this

;; No varargs support.  And this is not at its best, you shouldn't have to
;; specify any types yourself, but instead ask cffi what defcfun's arglist and
;; types are.  But I don't know how to do that.
(defmacro defun-with-passed-return-values (suppress-original-return fn &rest desc)
  "Define a fn* which calls fn, where * in DESC says to leave argument as is,
and anything else (a type specification like :int or (:struct smth)) says to
pass a generated foreign object of the type in its place and, after the function
is called, return it as a value.  If SUPPRESS-ORIGINAL-RETURN is t, what the
function returns is discarded, otherwise returned as the first value.  The order
of the returned values is the same as they appear in the original arglist."
  (let ((arglist (second (function-lambda-expression (fdefinition fn)))))
    (assert (eql (length desc) (length arglist)))
    (loop for type-spec in desc
          for argname in arglist
          when (equal (symbol-name type-spec) "*")
            collect argname into new-arglist
          else
            collect (list argname `',type-spec) into foreign-objects
          finally (return
                    `(defun ,(symbolicate fn '*) ,new-arglist
                       (cffi:with-foreign-objects ,foreign-objects
                         ,(if suppress-original-return `(,fn ,@arglist))
                         (values
                          ,@(unless suppress-original-return `((,fn ,@arglist)))
                          ,@(mapcar (lambda (x) `(cffi:mem-aref ,@x)) foreign-objects))))))))

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

;; TODO Add to readme: note that the order that you supply the keys doesn't
;; matter, it is always evaluated as they are declared in the struct, so it's
;; best not to rely on it in case it changes. I don't think there's ordering
;; information that can be extracted about the supplied keys, so that's that.

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
                  (print supplied-slots)
                  (print slot-names)
                  (print *package*)
                  (unless optional-p ; error check
                    (let ((diff (set-difference slot-names supplied-slots)))
                      (when diff (error (concat "These keys weren't supplied: ~a. "
                                                "Use ~a if you don't want this check.")
                                        diff (regex-replace "^WITH-"
                                                            (regex-replace
                                                             "^MAKE-" (symbol-name name) "MAKE*")
                                                            "WITH*")))))
                  (return (values supplied-slots key-value-pairs)))))

;; (hu.dwim.sdl/core:with-rect* ((a :y 1 :x 0 :h 3 :w 8)
;;                               (b :x 1 :y 0 :h 3 :w 7))
;;   (values a b))

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

;; (def-make-struct-macro make-rect rect (x y w h) nil)
;; (def-make-struct-macro make-rect* rect (x y w h) t)

;; (make*rect :y 0 :w 54)
;; (make-rect :y 0 :w 54 :h 88 :x 20)

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

;; (def-multi-with-macro with-rect* with-rect (x y w h))

;; (hu.dwim.sdl/core:with-rect (a :x 0 :y 0 :w 0 :h 44)
;;   (hu.dwim.sdl/core:with-rect (b :x 0 :y 0 :w 0 :h 1)
;;     (values a b)))

;; (hu.dwim.sdl/core:with-rect* ((a :x 0 :y 0 :w 0 :h 44)
;;                               (b :x 0 :y 0 :w 0 :h 1))
;;   (values a b))

;; (def-with-struct-macro with-rect rect (x y w h) nil)
;; (def-with-struct-macro with*-rect rect (x y w h) t)

;; (with-rect (r :h 5 :x 0 :y 555 :w 11)
;;   (print "shit"))

;; (def-multi-with-macro with-rect* with-rect (x y w h) nil)

;; (def-multi-with-macro with*-rect* with*-rect (x y w) nil)

;; (with*-rect* ((r1 :x 0 :y 1)))

;; (with-rect (r :x (1+ x) :w 11 :y 1) (print r))

;; (def-with-struct-macro with-rect rect (x y w))

;; (with-rect (r :y 0 :w 54) )

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
  (when (member (first form) '(cffi:defctype cffi:defcstruct))
    (multiple-value-bind (struct-name name slot-names)
        (case (first form)
          (defcstruct (struct-info-from-defcstruct form))
          (defctype (struct-info-from-defctype form)))
      (when (and struct-name
                 ;; don't redefine struct of the same name:
                 (not (and (eql (first form) 'cffi:defctype) (eql name struct-name)))) 
        (let* ((make-macro-name (symbolicate 'make- name))
               (with-macro-name (symbolicate 'with- name))
               (with-macro-name* (symbolicate with-macro-name '*))
               (with*-macro-name (symbolicate 'with*- name))
               (with*-macro-name* (symbolicate with*-macro-name '*))
               (all-macros (list make-macro-name
                                 with-macro-name with-macro-name*
                                 with*-macro-name with*-macro-name*)))
          (list
           `(eval-when (:compile-toplevel :load-toplevel :execute) ; avoid redefinition warnings:
              (mapcar #'fmakunbound ',all-macros))
           `(def-make-struct-macro ,make-macro-name ,(list :struct struct-name) ,slot-names nil)
           `(def-with-struct-macro ,with-macro-name ,(list :struct struct-name) ,slot-names nil)
           `(def-multi-with-macro ,with-macro-name* ,with-macro-name ,slot-names)
           `(def-with-struct-macro ,with*-macro-name ,(list :struct struct-name) ,slot-names t)
           `(def-multi-with-macro ,with*-macro-name* ,with*-macro-name ,slot-names)
           `(export ',all-macros)))))))

(find-symbol (symbol-name 'rect) (find-package :hu.dwim.sdl/core))

(defun resolve-type-spec (type-spec package)
  (flet ((ensure (x) (or (find-symbol (symbol-name x) package)
                         (error "Symbol ~a was not found in ~a." x package))))
    (cond ((keywordp type-spec) type-spec)
          ((symbolp type-spec) (ensure type-spec))
          ((listp type-spec) (mapcar (rcurry #'resolve-type-spec package) type-spec))
          (t (error "Don't know how to place ~a in a package." type-spec)))))

(resolve-type-spec '(:struct point) (find-package :hu.dwim.sdl/core))

;; we have shadowed cl version of with-slots to allow sdl:with-slots
(defmacro with-sdl-slots ((vars ptr type) &body body)
  (let ((type* (if (listp type) (second type) type)))
    (print type*)
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

;; (hu.dwim.sdl/core:with-point (a :y 0 :x 5)
;;   (hu.dwim.sdl::with-sdl-slots ((x y) a (:struct rect))
;;     (values y x)))

;; (defun-with-passed-return-values nil hu.dwim.sdl/core:enclose-points * * * hu.dwim.sdl/core:rect)

;; (cffi:with-foreign-objects ((x 'hu.dwim.sdl/core:rect))
;;   x)

;; (hu.dwim.sdl/core:with-rect (clip-rect :x 0 :y 0 :w 10 :h 10)
;;   (hu.dwim.sdl/core:with-point* ((a :x 2 :y 2)
;;                                  (b :x 7 :y 7))
;;     (let* ((type '(:struct hu.dwim.sdl/core:point))
;;            (ptr (foreign-alloc type :count 2)))
;;       (setf (mem-aref ptr type 0) (mem-ref a type)
;;             (mem-aref ptr type 1) (mem-ref b type))
;;       (multiple-value-bind (result enclosing-rect)
;;           (enclose-points* ptr 2 clip-rect)
;;         (values result (mem-ref enclosing-rect '(:struct hu.dwim.sdl/core:rect)))))))

;; (cffi:with-pointer-to-vector-data (points (make-array 2 :element-type 'fixnum :initial-contents (list 1 2)))
;;   points)

;; (make-array 5 :initial-contents (list 1 2 3 4 5))

#+nil
(cffi:foreign-free (hu.dwim.sdl/core:make-rect :x 0 :y 0 :w 10 :h 10))
#+nil
(hu.dwim.sdl/core:with-rect (data :x 0 :y 0 :w 10 :h 10) data)

;; (ql:quickload 'ut)
;; (ut:def-streamlined-macro )

;; (import '(with-rect) (find-package :hu.dwim.sdl/core))

;; (defmacro with-rect* (((name &key x y w h) &rest rest-of-bindings) &body body)
;;   `(with-rect (,name :x ,x :y ,y :w ,w :h ,h)
;;      ,@(if rest-of-bindings
;;            `((with-rect* (,@rest-of-bindings) ,@body))
;;            body)))

;; (def-multi-macro make-rect* make-rect (x y w h))

;; (hu.dwim.sdl/core:with-rect* ((a :x 0 :y 0 :w 0)
;;                               (b :x 0 :y 0 :w 0 :h 1))
;;   (values a b))

;; (defmacro m (&rest args &key a b) args)

;; (m :b 4 :a 0)

;; (defun fn (&key a) a)

;; (fn :a 0)

;; (hu.dwim.sdl/core:with-rect (r :x 0 :y 0 :w 0)
;;   a)

;; * Callback setup

(defun form-callback (form &key &allow-other-keys)
  (remove nil (maybe-generate-maker-and-with-macros form)))

(defun prologue-callback (&key &allow-other-keys)
  (remove nil (list (prologue-from-core))))

(defun epilogue-callback (&key &allow-other-keys)
  (remove nil (list (epilogue-unknown-names)
                    (epilogue-questionable-names))))

(defun callback-factory (&key &allow-other-keys)
  (values #'form-callback #'epilogue-callback #'prologue-callback))
