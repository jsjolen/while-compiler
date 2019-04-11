(defpackage :lab1/test
  (:use :lab1 :fiveam :cl))
(in-package :lab1/test)

(defparameter *arit1*
  (.+ (.ref 'x) (.num 2)))
(defparameter *arit2*
  (.+ (.num 1) (.num 1)))
(defparameter *complex*
  (.comp
   (.set 'z (.num 0))
   (.while (.<= (.ref 'y) (.ref 'x))
	   (.comp
	    (.set 'z (.+ (.ref 'z) (.num 1)))
	    (.set 'x (.- (.ref 'x) (.ref 'y)))))))
(defparameter *try-catch*)
(def-suite lab1)
(in-suite lab1)

(defgeneric shape-equals? (x y)
  (:documentation
   "Equality over the shape of data."))
(defmethod shape-equals? ((x t) (y t))
  nil)
(defmethod shape-equals? ((x list) (y list))
    (reduce #'(lambda (x y) (and x y))
	    (mapcar #'shape-equals? x y)
	    :initial-value t))
(defmethod shape-equals? ((x integer) (y integer))
  (= x y))
(defmethod shape-equals? ((x symbol) (y symbol))
  (eq x y))


;; OK figure this shit out some other time.
;; Instead: Go ahead and define shape-equals? manually for every single PoS ADT, Jesus Fing Christ
;; We need fold for adts
#|
(defgeneric fold-map2 (f x y initial-value))
(defmethod fold-map2 (f (x t) (y t) initial-value)
  (error "Undefined fold-map2 for: ~A, ~A" (type-of x) (type-of y)))
(defmacro make-fold-map2 (adt)
  `(progn
     ,@(loop for (ctr arity) in (adt:get-cosntructors adt) collect
	    (progn
	      (let ((arity-sym1 (loop for i from 0 upto (1- arity) collect (gensym)))
		    (arity-sym2 (loop for i from 0 upto (1- arity) collect (gensym))))
		`(defmethod fold-map2 ((x ,ctr) (y ,ctr))
		   (adt:match ,adt x
		     ((,ctr ,@arity-sym1)
		      (adt:match ,adt y
				 ((,ctr ,@arity-sym2
					;; Wow figure it out
					)))))))))))
(defmacro make-shape-equality (adt)
  `(progn
     ,@(loop for (ctr _) in (adt:get-constructors adt) collect
	    (progn
	      `(defmethod shape-equals? ((x ,ctr) (y ,ctr))
		 (fold-map2 #'(lambda (a b i)
				(and (shape-equals? a b) i))
			    x y t))))))
|#
;; Ca tests
(test ca-test
  (is (equal (ca (.num 5))
	     (list (.push 5))))
  (is (equal (ca (.ref 'x))
	     (list (.fetch 'x))))
  (is (equal (ca (.+ (.num 1) (.num 2)))
	     (list (.push 2) (.push 1) .add))))
