(in-package :lab1)
;;;;; AST

;; While
;; Convention: Prepend . to all constructors to avoid name-clashes with CL
;; AST for While
(defdata aexp
  (.ref symbol)
  (.num integer)
  (.+ aexp aexp)
  (.* aexp aexp)
  (./ aexp aexp)
  (.- aexp aexp))
(defdata bexp
  .tt ;; Yeah yeah, I know this isn't the semantic true, but we want to
  .ff ;; disambiguate with `inst`'s true/false below
  (.= aexp aexp)
  (.<= aexp aexp)
  (.~ bexp)
  (.^ bexp bexp))
(defdata stm
  .skip
  (.if bexp stm stm)
  (.while bexp stm)
  (.set symbol aexp)
  (.comp stm stm)
  (.try-catch stm stm))
(export '(aexp bexp stm .skip .if .while .set .comp .tt .ff .= .<= .~ .^ .ref .num .+ .* .- ./ .try-catch))

;; AST for AM
(defdata inst
  (.push t)
  .add .mult .sub .div
  .true .false .eq .le .and .neg
  (.fetch t)
  (.store t)
  .noop
  (.protect list) ;; list of inst
  (.branch list list) (.loop list list)) ;; list of inst
;;;;; Compiler

;; We're utilizing the LIST/APPEND pattern
;; to create our program.

(defun ca (aexp)
  "Returns (list inst)"
  (match aexp aexp
	 ((.num n) (list (.push n)))
	 ((.ref sym) (list (.fetch sym)))
	 ((.+ ae1 ae2) (append (ca ae2) (ca ae1) (list .add)))
	 ((.* ae1 ae2) (append (ca ae2) (ca ae1) (list .mult)))
	 ((.- ae1 ae2) (append (ca ae2) (ca ae1) (list .sub)))
	 ((./ ae1 ae2) (append (ca ae2) (ca ae1) (list .div)))))
(defun cb (bexp)
  "Returns (list inst)"
  (match bexp bexp
	 (.tt (list .true))
	 (.ff (list .false))
	 ((.= ae1 ae2) (append (ca ae2) (ca ae1) (list .eq)))
	 ((.<= ae1 ae2) (append (ca ae2) (ca ae1) (list .le)))
	 ((.~ be1) (append (cb be1) (list .neg)))
	 ((.^ be1 be2) (append (cb be2) (cb be1) (list .and)))))
(defun cs (stm)
  "Returns (list inst)"
  (match stm stm
	 ((.set sym ae1) (append (ca ae1) (list (.store sym))))
	 (.skip (list .noop))
	 ((.comp stm1 stm2) (append (cs stm1) (cs stm2)))
	 ((.if test then else) (append (cb test) (list (.branch (cs then) (cs else)))))
	 ((.while b s) (list (.loop (cb b) (cs s))))
	 ((.try-catch s1 s2) (append (cs s1) (list (.protect (cs s2)))))))
(export '(cs ca cb))
;;;;; Interpreter

;; Evaluation stack represented as list
(defclass store ()
  ((backing-map :accessor b-map
		:initform (make-hash-table))
   (valid-state :accessor valid?
		:initform t)))
(defmethod print-object ((object store) s)
  (format s "{")
  (maphash (lambda (k v)
	     (format s "{~A -> ~A} " k v))
	   (b-map object))
  (format s "}")
  (if (valid? object)
      (format s "⊤")
      (format s "⊥")))
(defun ref (store var)
  (gethash var (b-map store)))
(defsetf ref (store var) (new-value)
  (once-only (store var new-value)
    `(setf (gethash ,var (b-map ,store))
	   ,new-value)))

(defclass program ()
  ((c :accessor c :initarg :c) ; List of inst
   (e :accessor e :initarg :e) ; List of integer|bool
   (s :accessor s :initarg :s))) ; Var -> integer|bool map
(defmethod valid? ((object program))
  (valid? (s object)))
(defun make-program (c e &optional (s (make-instance 'store)))
  (make-instance 'program :c c :e e :s s))
(defmethod print-object ((object program) s)
  (format s "(~A; ~A; ~A)" (car (c object)) (e object) (s object)))

(defvar *debug* nil
  "Debug mode")
(defun interpret (stm &key (debug *debug*)
			(initial-values))
  "Interpret Stm by compiling into Code and then running in a local VM"
  (let ((program (make-program (cs stm) nil)))
    (loop for (var val) in initial-values do
	 (setf (ref (s program) var) val))
    (loop while (not (null (c program))) do
	 (let ((new-program (interpret-step program debug)))
	   (setf program new-program)))
    program))

(defun bottomify (f)
  "Make some function f into one that knows about bottom"
  (lambda (&rest args)
    (if (member :bottom args)
	:bottom
	(handler-case (apply f args)
	  (t () :bottom)))))

(defun interpret-step (program &optional (debug *debug*))
  "Given a `program' produce its next configuration"
  (when debug
    (break))
  (flet ((binary-op (op stack)
	   (make-program (cdr (c program))
			 (push (funcall (bottomify op) (pop stack) (pop stack)) stack)
			 (s program)))
	 (unary-op (op stack)
	   (make-program (cdr (c program))
			 (push (funcall (bottomify op) (pop stack)) stack)
			 (s program)))
	 (constant (v stack)
	   (make-program (cdr (c program))
			 (push v stack)
			 (s program))))
    (let ((head (car (c program)))
	  (stack (copy-list (e program))))
      (adt:match inst head
		 ((.push n)
		  (make-program (cdr (c program))
				(push n stack)
				(s program)))
		 (.add
		  (binary-op #'+ stack))
		 (.mult
		  (binary-op #'* stack))
		 (.sub
		  (binary-op #'- stack))
		 (.div
		  (binary-op #'/ stack))
		 (.true
		  ;; Letting T stand for semantic truth
		  (constant t stack))
		 (.false
		  ;; Letting NIL stand for semantic falsehood
		  (constant nil stack))
		 (.eq
		  (binary-op #'equal stack))
		 (.le
		  (binary-op #'<= stack))
		 (.and
		  (binary-op #'(lambda (x y) (and x y)) stack))
		 (.neg
		  (unary-op #'not stack))
		 ((.fetch x)
		  (make-program (cdr (c program))
				(push (ref (s program) x) stack)
				(s program)))
		 ((.store x)
		  (let ((val (pop stack)))
		    (if (or (eq val :bottom) (not (valid? (s program))))
			(setf (valid? (s program)) nil)
			(setf (ref (s program) x) val))
		    (make-program (cdr (c program))
				  stack
				  (s program))))
		  (.noop
		  (make-program (cdr (c program)) (e program) (s program)))
		  ((.branch l1 l2)
		   (if (not (valid? (s program)))
		       (make-program (cdr (c program)) (e program) (s program))
		       (let ((b (pop stack)))
			 (make-program
			  (cond ((eq b :bottom)
				 (cdr (c program)))
				((not b)
				 (append l2 (cdr (c program))))
				(t (append l1 (cdr (c program)))))
			  stack (if (not (eq b :bottom))
				    (s program)
				    (progn (setf (valid? (s program)) nil)
					   (s program)))))))
		  ((.loop c1 c2)
		   (if (not (valid? (s program)))
		       (make-program (cdr (c program)) (e program) (s program))
		       (make-program
			(append c1
				(list (.branch
				       (append
					c2
					(list (.loop c1 c2)))
				       (list .noop)))
				(cdr (c program)))
			stack (s program))))
		 ((.protect s1)
		  (make-program
		   (if (valid? (s program))
		       (cdr (c program))
		       (prog1 (append s1 (cdr (c program)))
			 (setf (valid? (s program)) t)))
		   (e program) (s program)))))))

(defun parse-stm (src)
  (omatch while stm () (string->list src)))
(defun file->stm (path)
  (with-open-file (stream path)
    (let ((src (munch-whitespace (alexandria:read-stream-content-into-string stream))))
      (parse-stm src))))
(defun interpret-file (path &optional debug initial-values)
  (interpret (file->stm path) :debug debug :initial-values initial-values))


(defparameter *complex-src* "z:=0;while y<=x do z:=z+1;x:=x-y")
;; (interpret (parse-stm *complex-src*) :initial-values '((|x| 17) (|y| 5)))
;; => (NIL; NIL; {{x -> 2} {y -> 5} {z -> 3} }⊤)
(defparameter *try-catch-src* "x:=7;try x:=x-7;x:=7/x;x:=x+7 catch x:=x-7")
;; (interpret (parse-stm *try-catch-src*))
;; => (NIL; NIL; {{x -> -7} }⊤)
(defparameter *will-i-loop-src?* "x:=1/0;y:=5;while y<=x do y:=y-1")
;; (interpret (parse-stm *will-i-loop-src?*))
;; => (NIL; NIL; {}⊥)
(defparameter *double-try-src* "x:=1;try try x:=x/0 catch x:=1 catch x:=2")
;; (interpret (parse-stm *double-try-src*))
;; => (NIL; NIL; {{x -> 1} }⊤)
(defparameter *if-fail-src* "x:=1;if x<=0 then x:=1/0 else x:=2")
;; (interpret (parse-stm *if-fail-src*))
;; => (NIL; NIL; {{x -> 2} }⊤)
