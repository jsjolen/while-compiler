(in-package :lab1)
(declaim (optimize (debug 3)))
;;;;; AST
  ;; While
  ;; Convention: Prepend . to all constructors to avoid name-clashes with CL
  ;; AST for While
"TODO:
Step 2.
Go through the semantics as shown in the lecture and see where stuff is non-deterministic.
Figure out when and how they are for Code too.

OK the semantics of if run both branches iff DB(b)ps == ANY and While is defined in terms of unrolling if.
Therefore:
LOOP is defined in terms of BRANCH, so execution only needs to be non-deterministic in BRANCH.

BRANCH(c1,c2):c, ANY:e, ps => c1:c, e, ps
BRANCH(c1,c2):c, ANY:e, ps => c2:c, e, ps
is all that needs to be added.

Consider:

Err <= Any.

Should not try-catch be non-deterministic too?

According to the PDF we perhaps ought to split execution on Any into one on Z and one on Err,
this makes sense! Then try-catch is kept deterministic and instead we expand STORE.

STORE-X:c, ANY:e, ps => c, e, ps^[X -> ERR]
STORE-X:c, ANY:e, ps => c, e, ps[X -> Z]

Now we get errors to be non-deterministic too!
OK, sounds good, IMPLEMENT IT!

THEN:
- Compute multiple configurations on BRANCH
- Compute multiple configurations on TRY-CATCH (ERR <= ANY)
DONE
Step 3.
- Compute LUB of store
Formally define this!
DONE

- Compute the ass/if/while lubs (?)
DONE
Step 4.
- Analysis
In *pprint-src* the inner sub control-point isn't caught as a configuration
- Pretty-printer
Pretty-print If-branches lubs!
Step 5.
"

(defdata aexp
  (.ref symbol)
  (.num fixnum)
  (.+ aexp aexp)
  (.* aexp aexp)
  (./ aexp aexp)
  (.- aexp aexp))
(defdata bexp
  (.bool boolean)
  (.= aexp aexp)
  (.<= aexp aexp)
  (.~ bexp)
  (.^ bexp bexp))
(defdata stm
  (.skip symbol)
  (.if bexp stm stm symbol)
  (.while bexp stm symbol)
  (.set symbol aexp symbol)
  (.comp stm stm symbol)
  (.try-catch stm stm symbol))
(export '(aexp bexp stm .skip .if .while .set .comp .= .<= .~ .^ .ref .num .+ .* .- ./ .try-catch))

;; AST for AM
(defdata inst
  (.push t symbol)
  (.add symbol) (.mult symbol) (.sub symbol) (.div symbol)
  (.push-bool common-exc symbol)
  (.eq symbol) (.le symbol) (.and symbol) (.negate symbol)
  (.fetch t symbol)
  (.store t symbol)
  (.noop symbol)
  (.try list list symbol)
  (.catch list symbol)
  (.branch list list symbol) (.loop list list symbol)) ;; list of inst
;;;;; Compiler

(defparameter *tag-counter* 0)
(defun gen-tag ()
  (intern (format nil "~A" (incf *tag-counter*))))
(defun tag () (intern (format nil "~A" *tag-counter*)))
(defun reset-tag ()
  (setf *tag-counter* 0))
(defun tagify (stm tag)
  (match stm stm
	 ((.set sym ae1 _) (.set sym ae1 tag))
	 ((.skip _) (.skip tag))
	 ((.comp stm1 stm2 _) (.comp (tagify stm1 (gen-tag))
				     (tagify stm2 (gen-tag))
				     tag))
	 ((.if test then else _)
	  (.if test (tagify then (gen-tag))
	       (tagify else (gen-tag)) tag))
	 ((.while b s _) (.while b (tagify s (gen-tag)) tag))
	 ((.try-catch s1 s2 _)
	  (.try-catch (tagify s1 (gen-tag))
		      (tagify s2 (gen-tag)) tag))))

;; We're utilizing the LIST/APPEND pattern
;; to create our program.

(defun ca (aexp tag)
  "Returns (list inst)"
  (match aexp aexp
	 ((.num n) (list (.push (abs-z n) tag)))
	 ((.ref sym) (list (.fetch sym tag)))
	 ((.+ ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.add tag))))
	 ((.* ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.mult tag))))
	 ((.- ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.sub tag))))
	 ((./ ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.div tag))))))
(defun cb (bexp tag)
  "Returns (list inst)"
  (match bexp bexp
	 ((.bool b) (list (.push-bool (abs-tt b) tag)))
	 ((.= ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.eq tag))))
	 ((.<= ae1 ae2) (append (ca ae2 tag) (ca ae1 tag) (list (.le tag))))
	 ((.~ be1) (append (cb be1 tag) (list (.negate tag))))
	 ((.^ be1 be2) (append (cb be2 tag) (cb be1 tag) (list (.and tag))))))
;; TODO: This doesn't work because of no good equality/hashing
(defvar *mapping* nil)
(defun push-alist (k v)
  (prog1 v
    (push (list k v) *mapping*)))
(defun cs (stm tag)
  "Returns (list inst)"
  (match stm stm
	 ((.set sym ae1 tag)
	  (push-alist stm (append (ca ae1 tag) (list (.store sym tag)))))
	 (.skip (push-alist stm (list (.noop tag))))
	 ((.comp stm1 stm2 tag) (push-alist stm (append (cs stm1 tag) (cs stm2 tag))))
	 ((.if test then else tag) (push-alist stm (append (cb test tag) (list (.branch (cs then tag) (cs else tag) tag)))))
	 ((.while b s tag) (push-alist stm (list (.loop (cb b tag) (cs s tag) tag))))
	 ((.try-catch s1 s2 tag) (push-alist stm (list (.try (cs s1 tag) (cs s2 tag) tag))))))
(export '(cs ca cb))
;;;;; Interpreter

;; Evaluation stack represented as list
(defclass store ()
  ((backing-map :accessor b-map
		:initform (make-hash-table)
		:initarg :backing-map)
   (valid-state :accessor valid?
		:initform t
		:initarg :valid?)))
(defun non-valid (store)
  (setf (valid? (copy-store store)) nil))
(defun alter (store validity &rest key/vals)
  (declare (type boolean validity)
	   (type store store))
  (let ((new-store (copy-store store)))
    (setf (valid? new-store) validity)
    (loop for (k v) in key/vals do
	 (when (and k v)
	     (setf (gethash k (b-map new-store)) v)))
    new-store))

(defun copy-store (store)
  (let ((ht (make-hash-table)))
    (maphash (lambda (k v)
	       (setf (gethash k ht) v))
	     (b-map store))
    (make-instance 'store
		   :backing-map ht)))
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
  (print "hi")
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


(defgeneric equals (a b))
(defmethod equals ((a symbol) (b symbol))
  (eq a b))
(defmethod equals ((a program) (b program))
  (and (equals (c a) (c b))
       (equals (e a) (e b))
       (equals (s a) (s b))))
(defmethod equals ((a store) (b store))
  (and (equals (b-map a) (b-map b))
       (equals (valid? a) (valid? b))))
(defmethod equals ((a list) (b list))
  (reduce  #'(lambda (a b) (and a b)) (mapcar #'equals a b) :initial-value t))
(defmethod equals ((a hash-table) (b hash-table))
  (let ((equal? t))
    (maphash (lambda (ka _)
	       (declare (ignore _))
	       (setf equal? (and equal? (equals (gethash ka a) (gethash ka b)))))
	     a)
    equal?))
(defmethod equals (a b)
  (equal a b))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ctr-equals (ctr arity)
    (let ((argsa (loop for i from 1 upto arity collect (intern (format nil "~Aa" i))))
	  (argsb (loop for i from 1 upto arity collect (intern (format nil "~Ab" i)))))
      `(defmethod equals ((a ,ctr) (b ,ctr))
	 (with-data (,ctr ,@argsa) a
	   (with-data (,ctr ,@argsb) b
	     (reduce #'(lambda (a b) (and a b))
		     (mapcar #'equals (list ,@argsa) (list ,@argsb))
		     :initial-value t)))))))
(defmacro adt-equals (adt)
  `(progn
     ,@ (loop for (ctr arity) in (get-constructors adt) collect
	     (ctr-equals ctr arity))))
(adt-equals inst)
(adt-equals common-exc)
(adt-equals sign-exc)
(adt-equals tt-exc)
(adt-equals stm)
(adt-equals aexp)
(adt-equals bexp)

;; This probably works but we get screwed over because of mutation *MAYBE*
(defun hash-program (program)
  "Clearly gives same hash for equal programs." 
 (when (not (eq (type-of program) 'program))
    (error "Tried to hash something which wasn't a program!"))
  (let (keys vals)
    (maphash (lambda (k v) (push k keys) (push v vals)) (b-map (s program)))
    (apply #'logxor
	   (sxhash (c program))
	   (sxhash (e program))
	   (sxhash (valid? (s program)))
	   (append (mapcar #'sxhash keys)
		   (mapcar #'sxhash vals)))))
(sb-ext:define-hash-table-test equals hash-program)

(defvar *debug* nil
  "Debug mode")

"Currently INTERPRET has a global table of seen configurations.
Surely this is good enough? Yeah, it ought to be."
(defun interpret (stm &key (debug *debug*)
			(initial-values))
  "Interpret Stm by compiling into Code and then running in a local VM"
  (reset-tag)
  (let ((*mapping* nil)
	(*tag-counter* 0))
    (let* ((compiled-code (cs stm '|0|))
	   (program (make-program compiled-code nil))
	   (programs-seen (make-hash-table :test #'equals)))
      (loop for (var val) in initial-values do
	   (setf (ref (s program) var) val))
      (labels ((interpreter-loop (program)
		 (if (and (not (null (c program))) (not (gethash program programs-seen)))
		     (let ((new-programs (interpret-step program debug)))
		       (setf (gethash program programs-seen) t)
		       ;; Explore all remaining configuration paths in a DFS fashion
		       ;; (DFS implicit -- the call stack is our stack of remaining work)
		       (loop for new-program in new-programs do
			    (interpreter-loop new-program)))
		     (progn
		       ;; Kludge: Code might be empty, still gotta collect the config
		       (setf (gethash program programs-seen) t)
		       programs-seen))))
	(interpreter-loop program)
	(values programs-seen
		compiled-code
		*mapping*)))))

(defun interpret-step (program &optional (debug *debug*))
  "Given a `program' produce its next configurations"
  (when debug
    (break))
  (flet ((binary-op (op stack)
	   (list (make-program (cdr (c program))
			       (push (funcall op (pop stack) (pop stack)) stack)
			       (s program))))
	 (unary-op (op stack)
	   (list (make-program (cdr (c program))
			       (push (funcall  op (pop stack)) stack)
			       (s program))))
	 (constant (v stack)
	   (list (make-program (cdr (c program))
			       (push v stack)
			       (s program)))))
    (let ((head (car (c program)))
	  (stack (copy-list (e program))))
      (adt:match inst head
		 ((.push n _)
		  (list (make-program (cdr (c program))
				      (push n stack)
				      (s program))))
		 ((.add _)
		  (binary-op #'sign-add stack))
		 ((.mult _)
		  (binary-op #'sign-mult stack))
		 ((.sub _)
		  (binary-op #'sign-sub stack))
		 ((.div _)
		  (binary-op #'sign-div stack))
		 ((.push-bool abs-tt)
		  (constant abs-tt stack))
		 ((.eq _)
		  (binary-op #'tt-eq stack))
		 ((.le _)
		  (binary-op #'tt-leq stack))
		 ((.and _)
		  (binary-op #'tt-and stack))
		 ((.negate _)
		  (unary-op #'tt-neg stack))
		 ((.fetch x _)
		  (list (make-program (cdr (c program))
				      (push (ref (s program) x) stack)
				      (s program))))
		 ((.store x _)
		  (let* ((val (pop stack))
			 (new-stores
			  (match common-exc val
				 (.err  (list (alter (s program) nil)))
				 (.any 
				  (list (alter (s program) nil)
					(alter (s program) t (list x #..z))))
				 (_  (list (alter (s program) (valid? (s program)) (if (valid? (s program)) ; Only push new value if we're in a valid state
										       (list x val)
										       (list))))))))
		    (loop for store in new-stores collect
			 (make-program (cdr (c program)) stack store))))
		 ((.noop _)
		  (list (make-program (cdr (c program)) (e program) (s program))))
		 ((.branch l1 l2 _)
		  (if (not (valid? (s program)))
		      (list (make-program (cdr (c program)) (e program) (s program)))
		      (let ((b (pop stack)))
			(cond ((eq (type-of b) (type-of #..err))
			       (list (make-program (cdr (c program)) (e program) (alter (s program) nil))))
			      ((eq (type-of b) (type-of #..ff))
			       (list (make-program (append l2 (cdr (c program))) (e program) (s program))))
			      ((eq (type-of b) (type-of #..tt))
			       (list (make-program (append l1 (cdr (c program))) (e program) (s program))))
			      ((eq (type-of b) (type-of #..t))
			       ;; In cases of false and true
			       (list
			        (make-program (append l2 (cdr (c program))) stack (s program))
				(make-program (append l1 (cdr (c program))) stack (s program))))
			      ((eq (type-of b) (type-of #..any))
			       (list
				; In case it's an Err
				(make-program (cdr (c program)) stack (alter (s program) nil))
				; In case false
				(make-program (append l2 (cdr (c program))) stack (s program))
				; In case true
				(make-program (append l1 (cdr (c program))) stack (s program))))))))
		 ((.loop c1 c2 tag)
		  (if (not (valid? (s program)))
		      (list (make-program (cdr (c program)) (e program) (s program)))
		      (list (make-program
			     (append c1
				     (list (.branch
					    (append
					     c2
					     (list (.loop c1 c2 tag)))
					    (list (.noop tag)) tag))
				     (cdr (c program)))
			     stack (s program)))))
		 ((.try s1 s2 tag)
		  (list (make-program
			 (if (valid? (s program))
			     (append s1 (list (.catch s2 tag)) (cdr (c program)))
			     (cdr (c program)))
			 (e program)
			 (s program))))
		 ((.catch s2 _)
		  (list (make-program
			 (if (valid? (s program))
			     (cdr (c program))
			     (append s2 (cdr (c program))))
			 (e program)
			 (alter (s program) t))))))))

(defun parse-stm (src)
  (let ((*tag-counter* 0))
    (tagify (omatch while stm () (string->list src)) (tag))))
(defun file->stm (path)
  (with-open-file (stream path)
    (let ((src (munch-whitespace (alexandria:read-stream-content-into-string stream))))
      (parse-stm src))))
(defun interpret-file (path &optional debug initial-values)
  (interpret (file->stm path) :debug debug :initial-values initial-values))

(defparameter *test-src* "x:=4;while 0<=x do x:=x-2")
;; (interpret (parse-stm *test-src*))
(defparameter *multi-branch-division* "try y:=1/x catch z:=2")
;; (interpret (parse-stm *multi-branch-division*) :initial-values '((|x| #..any)))
(defparameter *failing-src* "y:=1/x; z:=2")
(defparameter *multi-branch-if* "x:=1;if x<=0 then y:=1 else y:=0-1")
;; (interpret (parse-stm *multi-branch-if*) :initial-values '((|x| #..any)))
(defparameter *unreachable-branch*  "x:=7;if x<=0 then y:=1 else y:=0-1")

(defparameter *complex-src* "z:=0;while y<=x do z:=z+1;x:=x-y")
;; (interpret (parse-stm *complex-src*) :initial-values `((|x| ,(abs-z 17)) (|y| ,(abs-z 5))))
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
(defparameter *pprint-src* "x:=7;try x:=x-7;x:=7/x;x:=x+7 catch x:=x-7")

;; This guy can't be deduce that X is #..ZERO
(defparameter *terminating-loop-src* "x:=1;while 0<=x do x:=x-1")
;; Why can't we deduce that this will never terminate?
(defparameter *never-terminating-loop-src* "x:=1;while x<=2 do y:=1")
;; We can deduce that this will never enter the branch
(defparameter *always-terminating-loop-src* "x:=1;while x<=0 do y:=1")

(defparameter *unnecessary-try-src* "x:=1;try x:=x-1 catch z:=1")
(defparameter *trivial-fail-src* "x:=1/0;y:=1")
