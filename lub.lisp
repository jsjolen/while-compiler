(in-package :lab1)
(declaim (optimize (debug 3)))

(defun find-tags (tag configs)
   (remove-if #'null
		     (loop for config in configs collect
			  (when (car (c config))
			    (if (equal tag
				       (match inst (car (c config))
					      ((.push _ tag2)  tag2)
					      ((.add tag2) tag2)
					      ((.sub tag2) tag2)
					      ((.mult tag2) tag2)
					      ((.div tag2) tag2)
					      ((.push-bool _ tag2) tag2)
					      ((.eq tag2) tag2)
					      ((.le tag2) tag2)
					      ((.and tag2) tag2)
					      ((.negate tag2) tag2)
					      ((.fetch _ tag2) tag2)
					      ((.store _ tag2) tag2)
					      ((.noop tag2) tag2)
					      ((.try _ _ tag2) tag2)
					      ((.catch _ tag2) tag2)
					      ((.branch _ _ tag2) tag2)
					      ((.loop _ _ tag2) tag2)))
				config nil)))))

(defun collect-by-tag (stm configs)
  (match stm stm
	 ((.skip tag)
	  (find-tags tag configs))
	 ((.if _ _ _ tag)
	  (find-tags tag configs))
	 ((.while _ _ tag)
	  (find-tags tag configs))
	 ((.set _ _ tag)
	   (find-tags tag configs))
	 ((.comp _ _ tag)
	  (find-tags tag configs))
	 ((.try-catch _  _ tag)
	  (find-tags tag configs))))

(defun compute-lub-state (control-point configurations mapping)
  (let* ((configs nil) ; List config
	 (corresponding-code-point (cadr (find control-point mapping :key #'car :test #'equals)))
	 (corresponding-configurations nil))
    (flet ((same-head? (inst-code-point program)
	     (let ((list-of-bools (mapcar #'(lambda (a b)
					      (equals a b))
				    inst-code-point
				    (c program))))
	       (reduce (lambda (a b) (and a b))
		       list-of-bools
		       :initial-value (car list-of-bools)))))
      (maphash-keys
       (lambda (program)
	 (push program configs)
	 (when (same-head? corresponding-code-point program)
	   (push program corresponding-configurations)))
       configurations))
    (let ((lubbed-conf (car corresponding-configurations))
	  (lubbed-set/while/if))
      (when (not lubbed-conf)
	;; Unreachable branch
	;(cerror "Continue" "Unreachable branch: ~A,~A" corresponding-code-point control-point)
	(return-from compute-lub-state (values control-point nil)))
	(loop for program in corresponding-configurations do
	     (when (valid? program)
	       (setf (s lubbed-conf) (store-lub (s program) (s lubbed-conf)))))
	;; Just calculate and return the BExp/AExp lub as a third value!!!
	(setf lubbed-set/while/if
	      (match stm control-point
		     ((.set _ _ _)
		      (let ((confs (collect-by-tag control-point configs))
			    (lub nil))
			(loop for c in confs if (car (c c)) do
			     (match inst (car (c c))
				    ((.store _ _)
				     (if lub
					 (setf lub (sign-lub lub (car (e c))))
					 (setf lub (car (e c)))))
				    (_ nil)))
			lub))
		     ((.while _ _ _)
		      (let ((confs (collect-by-tag control-point configs))
			    (lub nil))
			(loop for c in confs if (car (c c)) do
			     (match inst (car (c c))
				    ((.branch _ _ _)
				     (if lub
					 (setf lub (tt-lub lub (car (e c))))
					 (setf lub (car (e c)))))
				    (_ nil)))
			lub))
		     ((.if _ _ _ _)
		      (let ((confs (collect-by-tag control-point configs))
			    (lub nil))
			(loop for c in confs if (car (c c)) do
			     (match inst (car (c c))
				    ((.branch _ _ _)
				     (if lub
					 (setf lub (tt-lub lub (car (e c))))
					 (setf lub (car (e c)))))
				    (_ nil)))
			lub))
		     (_ nil)))
	(values
	 control-point
	 lubbed-conf
	 lubbed-set/while/if))
      ))

(defun aexp-lub (aexp store)
  (match aexp aexp
	 ((.ref v) (gethash v (b-map store)))
	 ((.num sign) sign)
	 ((.+ x y) (sign-add (aexp-lub x store) (aexp-lub y store)))
	 ((.* x y) (sign-mult (aexp-lub x store) (aexp-lub y store)))
	 ((./ x y) (sign-div (aexp-lub x store) (aexp-lub y store)))
	 ((.- x y) (sign-sub (aexp-lub x store) (aexp-lub y store)))))
(defun bexp-lub (bexp store)
  (match bexp bexp
	 ((.bool tt) tt)
	 ((.= a b) (tt-eq (aexp-lub a store) (aexp-lub b store)))
	 ((.<= a b) (tt-leq (aexp-lub a store) (aexp-lub b store)))
	 ((.~ a) (tt-neg (bexp-lub a store)))
	 ((.^ a b) (tt-and (bexp-lub a store) (bexp-lub b store)))))

(defun store-lub (store-a store-b)
  (let ((lub-store (alter store-a (valid? store-a)))) ;; copy!
    (maphash (lambda (ka va)
	       (setf (gethash ka (b-map lub-store)) va))
	     (b-map store-a))
    (maphash (lambda (kb vb)
	       (multiple-value-bind (v exists?) (gethash kb (b-map lub-store))
		 (if exists?
		     (setf (gethash kb (b-map lub-store))
			   (sign-lub v vb))
		     (setf (gethash kb (b-map lub-store)) vb))))
	     (b-map store-b))
    lub-store))

#|
(multiple-value-bind (configs code mapping) (interpret (parse-stm *multi-branch-if*) :initial-values '((|x| #..any)))
	(let ((x (parse-stm *multi-branch-if*)))
	  (print mapping)
	    (compute-lub-state x configs mapping)))
|#

(defun gather-cp-lubs (stm configs mapping accumulation)
  (multiple-value-bind (new-cp store lubbed)
      (compute-lub-state stm configs mapping)
    (match stm stm
	   ((.skip tag)
	    (append (list (list new-cp store))) accumulation)
	   ((.if bexp then else tag)
	    (append (list (list new-cp store lubbed)) accumulation
		    (gather-cp-lubs then configs mapping nil)
		    (gather-cp-lubs else configs mapping nil)))
	   ((.while bexp loop tag)
	     (append (list (list new-cp store lubbed)) accumulation
		     (gather-cp-lubs loop configs mapping nil)))
	   ((.set var aexp tag)
	    (append (list (list new-cp store lubbed)) accumulation))
	   ((.comp fst snd tag)
	    (append (list (list new-cp store)) accumulation
		     (gather-cp-lubs fst configs mapping nil)
		      (gather-cp-lubs snd configs mapping nil)))
	   ((.try-catch try catch tag)
	    (append (list (list new-cp store)) accumulation
		     (gather-cp-lubs try configs mapping nil)
		      (gather-cp-lubs catch configs mapping nil))))))
(defun all-cp-lubs (stm &key (debug *debug*) initial-values)
  (multiple-value-bind (configs code mapping) (interpret stm :debug debug :initial-values initial-values)
    (declare (ignore code))
    (gather-cp-lubs stm configs mapping '())))
