(in-package :lab1)

(defun format-lub-state (stm lub-state maybe-lub stream indent-level)
  (if lub-state
      (match stm stm
	     ((.set var aexp tag)
	      (format stream "{ ~A = ~A } rhs: ~A~%"
		      var (gethash var (b-map (s lub-state)))
		      (with-output-to-string (s)
			(cond
			  ((eq '.any (type-of maybe-lub))
			   (format s "~vt~A (Possible exception raiser!)~%" 0 maybe-lub))
			  ((eq '.err (type-of maybe-lub))
			   (format s "~vt~A (Definite exception raiser!)~%" 0 maybe-lub))
			  (t  (format s "~vt~A" 0  maybe-lub))))))
	     ((.while _ _ _)
	      (format stream "~vtLoop test Lub is: ~A, therefore ~A~%"
		      indent-level
		      maybe-lub
		      (match tt-exc maybe-lub
			     (.tt "while is always taken and WON'T terminate")
			     (.ff "while is never taken")
			     (.t "while may or may not be taken and MIGHT terminate")
			     (.err "while test always errors")
			     (.any "while test might error")
			     (_ "we know nothing"))))
	     ((.if _ _ _ _)
	      (format stream "~vtIf test Lub is: ~A, therefore ~A~%"
		      indent-level
		      maybe-lub
		      (match tt-exc maybe-lub
			     (.tt "then is always taken")
			     (.ff "else is always taken")
			     (.t "then or else might be taken")
			     (.err "if test always errors")
			     (.any "if test might error")
			     (_ "we know nothing"))))
	     (_ (format stream "~%")))
      (format stream "~vtUnreachable branch~%" indent-level)))
(defun pprint-analysis (stm lub-states final-configs &optional (stream *standard-output*) (indent-level 0))
  (%pprint-analysis stm lub-states stream indent-level)
  (format stream "~%")
  (loop for final-config in final-configs do
       (pprint-state (s final-config) stream)))
(defun pprint-state (store stream)
  (format stream "{")
  (maphash
   (lambda (k v)
     (format stream " ~A -> ~A" k v))
   (b-map store))
  (format stream " }")
  (if (valid? store)
      (format stream " normal termination~%" )
      (format stream " abnormal termination~%" )))
(defun %pprint-analysis (stm lub-states &optional (stream *standard-output*) (indent-level 0))
  (let ((corresponding-lub-state (cadr (find stm lub-states :key #'car :test #'equals)))
	(maybe-lub (caddr (find stm lub-states :key #'car :test #'equals))))
   (format-lub-state stm corresponding-lub-state maybe-lub stream indent-level)
    (match stm stm
	   ((.skip _)
	    (format stream "skip"))
	   ((.if test then else _)
	    (format stream "~vtIf ~A~%Then~%~A~%Else~%~A~%"
		    indent-level
		    (with-output-to-string (s)
		      (pprint-bexp test s))
		    (with-output-to-string (s)
		      (%pprint-analysis then lub-states s (+ indent-level 4)))
		    (with-output-to-string (s)
		      (%pprint-analysis else lub-states s (+ indent-level 4)))))
	   ((.while test loop _)
	    (format stream "~vtWhile ~A~%Do ~A~%" indent-level
		    (with-output-to-string (s) (pprint-bexp test s))
		    (with-output-to-string (s)
		      (%pprint-analysis loop lub-states s (+ indent-level 4)))))
	   ((.set var aexp _)
	    (format stream "~vt~A := ~A" indent-level var (with-output-to-string (s) (pprint-aexp aexp s))))
	   ((.comp fst snd _)
	    (format stream "~vt~%~A;~%~A" indent-level
		    (with-output-to-string (s)
		      (%pprint-analysis fst lub-states s (+ indent-level)))
		    		    (with-output-to-string (s)
				      (%pprint-analysis snd lub-states s (+ indent-level)))))
	   ((.try-catch try catch _)
	    (format stream "~vtTry~%~A~%Catch~%~A~%" indent-level
		    (with-output-to-string (s)
		      (%pprint-analysis try lub-states s (+ indent-level 4)))
		    (with-output-to-string (s)
		      (%pprint-analysis catch lub-states s (+ indent-level 4))))))))

(defun pprint-aexp (aexp stream)
  (match aexp aexp
	 ((.ref v) (format stream "~A" v))
	 ((.num n) (format stream "~A" n))
	 ((.+ a1 a2) (format stream "~A+~A"
			     (with-output-to-string (s)
			       (pprint-aexp a1 s))
			     (with-output-to-string (s)
			       (pprint-aexp a2 s))))
	 ((./ a1 a2) (format stream "~A/~A"
			     (with-output-to-string (s)
			       (pprint-aexp a1 s))
			     (with-output-to-string (s)
			       (pprint-aexp a2 s))))
	 ((.* a1 a2) (format stream "~A*~A"
			     (with-output-to-string (s)
			       (pprint-aexp a1 s))
			     (with-output-to-string (s)
			       (pprint-aexp a2 s))))
	 ((.- a1 a2) (format stream "~A-~A"
			     (with-output-to-string (s)
			       (pprint-aexp a1 s))
			     (with-output-to-string (s)
			       (pprint-aexp a2 s))))))
(defun pprint-bexp (bexp stream)
  (match bexp bexp
	 ((.bool b) (if b (format stream "true")
			(format stream "false")))
	 ((.= ae1 ae2) (format stream "~A=~A"
				(with-output-to-string (s)
				  (pprint-aexp ae1 s))
				(with-output-to-string (s)
				  (pprint-aexp ae2 s))))
	 ((.<= ae1 ae2)
	  (format stream "~A<=~A"
				(with-output-to-string (s)
				  (pprint-aexp ae1 s))
				(with-output-to-string (s)
				  (pprint-aexp ae2 s))))
	 ((.~ bexp)
	  (format stream "¬~A"
		  (with-output-to-string (s)
		    (pprint-bexp bexp s))))
	 ((.^ be1 be2)
	  (format stream "~A∧~A"
		  (with-output-to-string (s)
		    (pprint-bexp be1 s))
		  (with-output-to-string (s)
		    (pprint-bexp be2 s))))))
(defun pprint-program (program-source &key (debug nil) (initial-values nil))
  (multiple-value-bind (configs code mapping) (interpret (parse-stm program-source) :debug debug :initial-values initial-values)
    (declare (ignore code))
    (let (final-conf)
      (maphash-keys (lambda (k)
		      (if (not (c k))
			  (push k final-conf)))
		    configs)
      (pprint-analysis (parse-stm program-source)
		       (gather-cp-lubs (parse-stm program-source) configs mapping nil)
		       final-conf))))
