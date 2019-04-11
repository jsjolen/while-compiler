(in-package :lab1)
;;;;; Lexer/parser
;; Munch whitespace

(defun string->list (string)
  (map 'list #'identity string))
(defparameter *whitespace*
  (remove-duplicates
   (list #\Space #\Tab #\Linefeed #\Return #\Newline #\Page
	 #\Vt                 ;Vertical tab.
	 ))
  "Whitespace characters.")
(defun whitespacep (char)
  (member char *whitespace*))
(defun munch-whitespace (input)
  ;; Actually could've left it as a stream considering we do it char by char
  ;; Whatevs
  (when (streamp input)
    (setf input (read-stream-content-into-string input)))
  (with-output-to-string (s)
    (let ((in-whitespace? nil))
      (loop for char across input do
	   (cond ((and (not in-whitespace?) (whitespacep char))
		  ;; Set flag & print space (no newlines please)
		  (setf in-whitespace? t)
		  (format s " "))
		 ((and in-whitespace? (whitespacep char))
		  nil) ; Munch the whitespace

		 ((not (whitespacep char))
		  (setf in-whitespace? nil)
		  (format s (string char))))))))

(defun char->number (char)
  (case char
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))

(defgrammar while ()
  (digit () (char-range #\0 #\9))
  (char-range (x y)
              (bind c _)
              :->? (and (characterp c)
                        (char<= x c y))
              :-> c)
  (letter ()
	  (or
	   (char-range #\A #\Z)
	   (char-range #\a #\z)))
  (int ()
       (or (seq
	    (bind n (int))
	    (bind d (digit))
	    :-> (+ (* n 10) (char->number d)))
	   (seq (bind d (digit))
		:-> (char->number d))))
  (kint ()
	(seq (bind int (int))
	     :-> (lab1:.num int)))
  (kvariable ()
       (or (seq
	    (bind v (kvariable))
	    (bind l (letter))
	    :-> (concatenate 'string v (string l)))
	   (seq (bind l (letter))
		:-> (string l))))
  (kvar ()
       (seq (bind kvar (kvariable))
	    :-> (lab1:.ref (intern kvar))))
  (arithm-op ()
	     (or #\+ #\- #\* #\/))
  (arithm ()
	  (or
	   (seq
	    (bind left (arithm))
	    (bind op (arithm-op))
	    (bind right (arithm))
	    :->
	    (case op
	      (#\+ (lab1:.+ left right))
	      (#\- (lab1:.- left right))
	      (#\* (lab1:.* left right))
	      (#\/ (lab1:./ left right))))
	   (kint)
	   (kvar)))
  (true ()
 	(bind c (seq #\t #\r #\u #\e))
	:-> lab1:.tt)
  (false ()
	 (seq #\f #\a #\l #\s #\e)
	 :-> lab1:.ff)
  (bool-arithm-eq () #\=)
  (bool-arithm-le () (seq #\< #\=))
  (bool-arithm ()
	       (or
		(seq
		 (bind left (arithm))
		 (bind op (bool-arithm-eq))
		 (bind right (arithm))
		 :->
		 (lab1:.= left right)
		 )
		(seq
		 (bind left (arithm))
		 (bind op (bool-arithm-le))
		 (bind right (arithm))
		 :->
		 (lab1:.<= left right))))
  (bool () 
	(or
	 (seq 
	  (bind left (bool))
	  #\^
	  (bind right (bool))
	  :-> (lab1:.^ left right))
	 (seq
	  (bind left (bool))
	  #\~
	  :-> (lab1:.~ left))
	 (bool-arithm)
	 (true)
	 (false)))
  (stm ()
       (or
	(seq ; comp
	 (bind left (stm))
	 #\;
	 (bind right (stm))
	 :->
	 (lab1:.comp left right))
	(seq ; if
	 #\i #\f #\Space
	 (bind b (bool))
	 #\Space #\t #\h #\e #\n #\Space
	 (bind then (stm))
	 #\space #\e #\l #\s #\e #\Space
	 (bind else (stm))
	 :-> (lab1:.if b then else))
	(seq ; while
	 #\w #\h #\i #\l #\e #\Space
	 (bind b (bool))
	 #\Space #\d #\o #\Space
	 (bind loop (stm))
	 :-> (lab1:.while b loop))
	(seq ; try-catch
	 #\t #\r #\y #\Space
	 (bind s1 (stm))
	 #\Space #\c #\a #\t #\c #\h #\Space
	 (bind s2 (stm))
	 :-> (lab1:.try-catch s1 s2))
	(seq ; set
	 (bind left (kvariable))
	 (seq #\: #\=)
	 (bind right (arithm))
	 :-> (lab1:.set (intern left) right))
	(seq #\s #\k #\i #\p ; skip
	     :-> lab1:.skip)))
  (spaces () #\space))
