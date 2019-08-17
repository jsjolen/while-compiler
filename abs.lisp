(in-package :lab1)
#|
Use this like so:
FF .ff
(defparameter *and-map* ...
(fset 'map-it
      [?\C-  ?\C-e ?\C-w backspace down ?\C-a ?\C-  ?\C-\M-f ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g return ?\C-y ?\C-  M-left left ?\C-w backspace return ?\C-y return])

NONE_B .none
ANY_B .any
ERR_B .err
TT .tt
FF .ff
T .t
|#
;;;; Abstract Interpretation of Signs
;; Keeping the types as symbols now.
(defmacro define-lattice (&rest terms) nil)
(defdata common-exc
  .none .any .err)
(defdata (sign-exc :include common-exc)
  .neg .zero .pos .z
  .non-pos .non-neg .non-zero)
(defdata (tt-exc :include common-exc)
  .tt .ff .t)

(defun tt-exc-order (tt-exc)
  (match tt-exc tt-exc
	 (.none 0)
	 (.tt 1)
	 (.ff 2)
	 (.err 3)
	 (.t 4)
	 (.any 5)))
(defparameter *and-map*
  '(( #..none #..none #..none #..none #..none #..none )
    ( #..none #..tt     #..ff     #..err  #..t      #..any  )
    ( #..none #..ff     #..ff     #..err  #..t      #..any  )
    ( #..none #..err  #..err  #..err  #..err  #..err  )
    ( #..none #..t      #..t      #..err  #..t      #..any  )
    ( #..none #..any  #..any  #..err  #..any  #..any  )))
(defparameter *eq-map*
  '(( #..none #..none #..none #..none #..none #..none #..none  #..none #..none #..none )
    ( #..none #..t      #..ff     #..ff     #..err  #..t      #..t       #..ff     #..t      #..any  )
    ( #..none #..ff     #..tt     #..ff     #..err  #..t      #..ff      #..t      #..t      #..any  )
    ( #..none #..ff     #..ff     #..t      #..err  #..ff     #..t       #..t      #..t      #..any  )
    ( #..none #..err  #..err  #..err  #..err  #..err  #..err   #..err  #..err  #..err  )
    ( #..none #..t      #..t      #..ff     #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..t      #..ff     #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..ff     #..t      #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..t      #..t      #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..any  #..any  #..any  #..err  #..any  #..any   #..any  #..any  #..any  )))
(defparameter *leq-map*
  '(( #..none #..none #..none #..none #..none #..none #..none  #..none #..none #..none )
    ( #..none #..t      #..tt     #..tt     #..err  #..t      #..t       #..tt     #..t      #..any  )
    ( #..none #..ff     #..tt     #..tt     #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..ff     #..ff     #..t      #..err  #..ff     #..t       #..t      #..t      #..any  )
    ( #..none #..err  #..err  #..err  #..err  #..err  #..err   #..err  #..err  #..err  )
    ( #..none #..t      #..t      #..tt     #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..t      #..t      #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..ff     #..t      #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..t      #..t      #..t      #..err  #..t      #..t       #..t      #..t      #..any  )
    ( #..none #..any  #..any  #..any  #..err  #..any  #..any   #..any  #..any  #..any  )))
(defparameter *neg-map* '(.none .ff .tt .err .t .any))

(defparameter *tt-lub-map*
  '(( #..none #..tt     #..ff     #..err  #..t      #..any  )
    ( #..tt     #..tt     #..t      #..any  #..t      #..any  )
    ( #..ff     #..t      #..ff     #..any  #..t      #..any  )
    ( #..err  #..any  #..any  #..err  #..any  #..any  )
    ( #..t      #..t      #..t      #..any  #..t      #..any  )
    ( #..any  #..any  #..any  #..any  #..any  #..any  )))

(defparameter *tt-glb-map*
  '(( #..none #..none #..none #..none #..none #..none )
    ( #..none #..tt     #..none #..none #..tt     #..tt    )
    ( #..none #..none #..ff     #..none #..ff     #..ff    )
    ( #..none #..none #..none #..err  #..none #..err )
    ( #..none #..tt     #..ff     #..none #..t      #..t     )
    ( #..none #..tt     #..ff     #..err  #..t      #..any )))

(defun tt-lub (a b)
  (nth (tt-exc-order b) (nth (tt-exc-order a) *tt-lub-map*)))
(defun tt-glb (a b)
  (nth (tt-exc-order b) (nth (tt-exc-order a) *tt-glb-map*)))
(defun tt-and (a b)
  (nth (tt-exc-order b) (nth (tt-exc-order a) *and-map*)))
(defun tt-eq (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *eq-map*)))
(defun tt-leq (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *leq-map*)))
(defun tt-neg (a)
  (nth (tt-exc-order a) *neg-map*))

(defun sign-exc-order (sign-exc)
  (if (and (subtypep (type-of sign-exc) 'common-exc) (not (subtypep (type-of sign-exc) 'sign-exc)))
      (match common-exc sign-exc
	     (.any 9)
	     (.err 4)
	     (.none 0))
      (match sign-exc sign-exc
	     (.any 9)
	     (.err 4)
	     (.none 0)
	     (.neg 1)
	     (.zero 2)
	     (.pos 3)
	     (.non-pos 5)
	     (.non-zero 6)
	     (.non-neg 7)
	     (.z 8))))
(defparameter *div-map*
  '(( #..none  #..none   #..none  #..none   #..none #..none  #..none    #..none   #..none  #..none )
    ( #..none  #..non-neg  #..err   #..non-pos  #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..zero     #..err   #..zero     #..err  #..any   #..zero      #..any    #..any   #..any  )
    ( #..none  #..non-pos  #..err   #..non-neg  #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..err    #..err   #..err    #..err  #..err   #..err     #..err    #..err   #..err  )
    ( #..none  #..non-neg  #..err   #..non-pos  #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..z        #..err   #..z        #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..non-pos  #..err   #..non-neg  #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..z        #..err   #..z        #..err  #..any   #..z         #..any    #..any   #..any  )
    ( #..none  #..any    #..err   #..any    #..err  #..any   #..any     #..any    #..any   #..any  )))

(defparameter *mult-map*
  '(( #..none  #..none   #..none  #..none   #..none #..none  #..none    #..none   #..none  #..none )
    ( #..none  #..pos      #..zero    #..neg      #..err  #..non-neg #..non-zero  #..non-pos  #..z       #..any  )
    ( #..none  #..zero     #..zero    #..zero     #..err  #..zero    #..zero      #..zero     #..zero    #..any  )
    ( #..none  #..neg      #..zero    #..pos      #..err  #..non-pos #..non-zero  #..non-neg  #..z       #..any  )
    ( #..none  #..err    #..err   #..err    #..err  #..err   #..err     #..err    #..err   #..err  )
    ( #..none  #..non-neg  #..zero    #..non-pos  #..err  #..non-neg #..z         #..non-pos  #..z       #..any  )
    ( #..none  #..non-zero #..zero    #..non-zero #..err  #..z       #..non-zero  #..z        #..z       #..any  )
    ( #..none  #..non-pos  #..zero    #..non-neg  #..err  #..non-pos #..z         #..non-neg  #..z       #..any  )
    ( #..none  #..z        #..zero    #..z        #..err  #..z       #..z         #..z        #..z       #..any  )
    ( #..none  #..any    #..any   #..any    #..err  #..any   #..any     #..any    #..any   #..any  )))

(defparameter *add-map*
  '(( #..none  #..none   #..none   #..none   #..none #..none  #..none    #..none   #..none    #..none )
    ( #..none  #..neg      #..neg      #..z        #..err  #..neg     #..z         #..z        #..z         #..any  )
    ( #..none  #..neg      #..zero     #..pos      #..err  #..non-pos #..non-zero  #..non-neg  #..z         #..any  )
    ( #..none  #..z        #..pos      #..pos      #..err  #..z       #..z         #..pos      #..z         #..any  )
    ( #..none  #..err    #..err    #..err    #..err  #..err   #..err     #..err    #..err     #..err  )
    ( #..none  #..neg      #..non-pos  #..z        #..err  #..non-pos #..z         #..z        #..z         #..any  )
    ( #..none  #..z        #..non-zero #..z        #..err  #..z       #..z         #..z        #..z         #..any  )
    ( #..none  #..z        #..non-neg  #..pos      #..err  #..z       #..z         #..non-neg  #..z         #..any  )
    ( #..none  #..z        #..z        #..z        #..err  #..z       #..z         #..z        #..z         #..any  )
    ( #..none  #..any    #..any    #..any    #..err  #..any   #..any     #..any    #..any     #..any  )))
(defparameter *sub-map*
  '(( #..none  #..none   #..none   #..none  #..none #..none  #..none    #..none   #..none    #..none )
    ( #..none  #..z        #..neg      #..neg     #..err  #..z       #..z         #..z        #..z         #..any  )
    ( #..none  #..pos      #..zero     #..neg     #..err  #..non-neg #..non-zero  #..non-pos  #..z         #..any  )
    ( #..none  #..pos      #..pos      #..z       #..err  #..pos     #..z         #..z        #..z         #..any  )
    ( #..none  #..err    #..err    #..err   #..err  #..err   #..err     #..err    #..err     #..err  )
    ( #..none  #..pos      #..non-pos  #..neg     #..err  #..z       #..z         #..non-pos  #..z         #..any  )
    ( #..none  #..z        #..non-zero #..z       #..err  #..z       #..z         #..z        #..z         #..any  )
    ( #..none  #..pos      #..non-neg  #..z       #..err  #..non-neg #..z         #..z        #..z         #..any  )
    ( #..none  #..z        #..z        #..z       #..err  #..z       #..z         #..z        #..z         #..any  )
    ( #..none  #..any    #..any    #..any   #..err  #..any   #..any     #..any    #..any     #..any  )))

(defparameter *sign-lub-map*
  '(( #..none   #..neg      #..zero    #..pos      #..err #..non-pos #..non-zero #..non-neg #..z      #..any )
    ( #..neg      #..neg      #..non-pos #..non-zero #..any #..non-pos #..non-zero #..z       #..z      #..any )
    ( #..zero     #..non-pos  #..zero    #..non-neg  #..any #..non-pos #..z        #..non-neg #..z      #..any )
    ( #..pos      #..non-zero #..non-neg #..pos      #..any #..z       #..non-zero #..non-neg #..z      #..any )
    ( #..err    #..any    #..any   #..any    #..err #..any   #..any    #..any   #..any  #..any )
    ( #..non-pos  #..non-pos  #..non-pos #..z        #..any #..non-pos #..z        #..z       #..z      #..any )
    ( #..non-zero #..non-zero #..z       #..non-zero #..any #..z       #..non-zero #..z       #..z      #..any )
    ( #..non-neg  #..z        #..non-neg #..non-neg  #..any #..z       #..z        #..non-neg #..z      #..any )
    ( #..z        #..z        #..z       #..z        #..any #..z       #..z        #..z       #..z      #..any )
    ( #..any    #..any    #..any   #..any    #..any #..any   #..any    #..any   #..any  #..any )))
(defparameter *sign-glb-map*
  '(( #..none  #..none #..none #..none #..none  #..none  #..none   #..none  #..none   #..none   )
    ( #..none  #..neg    #..none #..none #..none  #..neg     #..neg      #..none  #..neg      #..neg      )
    ( #..none  #..none #..zero   #..none #..none  #..zero    #..none   #..zero    #..zero     #..zero     )
    ( #..none  #..none #..none #..pos    #..none  #..none  #..pos      #..pos     #..pos      #..pos      )
    ( #..none  #..none #..none #..none #..err   #..none  #..none   #..none  #..none   #..err    )
    ( #..none  #..neg    #..zero   #..none #..none  #..non-pos #..neg      #..zero    #..non-pos  #..non-pos  )
    ( #..none  #..neg    #..none #..pos    #..none  #..neg     #..non-zero #..pos     #..non-zero #..non-zero )
    ( #..none  #..none #..zero   #..pos    #..none  #..zero    #..pos      #..non-neg #..non-neg  #..non-neg  )
    ( #..none  #..neg    #..zero   #..pos    #..none  #..non-pos #..non-zero #..non-neg #..z        #..z        )
    ( #..none  #..neg    #..zero   #..pos    #..err   #..non-pos #..non-zero #..non-neg #..z        #..any    )))

    

;; TODO: Double-check order of arguments
(defun sign-lub (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *sign-lub-map*)))
(defun sign-glb (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *sign-glb-map*)))
(defun sign-add (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *add-map*)))
(defun sign-mult (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *mult-map*)))
(defun sign-div (a b) ; <- this is correct!
  (nth (sign-exc-order b) (nth (sign-exc-order a) *div-map*)))
(defun sign-sub (a b)
  (nth (sign-exc-order b) (nth (sign-exc-order a) *sub-map*)))


;;;;;;;;;;;;;;
;;;; LUB, c= relation
;;;;;;;;;;;;;;
    
(defparameter *sign-exc-graph*
  '((.none .neg .zero .pos .err)
    (.neg .non-pos .non-zero)
    (.zero .non-pos .non-neg)
    (.pos .non-neg .non-zero)
    (.non-pos .z) (.non-zero .z) (.non-neg .z)
    (.z .any)
    (.err .any)
    (.any)))
(defparameter *tt-exc-graph*
  '((.none .tt .ff .err)
    (.tt .t)
    (.ff .t)
    (.err .any)
    (.t .any)
    (.any)))

(defun abs-z (z)
  (if (eq z .err) .err
      (cond ((< z 0) .neg)
	    ((= z 0) .zero)
	    ((> z 0) .pos))))
(defun abs-tt (tt)
  (if (eq tt .err) .err
      (cond ((eq tt t) .tt)
	    ((eq tt nil) .ff))))
