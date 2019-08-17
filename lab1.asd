(asdf:defsystem "lab1"
  :depends-on (:cl-algebraic-data-type :closer-mop :alexandria
	       :clometa)
  :serial t
  :components ((:file "package")
	       (:file "abs")
	       (:file "lab1")
	       (:file "parser")
	       (:file "lub")
	       (:file "pprinter"))
  :in-order-to ((test-op (test-op "lab1/test"))))
(asdf:defsystem "lab1/test"
  :depends-on ("lab1" "fiveam")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run! :lab1)))
