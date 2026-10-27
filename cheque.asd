;;;; -- Cheque --

(asdf:defsystem #:cheque
  :description "Source rules, safe layout repairs, Mallet presets, and clean-compile warning checks."
  :author      "Lambda Symbolics OÜ"
  :license     "COLL-Attribution"
  :version     (:read-file-line "VERSION")
  :depends-on  (#:serapeum #:mallet #:cffi #:yason)
  :serial      t
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")
                              (:file "project")
                              (:file "syntax")
                              (:file "rules")
                              (:file "formatting")
                              (:file "definitions")
                              (:file "parinfer")
                              (:file "parinfer-build")
                              (:file "warnings")
                              (:file "mallet")
                              (:file "runner")
                              (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cheque/tests))))

(asdf:defsystem #:cheque/tests
  :description "Tests for cheque."
  :depends-on  (#:cheque #:fiveam)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:fiveam '#:run! (uiop:find-symbol* '#:cheque '#:cheque))))
