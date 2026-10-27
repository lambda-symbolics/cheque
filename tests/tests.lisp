(in-package #:cheque)

;;;; -- Test Support --

(fiveam:def-suite cheque
  :description "Cheque's rules, repairs, runner, and process boundaries.")

(fiveam:in-suite cheque)

(defmacro with-test-root ((root) &body body)
  "Run BODY with ROOT bound to a new empty directory, deleted on every exit.

ROOT is a variable name; BODY's values are returned."
  (let ((directory (gensym "DIRECTORY")))
    `(let* ((,directory (cheque-tests--make-root))
            (,root      ,directory))
       (unwind-protect
            (progn ,@body)
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist ':ignore)))))

(-> cheque-tests--make-root () pathname)
(defun cheque-tests--make-root ()
  "Create and return a new private directory below the host's temporary directory."
  (loop for directory = (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "cheque-test-~36R" (random (expt 36 12)))
                                          (uiop:temporary-directory)))
        when (nth-value 1 (ensure-directories-exist directory))
          return (truename directory)))

(-> test-assert (t string) boolean)
(defun test-assert (value description)
  "Record a FiveAM check that VALUE is true, described by DESCRIPTION."
  (fiveam:is-true value "~A" description)
  (and value t))

(defclass source-check-tests--appending-rule (source-rule)
  ((layout-only-p
    :initarg       :layout-only-p
    :reader        source-check-tests--appending-rule-layout-only-p
    :documentation "Whether the rule claims its repair changes layout only."))
  (:default-initargs
   :name        "appending"
   :description "Appends a form to every file.")
  (:documentation "A rule whose repair adds a form, to exercise the runner's layout guard."))

(defmethod source-rule-fix ((rule source-check-tests--appending-rule) (file source-file))
  "Return FILE's text with one more form."
  (declare (ignore rule))
  (concatenate 'string (source-file-text file) "(appended)" (string #\Newline)))

(defmethod source-rule-layout-only-p ((rule source-check-tests--appending-rule))
  "Return what RULE was created to claim."
  (source-check-tests--appending-rule-layout-only-p rule))


;;;; -- Helpers --

(-> source-check-tests--text (list) string)
(defun source-check-tests--text (lines)
  "Return LINES joined by newlines, ending in one newline."
  (format nil "~{~A~%~}" lines))

(-> source-check-tests--file (string list) source-file)
(defun source-check-tests--file (path lines)
  "Return a source file at PATH holding LINES."
  (source-file-create path (source-check-tests--text lines)))

(-> source-check-tests--finding-lines (symbol source-file) (or list (eql :not-applicable)))
(defun source-check-tests--finding-lines (class file)
  "Return the lines of the findings a fresh rule of CLASS reports in FILE."
  (let ((rule (make-instance class)))
    (if (source-rule-applies-p rule file)
        (mapcar #'source-finding-line (source-rule-findings rule file))
        ':not-applicable)))

(-> source-check-tests--write (pathname string string) null)
(defun source-check-tests--write (root path text)
  "Write TEXT to the file at repository-relative PATH under ROOT."
  (let ((pathname (merge-pathnames path root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                     :external-format ':utf-8)
      (write-string text stream)))
  nil)


;;;; -- Syntax --

(fiveam:test test-source-syntax-positions
  "Read nodes and comments with their kinds and positions, through reader syntax."
  (multiple-value-bind (nodes comments)
      (source-parse (source-check-tests--text
                     '("(alpha #\\( \"say \\\"hi\\\"\" |odd name| #+sbcl beta) ; trailing"
                       "#| block"
                       "   comment |#"
                       "  #'gamma")))
    (test-assert (= (length nodes) 2) "two top-level forms are read")
    (test-assert (= (length comments) 2) "the line comment and the block comment are read")
    (let* ((form     (first nodes))
           (children (source-node-children form)))
      (test-assert (eq (source-node-kind form) ':list) "the first form is a list")
      (test-assert (equal (source-node-operator form) "alpha") "its operator is its first symbol")
      (test-assert (= (length children) 5) "a character, string, and escaped symbol are one node each")
      (test-assert (source-node-string-p (third children)) "escaped quotes stay inside the string")
      (test-assert (equal (source-node-text (fourth children)) "|odd name|")
                   "an escaped symbol keeps its bars")
      (test-assert (and (equal (source-node-text (fifth children)) "beta")
                        (equal (first (first (source-node-prefixes (fifth children)))) "#+"))
                   "a feature expression prefixes the form it guards"))
    (let ((block (second comments))
          (form  (second nodes)))
      (test-assert (and (= (source-comment-line block) 2) (= (source-comment-end-line block) 3))
                   "a block comment spans its lines")
      (test-assert (and (= (source-node-line form) 4)
                        (= (source-node-prefix-column form) 2)
                        (= (source-node-column form) 4))
                   "a prefixed form records where its prefix and its body start"))))


;;;; -- Rules --

(fiveam:test test-source-rule-findings
  "Report each rule's findings at the lines that break it, and nothing in conforming text."
  (let ((em-dash   (string (code-char #x2014)))
        (tab       (string #\Tab))
        (*project* (project-create :package "example")))
    (dolist (case `((source-trailing-whitespace-rule "notes.org" ("* Notes" "text ") (2))
                    (source-trailing-whitespace-rule "notes.org" ("* Notes" "text") ())
                    (source-tab-rule "a.lisp" ("(a" ,(concatenate 'string tab "b)")) (2))
                    (source-tab-rule "a.lisp" ("\"a" ,(concatenate 'string tab "b\"")) (2))
                    (source-em-dash-rule "notes.md" ("# Notes" ,(format nil "a ~A b" em-dash)) (2))
                    (source-section-header-rule "a.lisp" (";;;; Title" "(a)") (1))
                    (source-section-header-rule "a.lisp" (";;;; -- Title --" "(a)") ())
                    (source-package-rule "src/a.lisp" ("(defpackage #:other)") (1 1))
                    (source-package-rule "src/a.lisp" ("(in-package #:example)") ())
                    (source-package-rule "script/a.lisp" ("(defpackage #:other)") :not-applicable)
                    (source-constant-rule "script/a.lisp" ("(defconstant +limit+ 1)") (1))
                    (source-constant-rule "a.lisp" ("(alexandria:define-constant +limit+ 1)") (1))
                    (source-todo-rule "a.lisp" ("(a)" ";; TODO finish") (2))
                    (source-todo-rule "a.lisp" ("\"TODO in a string\"") ())
                    (source-documentation-rule "a.lisp" ("(defun f (x)" "  x)") (1))
                    (source-documentation-rule "a.lisp" ("(defun f (x)" "  \"Return X.\"" "  x)") ())
                    (source-documentation-rule "a.lisp"
                                               ("(defclass c ()" "  ((slot :initarg :slot))"
                                                "  (:documentation \"C.\"))")
                                               (2))
                    (source-generic-declaration-rule "src/a.lisp" ("(-> g (t) t)" "(defgeneric g (x))")
                                                     (1))
                    (source-function-declaration-rule "src/a.lisp" ("(defun f (x)" "  \"X.\"" "  x)")
                                                      (1))
                    (source-function-declaration-rule "src/a.lisp"
                                                      ("(-> f (t) t)" "(defun f (x)" "  \"X.\"" "  x)")
                                                      ())
                    (source-function-declaration-rule "script/a.lisp" ("(defun f (x)" "  x)")
                                                      :not-applicable)
                    (source-positional-parameters-rule "a.lisp" ("(defun f (a b c d)" "  a)") (1))
                    (source-positional-parameters-rule "a.lisp" ("(defun f (a b c &key d)" "  a)") ())
                    (source-positional-parameters-rule "a.lisp" ("(defgeneric g (a b c d))") (1))
                    (source-positional-parameters-rule "a.lisp" ("(defmethod g ((a t) b c d)" "  a)") ())
                    (source-list-accessor-rule "a.lisp" ("(list (car x)" "      (cdr x))") (1 2))
                    (source-format-percent-rule "a.lisp" ("(format nil \"100%%\")") (1))
                    (source-format-percent-rule "a.lisp" ("(list \"100%%\")") ())
                    (source-format-percent-rule "a.lisp" ("(format nil \"~%%TEMP%\")") ())
                    (source-top-level-spacing-rule "a.lisp" ("(defun a ())" "(defun b ())") (2))
                    (source-top-level-spacing-rule "a.lisp" ("(-> a () t)" "(defun a ())" ""
                                                             "(defun b ())")
                                                   ())
                    (source-top-level-spacing-rule "a.lisp" (";;;; -- One --" "" "(defun a ())" ""
                                                             ";;;; -- Two --")
                                                   (5))
                    (source-clause-layout-rule "a.lisp" ("(cond (x y)" "      (t" "       nil))") (1))
                    (source-local-function-spacing-rule "a.lisp" ("(labels ((a () 1)"
                                                                  "         (b () 2))"
                                                                  "  (a))")
                                                        (2))
                    (source-alignment-rule "a.lisp" ("(let ((a 1)" "      (bb 2))" "  a)") (1))
                    (source-alignment-rule "a.lisp" ("(f :a 1" "   :bb 2)") (1))
                    (source-alignment-rule "a.lisp" ("(f :a 1 :bb 2" "   :c 3)") ())))
      (destructuring-bind (class path lines expected) case
        (test-assert (equal (source-check-tests--finding-lines class (source-check-tests--file path lines))
                            expected)
                     (format nil "~(~A~) reports ~S in ~A ~S" class expected path lines)))))
  (test-assert (equal (source-check-tests--finding-lines
                       'source-final-newline-rule (source-file-create "a.lisp" "(a)"))
                      '(1))
               "a missing final newline is reported")
  (test-assert (equal (source-check-tests--finding-lines
                       'source-final-newline-rule
                       (source-file-create "a.lisp" (format nil "(a)~%~%")))
                      '(3))
               "blank lines after the last form are reported")
  (let ((rule (make-instance 'source-positional-parameters-rule))
        (file (source-check-tests--file "a.lisp"
                                        '("(defgeneric g (a b c d))"
                                          "(defmethod g ((a t) (b t) (c t) (d t))"
                                          "  a)"
                                          "(defgeneric h (a b c d))"
                                          "(defmethod h ((a t) b c d)"
                                          "  a)"))))
    (source-rule-prepare rule file)
    (test-assert (equal (mapcar #'source-finding-line (source-rule-findings rule file)) '(4))
                 "a generic function may keep every parameter its methods specialize positional")))

(fiveam:test test-source-rule-repairs
  "Repair each repairable finding, leaving text the rule then accepts."
  (dolist (case `((source-trailing-whitespace-rule ("(a) " "(b)") ("(a)" "(b)"))
                  (source-tab-rule (,(format nil "(a~C1)" #\Tab)) ("(a      1)"))
                  (source-top-level-spacing-rule ("(defun a ())" "(defun b ())")
                                                 ("(defun a ())" "" "(defun b ())"))
                  (source-top-level-spacing-rule ("(defun a ())" "" "" "" "(defun b ())")
                                                 ("(defun a ())" "" "(defun b ())"))
                  (source-clause-layout-rule ("(cond (x y" "         z))")
                                             ("(cond (x" "       y" "       z))"))
                  (source-local-function-spacing-rule ("(labels ((a () 1)" "         (b () 2))" "  (a))")
                                                      ("(labels ((a () 1)" ""
                                                       "         (b () 2))" "  (a))"))
                  (source-alignment-rule ("(let ((a (f 1" "           2))" "      (bb 2))" "  a)")
                                         ("(let ((a  (f 1" "            2))" "      (bb 2))" "  a)"))
                  (source-alignment-rule ("(make-instance 'c :a 1" "                  :bb 2)")
                                         ("(make-instance 'c :a  1" "                  :bb 2)"))
                  (source-list-accessor-rule ("(list (car x) (cadr x))") ("(list (first x) (second x))"))
                  (source-format-percent-rule ("(format nil \"100%%\")") ("(format nil \"100%\")"))
                  (source-format-percent-rule ("(format nil \"100%% ~%%TEMP%\")") ("(format nil \"100% ~%%TEMP%\")"))
                  (source-generic-declaration-rule ("(-> g (t) t)" "(defgeneric g (x))")
                                                   ("(defgeneric g (x))"))))
    (destructuring-bind (class lines expected) case
      (let* ((rule  (make-instance class))
             (path  (if (eq class 'source-generic-declaration-rule) "src/a.lisp" "a.lisp"))
             (fixed (source-rule-fix rule (source-check-tests--file path lines))))
        (test-assert (equal fixed (source-check-tests--text expected))
                     (format nil "~(~A~) repairs ~S" class lines))
        (when fixed
          (test-assert (null (source-rule-findings rule (source-file-create path fixed)))
                       (format nil "~(~A~) accepts its own repair of ~S" class lines))))))
  (test-assert (equal (source-rule-fix (make-instance 'source-final-newline-rule)
                                       (source-file-create "a.lisp" (format nil "(a)~%~%~%")))
                      (format nil "(a)~%"))
               "blank lines after the last form are removed")
  (test-assert (null (source-rule-fix (make-instance 'source-em-dash-rule)
                                      (source-check-tests--file "notes.md" '("text"))))
               "rules without a repair return nothing"))


;;;; -- Runner --

(fiveam:test test-source-check-repair-guard
  "Refuse a layout repair that changes content, but accept the same change from a rewriting rule."
  (let ((file (source-check-tests--file "a.lisp" '("(a)"))))
    (test-assert (handler-case
                     (progn
                       (source-check-repair file (list (make-instance 'source-check-tests--appending-rule
                                                                      :layout-only-p t)))
                       nil)
                   (source-check-unsafe-repair (condition)
                     (equal (source-check-unsafe-repair-path condition) "a.lisp")))
                 "a layout repair that adds a form is refused")
    (multiple-value-bind (text changed)
        (source-check-repair file (list (make-instance 'source-check-tests--appending-rule
                                                       :layout-only-p nil)))
      (test-assert (equal text (source-check-tests--text '("(a)" "(appended)")))
                   "a rewriting rule may change content")
      (test-assert (equal changed '("appending")) "the repair reports the rule that changed the file"))))

(fiveam:test test-source-check-repository-run
  "Check tracked files only, repair them in place, and judge declarations across files."
  (with-test-root (root)
    (source-check-tests--write root "src/a.lisp"
                               (source-check-tests--text
                                '("(in-package #:example)" ""
                                  "(-> shared () t)" "(defun shared ()" "  \"Return T.\""
                                  "  (let ((a 1)" "        (bb 2))" "    (list a bb)))")))
    (source-check-tests--write root "src/b.lisp"
                               (source-check-tests--text
                                '("(in-package #:example)" "" "(-> shared () t)")))
    (source-check-tests--write root "src/untracked.lisp" "(defconstant +limit+ 1)")
    (uiop:run-program (list "git" "-C" (uiop:native-namestring root) "init" "--quiet"))
    (uiop:run-program (list "git" "-C" (uiop:native-namestring root) "add" "src/a.lisp" "src/b.lisp"))
    (let* ((report   (make-string-output-stream))
           (findings (source-check-run (project-create :root root :package "example")
                                      :fix t :stream report)))
      (test-assert (equal (mapcar (lambda (finding)
                                    (list (source-finding-rule finding) (source-finding-path finding)
                                          (source-finding-line finding)))
                                  findings)
                          '(("declaration-uniqueness" "src/a.lisp" 3)
                            ("declaration-uniqueness" "src/b.lisp" 3)))
                   "only the declarations repeated across files remain, and untracked files are ignored")
      (test-assert (search "repaired src/a.lisp (alignment)" (get-output-stream-string report))
                   "each repaired file is reported")
      (test-assert (search "(let ((a  1)" (uiop:read-file-string (merge-pathnames "src/a.lisp" root)))
                   "the repair is written in place"))))


;;;; -- Native and Process Boundaries --

(fiveam:test test-source-parinfer-missing-library
  "Report a missing parinfer library as a structured failure that names its builder."
  (with-test-root (root)
    (let ((library (merge-pathnames (source-parinfer-library-name) root)))
      (test-assert (handler-case
                       (progn
                         (source-parinfer-run library "(a)")
                         nil)
                     (source-parinfer-error (condition)
                       (and (equal (source-parinfer-error-library condition) library)
                            (search "SOURCE-PARINFER-LIBRARY-ENSURE" (cheque-error-message condition))
                            t)))
                   "a missing library is a parinfer error"))))

(fiveam:test test-source-mallet-findings
  "Report Mallet's selected rules by their names, scoped by path, and honor reasoned suppressions."
  (dolist (case '(("script/a.lisp" ("(loop for (key value) on plist" "      collect key)")
                   (("mallet/unused-loop-variables" 1)))
                  ("src/a.lisp" ("(list 1)" "(ignore-errors (f))")
                   (("mallet/no-ignore-errors" 2)))
                  ("tests/a.lisp" ("(ignore-errors (f))") ())
                  ("src/a.lisp" ("; mallet:suppress no-ignore-errors -- best-effort cleanup"
                                 "(ignore-errors (f))")
                   ())
                  ("src/a.lisp" ("(eval (intern name))") ())
                  ("example.asd" ("(defsystem \"example/tests\")") ())))
    (destructuring-bind (path lines expected) case
      (test-assert (equal (mapcar (lambda (finding)
                                    (list (source-finding-rule finding) (source-finding-line finding)))
                                  (source-rule-findings (make-instance 'source-mallet-rule)
                                                        (source-check-tests--file path lines)))
                          expected)
                   (format nil "Mallet reports ~S in ~A ~S" expected path lines)))))

(fiveam:test test-source-warning-run
  "Turn the compiling process's records into distinct findings, and report a failed compile."
  (with-test-root (root)
    (let ((command (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--script")))
      (source-check-tests--write
       root "script/compile-warnings.lisp"
       (source-check-tests--text
        '("(require :asdf)"
          "(with-open-file (stream (third (uiop:command-line-arguments)) :direction :output)"
          "  (prin1 '((:path \"src/a.lisp\" :line 3 :type \"style-warning\" :message \"unused"
          "   variable\")"
          "          (:path \"src/a.lisp\" :line 3 :type \"style-warning\" :message \"unused variable\")"
          "          (:path nil :line nil :type \"warning\" :message \"undefined function\"))"
          "         stream))")))
      (let ((findings (source-warning-run-finish
                       (source-warning-run-start (project-create :root root)
                                                 :command   command
                                                 :script    (merge-pathnames "script/compile-warnings.lisp" root)
                                                 :directory (merge-pathnames "run/" root)))))
        (test-assert (equal (mapcar (lambda (finding)
                                      (list (source-finding-path finding) (source-finding-line finding)
                                            (source-finding-message finding)))
                                    findings)
                            '(("src/a.lisp" 3 "style-warning: unused variable")
                              ("(end of compilation)" 1 "warning: undefined function")))
                     "records become one finding per distinct warning"))
      (delete-file (merge-pathnames "script/compile-warnings.lisp" root))
      (test-assert (handler-case
                       (progn
                         (source-warning-run-finish
                          (source-warning-run-start (project-create :root root)
                                                    :command   command
                                                    :script    (merge-pathnames "script/compile-warnings.lisp" root)
                                                    :directory (merge-pathnames "failed/" root)))
                         nil)
                     (source-warning-run-failed ()
                       t))
                   "a compile that cannot finish is a failure, not a clean result")
      (source-check-tests--write
       root "script/compile-warnings.lisp"
       (source-check-tests--text
        '("(require :asdf)"
          "(write-string (make-string 20000 :initial-element #\\x))"
          "(write-line \"compiler-failure-evidence\")"
          "(uiop:quit 1)")))
      (let* ((directory (merge-pathnames "diagnostic/" root))
             (failure
               (handler-case
                   (source-warning-run-finish
                    (source-warning-run-start (project-create :root root)
                                              :command   command
                                              :script    (merge-pathnames "script/compile-warnings.lisp" root)
                                              :directory directory))
                 (source-warning-run-failed (condition)
                   condition))))
        (test-assert (typep failure 'source-warning-run-failed)
                     "failed compilation returns its typed condition")
        (uiop:delete-directory-tree directory :validate t)
        (let ((report (princ-to-string failure)))
          (test-assert (search "compiler-failure-evidence" report)
                       "compiler evidence survives run-directory cleanup")
          (test-assert (< (length report) 17000)
                       "large compiler output is bounded in the failure report"))))))

(fiveam:test test-source-warning-ownership
  "Record project compile and load warnings without claiming nested vendored systems."
  (with-test-root (root)
    (source-check-tests--write
     root ".qlot/setup.lisp"
     (source-check-tests--text
      '("(asdf:initialize-source-registry"
         " `(:source-registry (:directory ,(namestring (uiop:pathname-parent-directory-pathname"
         "                                            (uiop:pathname-directory-pathname *load-truename*))))"
         "   :ignore-inherited-configuration))"
         "(asdf:load-asd (merge-pathnames \"vendor/vendor.asd\" *load-truename*))")))
    (source-check-tests--write
     root ".qlot/vendor/vendor.asd"
     "(asdf:defsystem \"vendor\" :components ((:file \"dependency\")))")
    (source-check-tests--write
     root "example.asd"
     (source-check-tests--text
      '("(asdf:defsystem \"example\" :depends-on (\"vendor\")"
        "  :components ((:module \"src\" :serial t"
        "                :components ((:file \"project\") (:file \"end\")))))")))
    (dolist (entry '(("src/project.lisp" "project")
                     (".qlot/vendor/dependency.lisp" "dependency")))
      (destructuring-bind (path label) entry
        (source-check-tests--write
         root path
         (format nil "(eval-when (:compile-toplevel)~%~
                        (warn 'sb-int:simple-style-warning :format-control ~S))~%~
                      (eval-when (:load-toplevel) (warn ~S))~%"
                 (concatenate 'string label " compile marker")
                 (concatenate 'string label " load marker")))))
    (source-check-tests--write
     root "src/end.lisp"
     "(let ((*compile-file-truename* nil) (*load-truename* nil))
        (warn \"unattributed diagnostic marker\"))")
    ;; On a case-insensitive filesystem, request an existing directory with a
    ;; different spelling. Its new FASLs acquire the on-disk directory case.
    (let* ((existing  (ensure-directories-exist (merge-pathnames "Run/" root)))
           (requested (merge-pathnames "run/" root))
           (directory (if (probe-file requested) requested existing))
           (findings  (source-warning-run-finish
                       (source-warning-run-start
                        (project-create :root               root
                                        :system-definitions '("example.asd"))
                        :command   (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--script")
                        :directory directory)))
           (project   (remove-if-not
                       (lambda (finding)
                         (equal (source-finding-path finding) "src/project.lisp"))
                       findings)))
      (dolist (marker '("project compile marker" "project load marker"))
        (test-assert (some (lambda (finding)
                             (search marker (source-finding-message finding)))
                           project)
                     (format nil "the project warning is attributed to its source: ~A" marker)))
      (test-assert (notany (lambda (finding)
                             (search "dependency" (source-finding-message finding)))
                           findings)
                   "nested dependency source and translated FASL warnings are excluded")
      (test-assert (some (lambda (finding)
                           (and (equal (source-finding-path finding) "(end of compilation)")
                                (search "unattributed diagnostic marker"
                                        (source-finding-message finding))))
                         findings)
                   "diagnostics without a current file are retained"))))


;;;; -- Projects --

(fiveam:test test-project-configuration
  "Start from the preset, apply cheque.sexp's overrides, and refuse what it cannot use."
  (dolist (case '((nil (nil 3 "lambda-symbolics"))
                  ("(:package \"example\" :positional-limit 4)" ("example" 4 "lambda-symbolics"))
                  ("(:preset \"lambda-symbolics\" :package \"other\")" ("other" 3 "lambda-symbolics"))
                  ("(:preset \"unknown\")" :error)
                  ("(:packages \"example\")" :error)
                  ("(:package \"example\" :positional-limit)" :error)
                  ("(:package \"example\"" :error)
                  ("#.(error \"evaluated\")" :error)))
    (destructuring-bind (text expected) case
      (with-test-root (root)
        (when text
          (source-check-tests--write root "cheque.sexp" text))
        (test-assert (equal (handler-case
                                (let ((project (project-read root)))
                                  (list (project-package project)
                                        (project-positional-limit project)
                                        (project-preset project)))
                              (project-configuration-error ()
                                ':error))
                            expected)
                     (format nil "cheque.sexp ~S configures ~S" text expected))))))

(fiveam:test test-project-package-optional
  "Leave package choice to each file when the project names no package."
  (let ((*project* (project-create)))
    (test-assert (not (source-rule-applies-p (make-instance 'source-package-rule)
                                             (source-check-tests--file "src/a.lisp"
                                                                       '("(in-package #:any)"))))
                 "the package rule does not apply without a project package")))
