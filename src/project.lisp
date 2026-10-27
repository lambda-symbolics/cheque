(in-package #:cheque)

;;;; -- Projects --

;;; A project is a repository root and the settings its rules follow: which
;;; directories hold typed source, which package that source belongs to, which
;;; Mallet rules apply where, and which system definitions the clean compile
;;; loads. A preset supplies every setting; the project's cheque.sexp names a
;;; preset and overrides individual settings with the same keywords.

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))

(defparameter *project-presets*
  '(("lambda-symbolics"
     :package                    nil
     :package-definition         "src/package.lisp"
     :package-directories        ("src/" "tests/")
     :typed-directories          ("src/" "tests/")
     :mallet-rules               (:wrong-otherwise
                                  :mixed-optional-and-key
                                  :asdf-if-feature-keyword
                                  :asdf-secondary-system-name
                                  :asdf-operate-in-perform
                                  :asdf-reader-conditional
                                  :no-package-use
                                  :defpackage-interned-symbol
                                  :unused-imported-symbols
                                  :unused-local-nicknames
                                  :unused-loop-variables
                                  :redundant-progn
                                  :closing-paren-on-own-line
                                  :consecutive-blank-lines
                                  :missing-package-docstring
                                  :missing-variable-docstring
                                  :missing-struct-docstring
                                  :stale-suppression)
     :mallet-project-rules       (:no-ignore-errors
                                  :error-without-custom-condition)
     :mallet-project-directories ("src/")
     :system-definitions         ()
     :setup                      ".qlot/setup.lisp"
     :positional-limit           3
     :text-file-types            ("lisp" "asd" "org" "md")
     :excluded-paths             ("LICENSE.lisp")))
  "Named sets of project settings, each a complete plist of every setting.

The lambda-symbolics preset holds the rules Lambda Symbolics libraries and
Autolith follow. Its Mallet selection leaves out rules that contradict them:
one-package-per-file (one project package spans every file); no-eval,
runtime-intern, and runtime-unintern (live images evaluate code and create
symbols at run time); double-colon-access (platform adapters reach
implementation internals); needless-let* (independent LET* bindings are
aligned); asdf-component-strings and asdf-redundant-package-prefix (system
definitions name components with uninterned symbols and qualify ASDF's);
no-allow-other-keys (protocol methods accept their generic function's keys);
missing-else, missing-otherwise, progn-in-conditional, bare-float-literal, and
constant-naming (taste, or constants, which are banned outright); and
line-length, function-length, cyclomatic-complexity, and comment-ratio (limits
the rules do not set). Rules another check owns are left out as well:
trailing-whitespace, no-tabs, missing-final-newline, and missing-docstring
belong to cheque's own rules, and unused-variables and unused-local-functions
to the compiler warnings, which read conditionalized code correctly.
special-variable-naming misreads macro lambda lists, and the Coalton rules have
nothing to inspect.")

(defparameter *project-configuration-name* "cheque.sexp"
  "The file at a project's root that names its preset and overrides settings.")

(defclass project ()
  ((root
    :initarg       :root
    :reader        project-root
    :type          (option pathname)
    :documentation "The repository root, a directory pathname, or NIL for rules run on loose text.")
   (package
    :initarg       :package
    :reader        project-package
    :type          (option string)
    :documentation "The one package project source uses, or NIL when files may choose.")
   (package-definition
    :initarg       :package-definition
    :reader        project-package-definition
    :type          string
    :documentation "The repository-relative path of the one file that defines the package.")
   (package-directories
    :initarg       :package-directories
    :reader        project-package-directories
    :type          list
    :documentation "Directories whose files belong to the project package.")
   (typed-directories
    :initarg       :typed-directories
    :reader        project-typed-directories
    :type          list
    :documentation "Directories whose functions declare their types with Serapeum's ->.")
   (mallet-rules
    :initarg       :mallet-rules
    :reader        project-mallet-rules
    :type          list
    :documentation "The Mallet rules, as keywords, every tracked Lisp file follows.")
   (mallet-project-rules
    :initarg       :mallet-project-rules
    :reader        project-mallet-project-rules
    :type          list
    :documentation "The Mallet rules files below the Mallet project directories follow as well.")
   (mallet-project-directories
    :initarg       :mallet-project-directories
    :reader        project-mallet-project-directories
    :type          list
    :documentation "Directories whose files follow the Mallet project rules.

Standalone scripts and tests usually cannot rely on the project's conditions.")
   (system-definitions
    :initarg       :system-definitions
    :reader        project-system-definitions
    :type          list
    :documentation "Repository-relative .asd paths whose systems the clean compile loads.

Their source files are project source; other Lisp files, such as scripts, are not.")
   (setup
    :initarg       :setup
    :reader        project-setup
    :type          (option string)
    :documentation "The repository-relative file the clean compile loads first to find dependencies.")
   (positional-limit
    :initarg       :positional-limit
    :reader        project-positional-limit
    :type          (integer 0)
    :documentation "The most required and optional parameters a function takes before keywords.")
   (text-file-types
    :initarg       :text-file-types
    :reader        project-text-file-types
    :type          list
    :documentation "The file types whose tracked files the rules inspect.")
   (excluded-paths
    :initarg       :excluded-paths
    :reader        project-excluded-paths
    :type          list
    :documentation "Repository-relative paths of tracked files the rules never inspect, such as generated text.")
   (preset
    :initarg       :preset
    :reader        project-preset
    :type          string
    :documentation "The name of the preset the settings start from."))
  (:documentation "A repository and the settings its source rules follow."))

(defvar *project* nil
  "The project whose settings rules consult, or NIL for the default preset.

The runner binds it for each check.")


;;;; -- Public Functions --

(-> project-create (&rest t) project)
(defun project-create (&rest settings &key (preset "lambda-symbolics") root &allow-other-keys)
  "Return a project at ROOT starting from PRESET's settings, overridden by SETTINGS.

Signal PROJECT-CONFIGURATION-ERROR for an unknown preset or setting."
  (let ((defaults (rest (assoc preset *project-presets* :test #'string=))))
    (unless defaults
      (error 'project-configuration-error
             :message (format nil "Unknown cheque preset ~S; known presets are ~{~A~^, ~}."
                              preset (mapcar #'first *project-presets*))))
    (loop for (key) on settings by (partial #'nthcdr 2)
          unless (or (member key '(:preset :root))
                     (nth-value 2 (get-properties defaults (list key))))
            do (error 'project-configuration-error
                      :message (format nil "Unknown cheque setting ~S." key)))
    (let ((merged (copy-list defaults)))
      (loop for (key value) on settings by (partial #'nthcdr 2)
            unless (member key '(:preset :root))
              do (setf (getf merged key) value))
      (apply #'make-instance 'project
             :root   (and root (uiop:ensure-directory-pathname root))
             :preset preset
             merged))))

(-> project-read (pathname &key (:overrides list)) project)
(defun project-read (root &key overrides)
  "Return the project at ROOT, configured by its cheque.sexp when it has one.

OVERRIDES is a plist of settings that take precedence over the file's.
Signal PROJECT-CONFIGURATION-ERROR when the file is not one readable plist."
  (let* ((root     (uiop:ensure-directory-pathname root))
         (pathname (merge-pathnames *project-configuration-name* root)))
    (if (probe-file pathname)
        (let ((settings (handler-case
                            (with-open-file (stream pathname :external-format ':utf-8)
                              (let ((*read-eval* nil)
                                    (*package*   (find-package '#:cheque)))
                                (read stream)))
                          (error (condition)
                            (error 'project-configuration-error
                                   :message (format nil "~A cannot be read: ~A"
                                                    (uiop:native-namestring pathname) condition))))))
          (unless (and (listp settings) (evenp (length settings))
                       (loop for (key) on settings by (partial #'nthcdr 2)
                             always (keywordp key)))
            (error 'project-configuration-error
                   :message (format nil "~A must hold one plist of settings."
                                    (uiop:native-namestring pathname))))
          (apply #'project-create :root root (append settings overrides)))
        (apply #'project-create :root root overrides))))

(-> project-current () project)
(defun project-current ()
  "Return the project being checked, or a rootless one with the default preset."
  (or *project* (project-create)))


;;;; -- Conditions --

(define-condition cheque-error (error)
  ((message
    :initarg       :message
    :reader        cheque-error-message
    :type          string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected cheque failures.")
  (:report (lambda (condition stream)
             (write-string (cheque-error-message condition) stream))))

(define-condition project-configuration-error (cheque-error)
  ()
  (:documentation "A project's cheque.sexp names an unknown preset or setting, or cannot be read."))
