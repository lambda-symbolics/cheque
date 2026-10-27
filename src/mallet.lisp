(in-package #:cheque)

;;;; -- Mallet --

;;; Mallet (https://github.com/fukamachi/mallet) reports mistakes cheque's own
;;; rules do not look for. Only the Mallet rules the project's settings select
;;; run, and their findings are reported, never repaired. The
;;; lambda-symbolics preset in *PROJECT-PRESETS* records which rules it leaves
;;; out and why.
;;;
;;; Code that breaks a rule on purpose says why where it does, with a comment
;;; such as "; mallet:suppress no-ignore-errors -- best-effort cleanup".

(defclass source-mallet-rule (source-rule)
  ()
  (:default-initargs
   :name        "mallet"
   :description "The project's selection of Mallet's rules, each finding named after its Mallet rule.")
  (:documentation "Mallet's linter, run over each Lisp file."))


;;;; -- Methods --

(defmethod source-rule-findings ((rule source-mallet-rule) (file source-file))
  "Report the violations Mallet's selected rules find in FILE."
  (declare (ignore rule))
  (mapcar (lambda (violation)
            (source-mallet--finding file violation))
          (source-mallet-violations file)))


;;;; -- Public Functions --

(-> source-mallet-configuration (string) mallet/config:config)
(defun source-mallet-configuration (path)
  "Return the Mallet configuration for the file at repository-relative PATH."
  (let ((project (project-current)))
    (mallet/config:make-config
     :rules (mapcar #'mallet/rules:make-rule
                    (append (project-mallet-rules project)
                            (and (source-mallet--project-path-p path)
                                 (project-mallet-project-rules project)))))))

(-> source-mallet-violations (source-file) list)
(defun source-mallet-violations (file)
  "Return the violations Mallet reports in FILE under the rules its path follows.

Mallet reads files, so FILE's text is linted from a file of the same name in a
private temporary directory; some rules compare definitions with that name."
  (let ((directory (source-mallet--temporary-directory)))
    (unwind-protect
         (let ((pathname (merge-pathnames (file-namestring (source-file-path file)) directory)))
           (with-open-file (stream pathname :direction ':output :external-format ':utf-8)
             (write-string (source-file-text file) stream))
           (mallet/engine:lint-file pathname
                                    :config (source-mallet-configuration (source-file-path file))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist ':ignore))))


;;;; -- Private Functions --

(-> source-mallet--temporary-directory () pathname)
(defun source-mallet--temporary-directory ()
  "Create and return a new private directory below the host's temporary directory."
  (loop for directory = (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "cheque-mallet-~36R" (random (expt 36 12)))
                                          (uiop:temporary-directory)))
        when (nth-value 1 (ensure-directories-exist directory))
          return directory))

(-> source-mallet--project-path-p (string) boolean)
(defun source-mallet--project-path-p (path)
  "Return true when repository-relative PATH is project source."
  (and (some (lambda (directory)
               (uiop:string-prefix-p directory path))
             (project-mallet-project-directories (project-current)))
       t))

(-> source-mallet--finding (source-file mallet/violation:violation) source-finding)
(defun source-mallet--finding (file violation)
  "Return VIOLATION, which Mallet reported in FILE, as a finding named after its rule."
  (make-instance 'source-finding
                 :rule    (format nil "mallet/~(~A~)" (mallet/violation:violation-rule violation))
                 :path    (source-file-path file)
                 :line    (max 1 (or (mallet/violation:violation-line violation) 1))
                 :column  (max 0 (or (mallet/violation:violation-column violation) 0))
                 :message (mallet/violation:violation-message violation)))
