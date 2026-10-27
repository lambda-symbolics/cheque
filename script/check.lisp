;;;; -- Cheque's Own Check --

;;; Run cheque's tests, then check cheque's source with cheque. Arguments go to
;;; CHEQUE:MAIN; with --fix the tests are skipped.
;;;
;;; The locked dependencies install as a qlot project in the per-user cache,
;;; named by a digest of the files that define it, never below the checkout:
;;; ASDF's default source registry searches trees such as ~/common-lisp, and
;;; would find Mallet's vendored ASDF inside a .qlot directory there.
;;;
;;; Usage: sbcl --script script/check.lisp [--fix] [--no-warnings] [--no-parinfer]

(require :asdf)
(require :sb-md5)

(defparameter *check-tool-files* '("qlfile" "qlfile.lock" "cheque.asd")
  "The files that define cheque's development qlot project.")

(defun check--tool-root (root)
  "Return the cached qlot project for ROOT's current tool definition."
  (let ((digest (sb-md5:md5sum-string
                 (with-output-to-string (stream)
                   (dolist (name *check-tool-files*)
                     (write-string (uiop:read-file-string (merge-pathnames name root)) stream)
                     (write-char #\Nul stream)))
                 :external-format ':utf-8)))
    (uiop:xdg-cache-home "cheque/tools/" (format nil "~(~{~2,'0x~}~)/" (coerce digest 'list)))))

(defun check--ensure-tools (root)
  "Install ROOT's locked development tools unless they are installed; return their setup file."
  (let* ((tools (check--tool-root root))
         (stamp (merge-pathnames "installed" tools)))
    (unless (probe-file stamp)
      (format t "~&Installing cheque's locked development tools.~%")
      (finish-output)
      (dolist (name *check-tool-files*)
        (uiop:copy-file (merge-pathnames name root)
                        (ensure-directories-exist (merge-pathnames name tools))))
      (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
      (uiop:symbol-call '#:ql '#:quickload :qlot :silent t)
      (progv (list (uiop:find-symbol* '#:*project-root* '#:qlot)) (list tools)
        (uiop:with-current-directory (tools)
          (uiop:symbol-call '#:qlot '#:install)))
      (with-open-file (stream stamp :direction ':output :if-exists ':supersede)
        (write-line "installed" stream)))
    (merge-pathnames ".qlot/setup.lisp" tools)))

(let* ((root      (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname (truename *load-truename*))))
       (arguments (uiop:command-line-arguments))
       (setup     (check--ensure-tools root)))
  (load setup)
  (asdf:load-asd (merge-pathnames "cheque.asd" root))
  (asdf:load-system "cheque/tests")
  (let ((tests-passed-p (or (member "--fix" arguments :test #'string=)
                            (uiop:symbol-call '#:fiveam '#:run!
                                              (uiop:find-symbol* '#:cheque '#:cheque))))
        (status         (uiop:symbol-call '#:cheque '#:main arguments
                                          :root     root
                                          :settings (list :setup (uiop:native-namestring setup)))))
    (uiop:quit (if (and tests-passed-p (zerop status)) 0 (max status 1)))))
