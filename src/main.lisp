(in-package #:cheque)

;;;; -- Command Entry --

;;; MAIN runs a project's complete source check the way a ./script/check
;;; would: the clean compile starts in its own process, the source rules and
;;; parinfer run meanwhile, and every finding is reported at the end. A
;;; project with its own test runner can call the pieces directly instead, as
;;; Autolith does to overlap the clean compile with its test workers.

(defparameter *main-usage*
  "Usage: cheque [--fix] [--no-warnings] [--no-parinfer]

  --fix          Repair what the rules can repair, then report the rest.
  --no-warnings  Skip the clean compile that reports compiler warnings.
  --no-parinfer  Skip parinfer, which needs Git and Cargo to build once."
  "The text MAIN prints for --help and for an unknown argument.")


;;;; -- Public Functions --

(-> main (list &key (:root pathname) (:command list) (:settings list) (:stream stream))
    (integer 0 2))
(defun main (arguments &key (root (uiop:getcwd)) (command (main--default-command)) settings
                            (stream *standard-output*))
  "Check the project at ROOT as ARGUMENTS select and return the exit status.

SETTINGS is a plist of project settings that take precedence over cheque.sexp.

COMMAND runs the clean compile's Lisp script, an SBCL command line ending in
--script by default. The status is 0 when nothing was found, 1 when findings
remain, and 2 for unusable arguments or configuration."
  (block nil
    (let ((fix-p      nil)
          (warnings-p t)
          (parinfer-p t))
      (dolist (argument arguments)
        (cond
          ((string= argument "--fix")
           (setf fix-p      t
                 warnings-p nil))
          ((string= argument "--no-warnings")
           (setf warnings-p nil))
          ((string= argument "--no-parinfer")
           (setf parinfer-p nil))
          ((member argument '("--help" "-h") :test #'string=)
           (format stream "~&~A~%" *main-usage*)
           (return 0))
          (t
           (format stream "~&Unknown argument ~S.~%~A~%" argument *main-usage*)
           (return 2))))
      (handler-case
          (main--run (project-read root :overrides settings)
                     :fix        fix-p
                     :warnings-p warnings-p
                     :parinfer-p parinfer-p
                     :command    command
                     :stream     stream)
        (cheque-error (condition)
          (format stream "~&~A~%" condition)
          2)))))


;;;; -- Private Functions --

(-> main--default-command () list)
(defun main--default-command ()
  "Return the command that runs a Lisp script with this image's SBCL runtime."
  (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--script"))

(-> main--run
    (project &key (:fix boolean) (:warnings-p boolean) (:parinfer-p boolean)
                  (:command list) (:stream stream))
    (integer 0 1))
(defun main--run (project &key fix warnings-p parinfer-p command stream)
  "Check PROJECT, report every finding to STREAM, and return 0 when none remain."
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "cheque-~36R" (random (expt 36 12)))
                                      (uiop:temporary-directory))))
         (run       (and warnings-p
                         (project-system-definitions project)
                         (source-warning-run-start project
                                                   :command   command
                                                   :directory directory))))
    (unwind-protect
         (let ((findings (source-check-run project
                                           :fix              fix
                                           :parinfer-library (and parinfer-p
                                                                  (source-parinfer-library-ensure
                                                                   :stream stream))
                                           :stream           stream)))
           (when run
             (setf findings (append findings (source-warning-run-finish run))))
           (source-check-report findings stream)
           (if findings 1 0))
      (when (and run (uiop:process-alive-p (source-warning-run-process run)))
        (uiop:terminate-process (source-warning-run-process run) :urgent t)
        (uiop:wait-process (source-warning-run-process run)))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist ':ignore))))
