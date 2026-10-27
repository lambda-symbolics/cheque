(in-package #:cheque)

;;;; -- Private Parinfer Library --

;;; The parinfer rule runs parinfer-rust's paren mode exactly as editors do.
;;; This builds the C library of the fork parinfer-rust-mode downloads, at the
;;; commit native/parinfer/commit pins, without its Emacs module, and installs
;;; it under the per-user data root. Its own Cargo.lock pins the dependencies.
;;; The build needs Git and Cargo, and runs only when the installed library
;;; is missing or was built from another commit.

(defparameter *source-parinfer-remote* "https://github.com/justinbarclay/parinfer-rust-emacs.git"
  "The repository of the parinfer-rust fork parinfer-rust-mode downloads.")


;;;; -- Public Functions --

(-> source-parinfer-library () pathname)
(defun source-parinfer-library ()
  "Return where the private parinfer library is installed for this user."
  (merge-pathnames (source-parinfer-library-name) (source-parinfer-build--install-root)))

(-> source-parinfer-library-ensure (&key (:stream stream)) pathname)
(defun source-parinfer-library-ensure (&key (stream *standard-output*))
  "Return the private parinfer library, building it first when it is missing or stale.

Build progress and the commands' output go to STREAM."
  (let* ((commit   (source-parinfer-build--commit))
         (library  (source-parinfer-library))
         (manifest (merge-pathnames "manifest.sexp" (source-parinfer-build--install-root))))
    (unless (source-parinfer-build--current-p library manifest commit)
      (source-parinfer-build--install :commit   commit
                                      :library  library
                                      :manifest manifest
                                      :stream   stream))
    library))


;;;; -- Private Functions --

(-> source-parinfer-build--commit () string)
(defun source-parinfer-build--commit ()
  "Return the parinfer-rust commit native/parinfer/commit pins."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (uiop:read-file-string
                (asdf:system-relative-pathname "cheque" "native/parinfer/commit"))))

(-> source-parinfer-build--install-root () pathname)
(defun source-parinfer-build--install-root ()
  "Return the per-user directory holding the installed library and its manifest."
  (uiop:xdg-data-home "cheque/parinfer/"))

(-> source-parinfer-build--current-p (pathname pathname string) boolean)
(defun source-parinfer-build--current-p (library manifest commit)
  "Return true when the installed LIBRARY's MANIFEST records COMMIT."
  (and (probe-file library)
       (probe-file manifest)
       (handler-case
           (with-open-file (stream manifest :direction ':input :external-format ':utf-8)
             (let ((*read-eval* nil))
               (equal (read stream nil nil)
                      (list :parinfer-library :version 1 :commit commit))))
         (error ()
           nil))
       t))

(-> source-parinfer-build--install
    (&key (:commit string) (:library pathname) (:manifest pathname) (:stream stream))
    null)
(defun source-parinfer-build--install (&key commit library manifest stream)
  "Fetch parinfer-rust at COMMIT, build it, and atomically install LIBRARY and MANIFEST."
  (let ((checkout (uiop:xdg-cache-home "cheque/parinfer/" (concatenate 'string commit "/"))))
    (unless (probe-file (merge-pathnames ".git/" checkout))
      (ensure-directories-exist checkout)
      (source-parinfer-build--run (list "git" "init" "--quiet" (uiop:native-namestring checkout))
                                  :stream stream)
      (source-parinfer-build--run (list "git" "-C" (uiop:native-namestring checkout)
                                        "remote" "add" "origin" *source-parinfer-remote*)
                                  :stream stream))
    (format stream "~&Fetching parinfer-rust at ~A.~%" commit)
    (finish-output stream)
    (source-parinfer-build--run (list "git" "-C" (uiop:native-namestring checkout)
                                      "fetch" "--depth" "1" "origin" commit)
                                :stream stream)
    (source-parinfer-build--run (list "git" "-C" (uiop:native-namestring checkout)
                                      "checkout" "--detach" "--force" "FETCH_HEAD")
                                :stream stream)
    (let ((actual (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (uiop:run-program (list "git" "-C" (uiop:native-namestring checkout)
                                                       "rev-parse" "HEAD")
                                                 :output ':string))))
      (unless (string= actual commit)
        (error 'source-parinfer-error
               :library library
               :message (format nil "Fetched parinfer-rust commit ~A instead of ~A." actual commit))))
    ;; The release's lock file predates its own version bump, so Cargo
    ;; rewrites that one entry; every dependency stays as locked. Musl
    ;; defaults to a static C runtime, which disables Cargo's cdylib output.
    (source-parinfer-build--run
     (list "cargo" "build" "--release" "--lib" "--no-default-features"
           "--config"
           "target.'cfg(target_env = \"musl\")'.rustflags=[\"-C\",\"target-feature=-crt-static\"]")
     :directory checkout
     :stream    stream)
    (let ((built (merge-pathnames (concatenate 'string "target/release/" (source-parinfer-library-name))
                                  checkout)))
      (unless (probe-file built)
        (error 'source-parinfer-error
               :library library
               :message (format nil "parinfer-rust did not produce ~A." (uiop:native-namestring built))))
      (ensure-directories-exist library)
      (source-parinfer-build--publish library
                                      (lambda (temporary) (uiop:copy-file built temporary)))
      (source-parinfer-build--publish manifest
                                      (lambda (temporary)
                                        (with-open-file (output temporary :direction       ':output
                                                                          :if-exists       ':supersede
                                                                          :external-format ':utf-8)
                                          (prin1 (list :parinfer-library :version 1 :commit commit)
                                                 output)
                                          (terpri output))))
      (format stream "~&Installed the private parinfer library at ~A.~%"
              (uiop:native-namestring library))))
  nil)

(-> source-parinfer-build--run (list &key (:directory (option pathname)) (:stream stream)) null)
(defun source-parinfer-build--run (command &key directory stream)
  "Run one build COMMAND in DIRECTORY, copying its output to STREAM.

Signal SOURCE-PARINFER-ERROR when the command fails."
  (multiple-value-bind (output error-output status)
      (uiop:run-program command
                        :directory           directory
                        :output              ':string
                        :error-output        ':output
                        :ignore-error-status t)
    (declare (ignore error-output))
    (write-string output stream)
    (unless (eql status 0)
      (error 'source-parinfer-error
             :library (source-parinfer-library)
             :message (format nil "Building the parinfer library failed: ~{~A~^ ~} exited with ~A."
                              command status))))
  nil)

(-> source-parinfer-build--publish (pathname function) null)
(defun source-parinfer-build--publish (target write)
  "Atomically replace TARGET with the file WRITE creates at the temporary pathname it is given."
  (let ((temporary (make-pathname :name     (format nil ".~A.~36R" (pathname-name target)
                                                    (random (expt 36 8) (make-random-state t)))
                                  :type     (pathname-type target)
                                  :defaults target)))
    (unwind-protect
         (progn
           (funcall write temporary)
           (uiop:rename-file-overwriting-target temporary target))
      (when (probe-file temporary)
        (delete-file temporary))))
  nil)
