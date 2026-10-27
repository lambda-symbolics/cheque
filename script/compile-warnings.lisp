;;;; -- Clean Compile Warnings --

;;; Compile and load every system a project's definitions register for this
;;; host from scratch into a private FASL directory and record each warning the
;;; compiler or loader would print. Warnings SBCL already muffles by policy,
;;; such as uninteresting redefinitions, are not recorded, nor are those of
;;; dependencies or ASDF's per-file summaries of them. The parent check reports what this records.
;;;
;;; Usage: compile-warnings.lisp SOURCE-ROOT FASL-DIRECTORY RESULT-FILE SETUP DEFINITION...
;;;
;;; SETUP is the repository-relative file that makes dependencies loadable,
;;; such as .qlot/setup.lisp, or an empty string. Each DEFINITION is a
;;; repository-relative .asd file whose systems are compiled.

(require :asdf)

(defun compile-warnings--systems (definitions)
  "Return the names of the systems the loaded DEFINITIONS register, in name order."
  (sort (remove-if-not (lambda (name)
                         (let ((system (asdf:find-system name nil)))
                           (and system
                                (member (asdf:system-source-file system) definitions
                                        :test #'uiop:pathname-equal))))
                       (asdf:registered-systems))
        #'string<))

(defun compile-warnings--owned-paths (source-root systems)
  "Map supported systems' exact source and translated output paths to source names."
  (let ((paths (make-hash-table :test #+win32 'equalp #-win32 'equal)))
    (labels ((record (path relative)
               ;; SBCL 2.6.6 EQUALP tables can compare distinct pathname keys by EQ.
               ;; Namestrings also fold Windows case before the output files exist.
               (setf (gethash (uiop:native-namestring path) paths) relative)
               (let ((existing (probe-file path)))
                 (when existing
                   (setf (gethash (uiop:native-namestring existing) paths) relative))))

             (visit (component)
               (when (typep component 'asdf:cl-source-file)
                 (let* ((source   (asdf:component-pathname component))
                        (relative (uiop:unix-namestring
                                   (uiop:enough-pathname source source-root))))
                   (record source relative)
                   (dolist (output (asdf:output-files 'asdf:compile-op component))
                     (record output relative))))
               (when (typep component 'asdf:parent-component)
                 (mapc #'visit (asdf:component-children component)))))
      (dolist (name systems)
        (let ((system (asdf:find-system name nil)))
          (when system
            (visit system)))))
    paths))

(defun compile-warnings--source-path (owned-paths)
  "Return the owned source name for the current compilation or load, or NIL."
  ;; A dependency compilation nested in a project load belongs to the compiled
  ;; file, not to the enclosing load. Output paths come from ASDF, not suffixes.
  (let ((path (or *compile-file-truename* *load-truename*)))
    (and path (gethash (uiop:native-namestring path) owned-paths))))

(defun compile-warnings--line (source-root path)
  "Return the source line of the form the compiler is processing in PATH, or NIL."
  (let ((position (ignore-errors
                   (let ((context (sb-c::find-error-context nil)))
                     (and context (sb-c::compiler-error-context-file-position context))))))
    (and position
         path
         (with-open-file (stream (merge-pathnames path source-root)
                                 :external-format ':utf-8)
           (let ((text (make-string position)))
             (read-sequence text stream)
             (1+ (count #\Newline text)))))))

(defun compile-warnings--interesting-p (condition)
  "Return true when CONDITION would be printed and is not ASDF's summary of others.

Compiling a file proclaims its -> declarations while its classes are still
placeholders, so loading the file proclaims them again and SBCL reports that
each declaration over a class from that file differs from itself. Those
reports arrive while loading, not compiling; a genuine disagreement between
two declarations still warns when the second one is compiled."
  (not (or (typep condition sb-ext:*muffled-warnings*)
           (and (typep condition 'sb-kernel::ftype-proclamation-mismatch-warning)
                (null *compile-file-truename*))
           (typep condition 'uiop:compile-warned-warning)
           (typep condition 'uiop:compile-failed-warning))))

(let* ((arguments      (uiop:command-line-arguments))
       (source-root    (uiop:ensure-directory-pathname (first arguments)))
       (fasl-directory (uiop:ensure-directory-pathname (second arguments)))
       (result-file    (pathname (third arguments)))
       (setup          (fourth arguments))
       (definitions    (mapcar (lambda (relative) (truename (merge-pathnames relative source-root)))
                               (nthcdr 4 arguments)))
       (warnings       nil))
  (when (plusp (length setup))
    (load (merge-pathnames setup source-root)))
  ;; Select registered ASDF before loading dependencies so a later upgrade
  ;; cannot reset their load state and redefine installed structures.
  (asdf:load-system "asdf")
  (asdf:initialize-output-translations
   `(:output-translations (,(uiop:native-namestring source-root)
                           ,(uiop:native-namestring fasl-directory))
                          :inherit-configuration))
  (mapc #'asdf:load-asd definitions)
  (let* ((systems     (compile-warnings--systems definitions))
         (owned-paths (compile-warnings--owned-paths source-root systems)))
    (handler-bind ((warning
                     (lambda (condition)
                       (when (compile-warnings--interesting-p condition)
                         (let ((path (compile-warnings--source-path owned-paths)))
                           ;; A warning without a file of ours belongs to a
                           ;; dependency, unless no file is being processed at
                           ;; all, as for undefined functions reported at the end.
                           (unless (and (null path) (or *compile-file-truename* *load-truename*))
                             (push (list :path    path
                                         :line    (compile-warnings--line source-root path)
                                         :type    (string-downcase (type-of condition))
                                         :message (substitute #\Space #\Newline
                                                              (princ-to-string condition)))
                                   warnings)))))))
      (let ((asdf:*compile-file-warnings-behaviour* ':warn)
            (asdf:*compile-file-failure-behaviour*  ':warn))
        (dolist (system systems)
          (asdf:load-system system)))))
  (with-open-file (stream result-file :direction ':output :if-exists ':supersede
                                      :external-format ':utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (prin1 (reverse warnings) stream))))
  (uiop:quit 0))
