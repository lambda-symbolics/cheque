(in-package #:cheque)

;;;; -- Formatting Rules --

;;; These rules hold the lambda-symbolics layout: no trailing whitespace or
;;; tabs, one final newline, one blank line between definitions and two
;;; before a major section, conditional clause bodies on the line after their
;;; test, blank lines between local functions, and vertically aligned let
;;; bindings and keyword arguments. Each repair changes only whitespace.

(defparameter *source-clause-operators*
  '("cond" "case" "ecase" "ccase" "typecase" "etypecase" "ctypecase")
  "Operators whose clauses put their body on the line after the test.")

(defparameter *source-binding-operators* '("let" "let*")
  "Operators whose binding values are aligned.")

(defclass source-trailing-whitespace-rule (source-rule)
  ()
  (:default-initargs
   :name "trailing-whitespace"
   :description "Leave no trailing whitespace.")
  (:documentation "Lines must not end in spaces or tabs."))

(defclass source-final-newline-rule (source-rule)
  ()
  (:default-initargs
   :name "final-newline"
   :description "A file ends in exactly one newline, without trailing blank lines.")
  (:documentation "A file must end with one newline and no blank lines after its content."))

(defclass source-tab-rule (source-rule)
  ()
  (:default-initargs
   :name "tab"
   :description "Indent with spaces; parinfer refuses files containing tab characters.")
  (:documentation "Lisp source must not contain tab characters."))

(defclass source-em-dash-rule (source-rule)
  ()
  (:default-initargs
   :name "em-dash"
   :description "Never use em dashes in source comments, documentation, or user-facing text.")
  (:documentation "Text must not contain em dashes."))

(defclass source-section-header-rule (source-rule)
  ()
  (:default-initargs
   :name "section-header"
   :description "A four-semicolon comment is a major section header written ;;;; -- Title --.")
  (:documentation "Four-semicolon comments must be major section headers."))

(defclass source-top-level-spacing-rule (source-rule)
  ()
  (:default-initargs
   :name "top-level-spacing"
   :description "One blank line between definitions, none after a definition's -> declaration, and two before a major section.")
  (:documentation "Blank lines between top-level forms and before major sections."))

(defclass source-clause-layout-rule (source-rule)
  ()
  (:default-initargs
   :name "clause-layout"
   :description "Put a conditional clause body on the line after its test, even for nil.")
  (:documentation "Conditional clause bodies start on the line after their test."))

(defclass source-local-function-spacing-rule (source-rule)
  ()
  (:default-initargs
   :name "local-function-spacing"
   :description "In labels, leave a blank line between local function definitions.")
  (:documentation "One blank line between labels local functions."))

(defclass source-alignment-rule (source-rule)
  ()
  (:default-initargs
   :name "alignment"
   :description "Align let binding values and keyword argument values, class slot options included, that start their own lines.")
  (:documentation "Vertical alignment of repeated name and value columns."))


;;;; -- Text Rules --

(defmethod source-rule-applies-p ((rule source-trailing-whitespace-rule) (file source-file))
  "Inspect Lisp source and documentation."
  (declare (ignore rule))
  (source-file-text-p file))

(defmethod source-rule-findings ((rule source-trailing-whitespace-rule) (file source-file))
  "Report each line ending in whitespace."
  (loop for text across (source-file-lines file)
        for line from 1
        when (and (plusp (length text))
                  (find (char text (1- (length text))) '(#\Space #\Tab #\Return)))
          collect (source-finding-create rule file
                                         :line    line
                                         :message "The line ends in whitespace."
                                         :column  (length (string-right-trim '(#\Space #\Tab #\Return)
                                                                             text)))))

(defmethod source-rule-fix ((rule source-trailing-whitespace-rule) (file source-file))
  "Remove whitespace at the ends of lines, except inside string literals."
  (let ((protected (source-file-protected-lines file)))
    (format nil "~{~A~^~%~}"
            (loop for text across (source-file-lines file)
                  for line from 1
                  collect (if (gethash (1+ line) protected)
                              text
                              (string-right-trim '(#\Space #\Tab #\Return) text))))))

(defmethod source-rule-applies-p ((rule source-final-newline-rule) (file source-file))
  "Inspect Lisp source and documentation."
  (declare (ignore rule))
  (source-file-text-p file))

(defmethod source-rule-findings ((rule source-final-newline-rule) (file source-file))
  "Report a missing final newline or blank lines at the end."
  (let* ((text        (source-file-text file))
         (content-end (or (position-if-not #'source-text--blank-character-p text :from-end t)
                          -1)))
    (cond
      ((zerop (length text))
       nil)
      ((not (char= (char text (1- (length text))) #\Newline))
       (list (source-finding-create rule file
                                    :line    (length (source-file-lines file))
                                    :message "The file does not end in a newline.")))
      ((> (count #\Newline text :start (1+ content-end)) 1)
       (list (source-finding-create rule file
                                    :line    (length (source-file-lines file))
                                    :message "The file ends in blank lines.")))
      (t
       nil))))

(defmethod source-rule-fix ((rule source-final-newline-rule) (file source-file))
  "End the text with exactly one newline."
  (declare (ignore rule))
  (let* ((text        (source-file-text file))
         (content-end (position-if-not #'source-text--blank-character-p text :from-end t)))
    (if content-end
        (concatenate 'string (subseq text 0 (1+ content-end)) (string #\Newline))
        text)))

(defmethod source-rule-findings ((rule source-tab-rule) (file source-file))
  "Report each line holding a tab character."
  (loop for text across (source-file-lines file)
        for line from 1
        for column = (position #\Tab text)
        when column
          collect (source-finding-create rule file
                                         :line    line
                                         :message "The line contains a tab character."
                                         :column  column)))

(defmethod source-rule-fix ((rule source-tab-rule) (file source-file))
  "Expand tabs outside string literals to spaces at eight-column stops."
  (declare (ignore rule))
  (let ((protected (source-file-protected-lines file))
        (strings   (source-file-string-ranges file))
        (offset    0))
    (format nil "~{~A~^~%~}"
            (loop for text across (source-file-lines file)
                  for line from 1
                  collect (prog1 (if (gethash line protected)
                                     text
                                     (source-text--expand-tabs text offset strings))
                            (incf offset (1+ (length text))))))))

(defmethod source-rule-applies-p ((rule source-em-dash-rule) (file source-file))
  "Inspect Lisp source and documentation."
  (declare (ignore rule))
  (source-file-text-p file))

(defmethod source-rule-findings ((rule source-em-dash-rule) (file source-file))
  "Report each line holding an em dash."
  (loop for text across (source-file-lines file)
        for line from 1
        for column = (position (code-char #x2014) text)
        when column
          collect (source-finding-create rule file
                                         :line    line
                                         :message "The line contains an em dash."
                                         :column  column)))

(defmethod source-rule-findings ((rule source-section-header-rule) (file source-file))
  "Report four-semicolon comments that are not ;;;; -- Title -- headers."
  (loop for comment in (source-file-comments file)
        for text = (source-comment-text comment)
        when (and (uiop:string-prefix-p ";;;;" text)
                  (not (source-text--major-header-p text)))
          collect (source-finding-create rule file
                                         :line    (source-comment-line comment)
                                         :message "A four-semicolon comment must read ;;;; -- Title --."
                                         :column  (source-comment-column comment))))


;;;; -- Top-Level Spacing --

(defmethod source-rule-findings ((rule source-top-level-spacing-rule) (file source-file))
  "Report top-level gaps with the wrong number of blank lines."
  (mapcar (lambda (gap)
            (source-finding-create
             rule file
             :line    (getf gap :line)
             :message (format nil "Expected ~D blank line~:P before this ~A, found ~D."
                       (getf gap :expected) (getf gap :what) (getf gap :found))))
          (source-top-level--wrong-gaps file)))

(defmethod source-rule-fix ((rule source-top-level-spacing-rule) (file source-file))
  "Give every top-level gap its expected number of blank lines."
  (declare (ignore rule))
  (source-edits-apply
   (source-file-text file)
   (mapcar (lambda (gap)
             (list (getf gap :start) (getf gap :end)
                   (source-text--break (1+ (getf gap :expected)) (getf gap :column))))
           (source-top-level--wrong-gaps file))))


;;;; -- Clause Layout --

(defmethod source-rule-findings ((rule source-clause-layout-rule) (file source-file))
  "Report clauses that share a line with the clause before them or whose body starts on the line of their test."
  (append
   (loop for (nil . pairs) in (source-clause--crowded file)
         append (mapcar (lambda (pair)
                          (source-finding-create rule file
                                                 :line    (source-node-line (rest pair))
                                                 :message "Start each clause on its own line."
                                                 :column  (source-node-column (rest pair))))
                        pairs))
   (mapcar (lambda (clause)
             (source-finding-create rule file
                                    :line    (source-node-line clause)
                                    :message "The clause body must start on the line after its test."
                                    :column  (source-node-column clause)))
           (source-clause--misplaced file))))

(defmethod source-rule-fix ((rule source-clause-layout-rule) (file source-file))
  "Start crowded clauses on their own lines, then move each misplaced clause body below its test.

A form's crowded clauses move together, before any body, so a later clause on
the same line never drifts right with the body before it."
  (declare (ignore rule))
  (let ((protected (source-file-protected-lines file)))
    (source-edit-groups-apply
     (source-file-text file)
     (append
      (loop for (column . pairs) in (source-clause--crowded file)
            collect (list :first-line (source-node-line (rest (first pairs)))
                          :last-line  (source-node-end-line (rest (first (last pairs))))
                          :edits      (loop for (previous . clause) in pairs
                                            append (cons (list (source-node-end previous)
                                                               (source-node-prefix-start clause)
                                                               (source-text--break 1 column))
                                                         (source-shift-edits
                                                          file
                                                          :first-line (1+ (source-node-line clause))
                                                          :last-line  (source-node-end-line clause)
                                                          :delta      (- column (source-node-column clause))
                                                          :protected  protected)))))
      (loop for clause in (source-clause--misplaced file)
           for children = (source-node-children clause)
           for test = (first children)
           for body = (second children)
           for column = (1+ (source-node-column clause))
           collect (list :first-line (source-node-line clause)
                         :last-line  (source-node-end-line clause)
                         :edits      (cons (list (source-node-end test) (source-node-prefix-start body)
                                                 (source-text--break 1 column))
                                           (source-shift-edits
                                            file
                                            :first-line (1+ (source-node-end-line test))
                                            :last-line  (source-node-end-line clause)
                                            :delta      (- column (source-node-prefix-column body))
                                            :protected  protected))))))))


;;;; -- Local Function Spacing --

(defmethod source-rule-findings ((rule source-local-function-spacing-rule) (file source-file))
  "Report labels local functions not separated by exactly one blank line."
  (mapcar (lambda (gap)
            (source-finding-create rule file
                                   :line    (getf gap :line)
                                   :message "Leave one blank line between local function definitions."))
          (source-local-function--wrong-gaps file)))

(defmethod source-rule-fix ((rule source-local-function-spacing-rule) (file source-file))
  "Separate labels local functions by exactly one blank line."
  (declare (ignore rule))
  (source-edits-apply
   (source-file-text file)
   (mapcar (lambda (gap)
             (list (getf gap :start) (getf gap :end)
                   (source-text--break 2 (getf gap :column))))
           (source-local-function--wrong-gaps file))))


;;;; -- Alignment --

(defmethod source-rule-findings ((rule source-alignment-rule) (file source-file))
  "Report values that do not start in their group's column."
  (loop for group in (source-alignment--groups file)
        append (loop for (name value) in (getf group :pairs)
                     unless (= (source-node-prefix-column value) (getf group :column))
                       collect (source-finding-create
                                rule file
                                :line    (source-node-line name)
                                :message (format nil "Align this value with the others at column ~D."
                                          (getf group :column))
                                :column  (source-node-prefix-column value)))))

(defmethod source-rule-fix ((rule source-alignment-rule) (file source-file))
  "Move misaligned values to their group's column, moving their following lines along."
  (declare (ignore rule))
  (let ((protected (source-file-protected-lines file)))
    (source-edit-groups-apply
     (source-file-text file)
     (loop for group in (source-alignment--groups file)
           for column = (getf group :column)
           for pairs = (remove-if (lambda (pair)
                                    (= (source-node-prefix-column (second pair)) column))
                                  (getf group :pairs))
           when pairs
             collect (list :first-line (source-node-line (first (first pairs)))
                           :last-line  (reduce #'max pairs
                                               :key (lambda (pair)
                                                      (source-node-end-line (second pair))))
                           :edits
                           (loop for (name value) in pairs
                                 for delta = (- column (source-node-prefix-column value))
                                 append (cons (list (source-node-end name)
                                                    (source-node-prefix-start value)
                                                    (make-string (- column (source-node-column name)
                                                                    (length (source-node-text name)))
                                                                 :initial-element #\Space))
                                              (source-shift-edits
                                               file
                                               :first-line (1+ (source-node-line value))
                                               :last-line  (source-node-end-line value)
                                               :delta      delta
                                               :protected  protected))))))))


;;;; -- Public Functions --

(-> source-file-text-p (source-file) boolean)
(defun source-file-text-p (file)
  "Return true when FILE is Lisp source or documentation that text rules inspect."
  (and (member (pathname-type (source-file-path file)) (project-text-file-types (project-current)) :test #'equal)
       t))

(-> source-file-protected-lines (source-file) hash-table)
(defun source-file-protected-lines (file)
  "Return the lines whose leading whitespace belongs to a string or comment, Lisp source only."
  (if (source-file-lisp-p file)
      (source-file-string-lines file)
      (make-hash-table)))

(-> source-file-string-ranges (source-file) list)
(defun source-file-string-ranges (file)
  "Return the (START END) offsets of FILE's string literals."
  (let ((ranges nil))
    (dolist (node (source-file-nodes file))
      (source-node-walk node
                        (lambda (node)
                          (when (source-node-string-p node)
                            (push (list (source-node-start node) (source-node-end node))
                                  ranges)))))
    (nreverse ranges)))


;;;; -- Private Functions --

(-> source-text--break ((integer 0) (integer 0)) string)
(defun source-text--break (newlines column)
  "Return NEWLINES newlines followed by indentation to COLUMN."
  (concatenate 'string
               (make-string newlines :initial-element #\Newline)
               (make-string column :initial-element #\Space)))

(-> source-text--definition-p (source-node) boolean)
(defun source-text--definition-p (node)
  "Return true when NODE defines something: a def or define- form, or a -> declaration."
  (let ((operator (source-node-operator node)))
    (and operator
         (or (uiop:string-prefix-p "def" operator)
             (equal operator "->"))
         t)))

(-> source-text--blank-character-p (character) boolean)
(defun source-text--blank-character-p (character)
  "Return true when CHARACTER is whitespace."
  (and (find character '(#\Space #\Tab #\Newline #\Return #\Page)) t))

(-> source-text--major-header-p (string) boolean)
(defun source-text--major-header-p (text)
  "Return true when comment TEXT is a ;;;; -- Title -- major section header."
  (and (uiop:string-prefix-p ";;;; -- " text)
       (uiop:string-suffix-p text " --")
       (> (length text) (length ";;;; --  --"))
       t))

(-> source-text--expand-tabs (string (integer 0) list) string)
(defun source-text--expand-tabs (text offset strings)
  "Return line TEXT, starting at OFFSET, with tabs outside STRINGS expanded to eight-column stops."
  (with-output-to-string (stream)
    (let ((column 0))
      (loop for character across text
            for position from offset
            do (if (and (char= character #\Tab)
                        (notany (lambda (range)
                                  (< (first range) position (second range)))
                                strings))
                   (let ((width (- 8 (mod column 8))))
                     (write-string (make-string width :initial-element #\Space) stream)
                     (incf column width))
                   (progn
                     (write-char character stream)
                     (incf column)))))))

(-> source-top-level--items (source-file) list)
(defun source-top-level--items (file)
  "Return FILE's top-level forms and major section headers in order.

Each item is a plist of :KIND (:FORM, :ARROW, or :HEADER), :DEFINITION-P,
:LINE, :END-LINE, :START, :END, and :COLUMN. Other comments are not items: a
gap holding one is not judged."
  (let ((items
          (append
           (mapcar (lambda (node)
                     (list :kind         (if (equal (source-node-operator node) "->") ':arrow ':form)
                           :definition-p (source-text--definition-p node)
                           :line         (source-node-prefix-line node)
                           :end-line     (source-node-end-line node)
                           :start        (source-node-prefix-start node)
                           :end          (source-node-end node)
                           :column       (source-node-prefix-column node)))
                   (source-file-nodes file))
           (loop for comment in (source-file-comments file)
                 when (and (zerop (source-comment-column comment))
                           (source-text--major-header-p (source-comment-text comment)))
                   collect (let ((start (source-file-line-offset file (source-comment-line comment))))
                             (list :kind     ':header
                                   :line     (source-comment-line comment)
                                   :end-line (source-comment-line comment)
                                   :start    start
                                   :end      (+ start (length (source-comment-text comment)))
                                   :column   0))))))
    (sort items #'< :key (lambda (item) (getf item :line)))))

(-> source-top-level--wrong-gaps (source-file) list)
(defun source-top-level--wrong-gaps (file)
  "Return the top-level gaps of FILE holding the wrong number of blank lines.

A definition follows its -> declaration directly, a definition and its
neighboring forms are one blank line apart, and a major section header has two
blank lines before it, one when it is the file's first header. Gaps between
other forms, such as consecutive requires, are not judged. A gap holding a comment, or two
items sharing a line, is not judged. Each gap is a plist of :LINE, :START,
:END, :EXPECTED, :FOUND, :COLUMN, and :WHAT."
  (let* ((items         (source-top-level--items file))
         (comment-lines (make-hash-table))
         ;; A header opening the file is never the later item of a gap.
         (headers-seen  (if (eq (getf (first items) :kind) ':header) 1 0))
         (gaps          nil))
    (dolist (comment (source-file-comments file))
      (loop for line from (source-comment-line comment) to (source-comment-end-line comment)
            do (setf (gethash line comment-lines) t)))
    (loop for (previous item) on items
          while item
          do (let ((expected (cond
                               ((eq (getf item :kind) ':header)
                                (if (zerop headers-seen) 1 2))
                               ((eq (getf previous :kind) ':header)
                                nil)
                               ((eq (getf previous :kind) ':arrow)
                                0)
                               ((or (getf previous :definition-p) (getf item :definition-p))
                                1)
                               (t
                                nil)))
                   (found    (- (getf item :line) (getf previous :end-line) 1)))
               (when (eq (getf item :kind) ':header)
                 (incf headers-seen))
               (when (and expected
                          (> (getf item :line) (getf previous :end-line))
                          (/= found expected)
                          (loop for line from (1+ (getf previous :end-line))
                                  below (getf item :line)
                                never (and (gethash line comment-lines)
                                           (not (and (eq (getf item :kind) ':header)
                                                     (= line (getf item :line)))))))
                 (push (list :line     (getf item :line)
                             :start    (getf previous :end)
                             :end      (getf item :start)
                             :expected expected
                             :found    found
                             :column   (getf item :column)
                             :what     (if (eq (getf item :kind) ':header)
                                           "major section"
                                           "form"))
                       gaps))))
    (nreverse gaps)))

(-> source-clause--clauses (source-node) list)
(defun source-clause--clauses (node)
  "Return the clauses of the conditional NODE, or NIL when NODE is not a conditional."
  (let ((operator (source-node-operator node)))
    (cond
      ((equal operator "cond")
       (rest (source-node-children node)))
      ((member operator *source-clause-operators* :test #'equal)
       (nthcdr 2 (source-node-children node))))))

(-> source-clause--crowded (source-file) list)
(defun source-clause--crowded (file)
  "Return (COLUMN . PAIRS) for each conditional of FILE with clauses that share a line.

PAIRS holds (PREVIOUS . CLAUSE) for each clause that starts, after only blanks,
on the line where the clause before it ends. COLUMN is where the form's first
clause that begins its own line starts, or its first clause when none does."
  (let ((forms nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (let* ((clauses (source-clause--clauses node))
                (pairs   (loop for (previous clause) on clauses
                               while clause
                               when (and (source-node-list-p clause)
                                         (null (source-node-prefixes clause))
                                         (= (source-node-end-line previous) (source-node-line clause))
                                         (source-text--blank-between-p
                                          file (source-node-end previous) (source-node-prefix-start clause)))
                                 collect (cons previous clause))))
           (when pairs
             (push (cons (source-node-column
                          (or (find-if (lambda (clause)
                                         (= (source-node-column clause)
                                            (or (position #\Space
                                                          (source-file-line file (source-node-line clause))
                                                          :test-not #'char=)
                                                0)))
                                       clauses)
                              (first clauses)))
                         pairs)
                   forms))))))
    (nreverse forms)))

(-> source-clause--misplaced (source-file) list)
(defun source-clause--misplaced (file)
  "Return the clauses of FILE whose body starts on the line where their test ends."
  (let ((clauses nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (let ((operator (source-node-operator node)))
           (when (member operator *source-clause-operators* :test #'equal)
             (dolist (clause (if (equal operator "cond")
                                 (rest (source-node-children node))
                                 (nthcdr 2 (source-node-children node))))
               (when (and (source-node-list-p clause)
                          (null (source-node-prefixes clause))
                          (rest (source-node-children clause))
                          (= (source-node-end-line (first (source-node-children clause)))
                             (source-node-prefix-line (second (source-node-children clause))))
                          (source-text--blank-between-p
                           file (source-node-end (first (source-node-children clause)))
                           (source-node-prefix-start (second (source-node-children clause)))))
                 (push clause clauses))))))))
    (nreverse clauses)))

(-> source-local-function--wrong-gaps (source-file) list)
(defun source-local-function--wrong-gaps (file)
  "Return the gaps between labels local functions of FILE that are not one blank line."
  (let ((gaps nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (when (equal (source-node-operator node) "labels")
           (let ((definitions (second (source-node-children node))))
             (when (source-node-list-p definitions)
               (loop for (previous definition) on (source-node-children definitions)
                     while definition
                     do (let ((blank (- (source-node-prefix-line definition)
                                        (source-node-end-line previous) 1)))
                          (when (and (>= blank 0)
                                     (/= blank 1)
                                     (source-text--blank-between-p
                                      file (source-node-end previous)
                                      (source-node-prefix-start definition)))
                            (push (list :line   (source-node-prefix-line definition)
                                        :start  (source-node-end previous)
                                        :end    (source-node-prefix-start definition)
                                        :column (source-node-prefix-column definition))
                                  gaps))))))))))
    (nreverse gaps)))

(-> source-text--blank-between-p (source-file (integer 0) (integer 0)) boolean)
(defun source-text--blank-between-p (file start end)
  "Return true when only whitespace lies between offsets START and END of FILE."
  (and (every #'source-text--blank-character-p
              (subseq (source-file-text file) start end))
       t))

(-> source-alignment--groups (source-file) list)
(defun source-alignment--groups (file)
  "Return FILE's alignment groups, each a plist of :PAIRS, (NAME VALUE) nodes, and :COLUMN."
  (let ((groups nil))
    (dolist (top (source-file-nodes file))
      (source-node-walk
       top
       (lambda (node)
         (when (and (source-node-list-p node) (null (source-node-prefixes node)))
           (let ((operator (source-node-operator node)))
             (if (member operator *source-binding-operators* :test #'equal)
                 (let ((bindings (second (source-node-children node))))
                   (when (and (source-node-list-p bindings) (null (source-node-prefixes bindings)))
                     (let ((group (source-alignment--group
                                   file
                                   (loop for binding in (source-node-children bindings)
                                         when (and (source-node-list-p binding)
                                                   (null (source-node-prefixes binding))
                                                   (= 2 (length (source-node-children binding)))
                                                   (source-node-atom-p
                                                    (first (source-node-children binding))))
                                           collect (source-node-children binding))
                                   (source-node-children bindings))))
                       (when group
                         (push group groups)))))
                 (let ((group (source-alignment--group
                               file
                               (source-alignment--keyword-pairs
                                (source-node-children node))
                               (source-node-children node))))
                   (when group
                     (push group groups)))))))))
    (nreverse groups)))

(-> source-alignment--keyword-pairs (list) list)
(defun source-alignment--keyword-pairs (children)
  "Return the (KEYWORD VALUE) pairs among CHILDREN, each keyword taking the element after it."
  (let ((pairs nil))
    (loop while children
          do (let ((name  (first children))
                   (value (second children)))
               (if (and (source-node-keyword-p name) value)
                   (progn
                     (push (list name value) pairs)
                     (setf children (nthcdr 2 children)))
                   (setf children (rest children)))))
    (nreverse pairs)))

(-> source-alignment--group (source-file list list) (option list))
(defun source-alignment--group (file pairs siblings)
  "Return the alignment group of PAIRS among SIBLINGS, or NIL when fewer than two pairs align.

A pair takes part when its value starts on its name's line, it is the only
pair on that line, and no sibling before it on the line is a keyword or a
binding. Its value then belongs in the column after the longest name."
  (declare (ignore file))
  (let ((eligible
          (remove-if-not
           (lambda (pair)
             (destructuring-bind (name value) pair
               (and (= (source-node-line name) (source-node-prefix-line value))
                    (= (source-node-end-line name) (source-node-line name))
                    (let ((before (loop for sibling in siblings
                                        until (or (eq sibling name)
                                                  (and (source-node-list-p sibling)
                                                       (member name (source-node-children sibling))))
                                        when (= (source-node-end-line sibling) (source-node-line name))
                                          collect sibling)))
                      (notany (lambda (sibling)
                                (or (source-node-keyword-p sibling)
                                    (some (lambda (other) (eq sibling (second other))) pairs)))
                              before))
                    (= 1 (count (source-node-line name) pairs
                                :key (lambda (pair) (source-node-line (first pair))))))))
           pairs)))
    (when (and (rest eligible)
               (= (length (remove-duplicates (mapcar (lambda (pair) (source-node-line (first pair)))
                                                     eligible)))
                  (length eligible)))
      (list :pairs  eligible
            :column (1+ (reduce #'max eligible
                                :key (lambda (pair)
                                       (+ (source-node-column (first pair))
                                          (length (source-node-text (first pair)))))))))))
