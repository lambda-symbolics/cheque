(in-package #:cheque)

;;;; -- Source Syntax --

;;; Source checks work on the text as written: where each form starts and
;;; ends, which lines it spans, and how it is spelled. The reader below keeps
;;; that layout instead of producing Lisp objects, so it needs no packages,
;;; never evaluates reader conditionals, and reads files for any host. A node
;;; is a list, an atom, or a string literal; reader macros such as ', #' and
;;; #+feature are kept as its prefixes. Comments are collected separately.

(defclass source-node ()
  ((kind
    :initarg       :kind
    :reader        source-node-kind
    :type          (member :list :atom :string)
    :documentation "Whether the node is a parenthesized list, an atom, or a string literal.")
   (text
    :initarg       :text
    :reader        source-node-text
    :type          string
    :documentation "The node's source text, without its prefixes.")
   (start
    :initarg       :start
    :reader        source-node-start
    :type          (integer 0)
    :documentation "The character offset where the node's own text starts.")
   (end
    :initarg       :end
    :reader        source-node-end
    :type          (integer 0)
    :documentation "The character offset just after the node.")
   (line
    :initarg       :line
    :reader        source-node-line
    :type          (integer 1)
    :documentation "The line where the node's own text starts.")
   (column
    :initarg       :column
    :reader        source-node-column
    :type          (integer 0)
    :documentation "The column where the node's own text starts.")
   (end-line
    :initarg       :end-line
    :reader        source-node-end-line
    :type          (integer 1)
    :documentation "The line holding the node's last character.")
   (prefix-start
    :initarg       :prefix-start
    :reader        source-node-prefix-start
    :type          (integer 0)
    :documentation "The character offset where the node's first prefix starts.")
   (prefix-line
    :initarg       :prefix-line
    :reader        source-node-prefix-line
    :type          (integer 1)
    :documentation "The line where the node's first prefix starts.")
   (prefix-column
    :initarg       :prefix-column
    :reader        source-node-prefix-column
    :type          (integer 0)
    :documentation "The column where the node's first prefix starts.")
   (prefixes
    :initarg       :prefixes
    :initform      nil
    :reader        source-node-prefixes
    :type          list
    :documentation "Reader prefixes such as \"'\", \"#'\", or (\"#+\" FEATURE-NODE), outermost first.")
   (children
    :initarg       :children
    :initform      nil
    :reader        source-node-children
    :type          list
    :documentation "A list node's elements in order."))
  (:documentation "One form of source text with its position and spelling."))

(defclass source-comment ()
  ((line
    :initarg       :line
    :reader        source-comment-line
    :type          (integer 1)
    :documentation "The line where the comment starts.")
   (column
    :initarg       :column
    :reader        source-comment-column
    :type          (integer 0)
    :documentation "The column where the comment starts.")
   (end-line
    :initarg       :end-line
    :reader        source-comment-end-line
    :type          (integer 1)
    :documentation "The line where the comment ends.")
   (text
    :initarg       :text
    :reader        source-comment-text
    :type          string
    :documentation "The comment's text, including its semicolons or #| |# delimiters."))
  (:documentation "One line comment or block comment of source text."))

(defclass source-reader ()
  ((text
    :initarg       :text
    :reader        source-reader-text
    :type          string
    :documentation "The text being read.")
   (position
    :initform      0
    :accessor      source-reader-position
    :type          (integer 0)
    :documentation "The offset of the next character.")
   (line
    :initform      1
    :accessor      source-reader-line
    :type          (integer 1)
    :documentation "The line of the next character.")
   (column
    :initform      0
    :accessor      source-reader-column
    :type          (integer 0)
    :documentation "The column of the next character.")
   (comments
    :initform      nil
    :accessor      source-reader-comments
    :type          list
    :documentation "The comments read so far, newest first."))
  (:documentation "The state of reading one source text."))


;;;; -- Reading Source --

(-> source-parse (string) (values list list))
(defun source-parse (text)
  "Return TEXT's top-level nodes and its comments, both in order.

Unbalanced closing delimiters are skipped and an unterminated list or string
ends at the end of the text, so any text can be inspected. A first line
starting with #! is a script's interpreter line and reads as a comment."
  (let ((reader (make-instance 'source-reader :text text))
        (nodes  nil))
    (when (uiop:string-prefix-p "#!" text)
      ;; A script's interpreter line, which SBCL's --script skips.
      (source-reader--read-line-comment reader))
    (loop
      (source-reader--skip-blank reader)
      (unless (source-reader--peek reader)
        (return))
      (let ((node (source-reader--read-node reader)))
        (if node
            (push node nodes)
            (source-reader--advance reader))))
    (values (nreverse nodes) (reverse (source-reader-comments reader)))))

(-> source-node-list-p (t) boolean)
(defun source-node-list-p (node)
  "Return true when NODE is a parenthesized list node."
  (and (typep node 'source-node) (eq (source-node-kind node) ':list) t))

(-> source-node-atom-p (t) boolean)
(defun source-node-atom-p (node)
  "Return true when NODE is an atom node."
  (and (typep node 'source-node) (eq (source-node-kind node) ':atom) t))

(-> source-node-string-p (t) boolean)
(defun source-node-string-p (node)
  "Return true when NODE is a string literal node."
  (and (typep node 'source-node) (eq (source-node-kind node) ':string) t))

(-> source-node-name (t) (option string))
(defun source-node-name (node)
  "Return NODE's lowercase symbol name without a package prefix, or NIL when NODE is no plain atom."
  (and (source-node-atom-p node)
       (null (source-node-prefixes node))
       (let* ((text  (string-downcase (source-node-text node)))
              (colon (position #\: text :from-end t)))
         (if (and colon (plusp colon))
             (subseq text (1+ colon))
             text))))

(-> source-node-operator (t) (option string))
(defun source-node-operator (node)
  "Return the lowercase name of list NODE's first element when it is a plain symbol, or NIL."
  (and (source-node-list-p node)
       (null (source-node-prefixes node))
       (let ((first-child (first (source-node-children node))))
         (and first-child
              (not (source-node-keyword-p first-child))
              (source-node-name first-child)))))

(-> source-node-keyword-p (t) boolean)
(defun source-node-keyword-p (node)
  "Return true when NODE is an unprefixed keyword atom such as :type."
  (and (source-node-atom-p node)
       (null (source-node-prefixes node))
       (> (length (source-node-text node)) 1)
       (char= (char (source-node-text node) 0) #\:)
       t))

(-> source-node-walk (t function) null)
(defun source-node-walk (node function)
  "Call FUNCTION on NODE and every node inside it, outer nodes first."
  (funcall function node)
  (when (source-node-list-p node)
    (dolist (child (source-node-children node))
      (source-node-walk child function)))
  nil)


;;;; -- Private Functions --

(-> source-reader--peek (source-reader &optional (integer 0)) (option character))
(defun source-reader--peek (reader &optional (offset 0))
  "Return the character OFFSET characters ahead of READER's position, or NIL at the end."
  (let ((index (+ (source-reader-position reader) offset))
        (text  (source-reader-text reader)))
    (and (< index (length text)) (char text index))))

(-> source-reader--advance (source-reader) character)
(defun source-reader--advance (reader)
  "Consume and return READER's next character, tracking its line and column."
  (let ((character (char (source-reader-text reader) (source-reader-position reader))))
    (incf (source-reader-position reader))
    (if (char= character #\Newline)
        (setf (source-reader-line reader) (1+ (source-reader-line reader))
              (source-reader-column reader) 0)
        (incf (source-reader-column reader)))
    character))

(-> source-reader--blank-p (character) boolean)
(defun source-reader--blank-p (character)
  "Return true when CHARACTER separates tokens as whitespace."
  (and (find character '(#\Space #\Tab #\Newline #\Return #\Page)) t))

(-> source-reader--skip-blank (source-reader) null)
(defun source-reader--skip-blank (reader)
  "Skip whitespace and comments at READER's position, collecting the comments."
  (loop
    (let ((character (source-reader--peek reader)))
      (cond
        ((null character)
         (return))
        ((source-reader--blank-p character)
         (source-reader--advance reader))
        ((char= character #\;)
         (source-reader--read-line-comment reader))
        ((and (char= character #\#) (eql (source-reader--peek reader 1) #\|))
         (source-reader--read-block-comment reader))
        (t
         (return)))))
  nil)

(-> source-reader--read-line-comment (source-reader) null)
(defun source-reader--read-line-comment (reader)
  "Read one ; comment up to the end of its line."
  (let ((start  (source-reader-position reader))
        (line   (source-reader-line reader))
        (column (source-reader-column reader)))
    (loop while (and (source-reader--peek reader)
                     (char/= (source-reader--peek reader) #\Newline))
          do (source-reader--advance reader))
    (push (make-instance 'source-comment
                         :line     line
                         :column   column
                         :end-line line
                         :text     (subseq (source-reader-text reader) start
                                           (source-reader-position reader)))
          (source-reader-comments reader)))
  nil)

(-> source-reader--read-block-comment (source-reader) null)
(defun source-reader--read-block-comment (reader)
  "Read one nested #| |# comment."
  (let ((start  (source-reader-position reader))
        (line   (source-reader-line reader))
        (column (source-reader-column reader))
        (depth  0))
    (loop
      (let ((character (source-reader--peek reader)))
        (cond
          ((null character)
           (return))
          ((and (char= character #\#) (eql (source-reader--peek reader 1) #\|))
           (source-reader--advance reader)
           (source-reader--advance reader)
           (incf depth))
          ((and (char= character #\|) (eql (source-reader--peek reader 1) #\#))
           (source-reader--advance reader)
           (source-reader--advance reader)
           (decf depth)
           (when (zerop depth)
             (return)))
          (t
           (source-reader--advance reader)))))
    (push (make-instance 'source-comment
                         :line     line
                         :column   column
                         :end-line (source-reader-line reader)
                         :text     (subseq (source-reader-text reader) start
                                           (source-reader-position reader)))
          (source-reader-comments reader)))
  nil)

(-> source-reader--read-prefixes (source-reader) list)
(defun source-reader--read-prefixes (reader)
  "Read the reader prefixes before a node and return them outermost first."
  (let ((prefixes nil))
    (loop
      (let ((character (source-reader--peek reader))
            (next      (source-reader--peek reader 1)))
        (cond
          ((null character)
           (return))
          ((find character "'`")
           (push (string (source-reader--advance reader)) prefixes))
          ((char= character #\,)
           (source-reader--advance reader)
           (if (eql (source-reader--peek reader) #\@)
               (progn
                 (source-reader--advance reader)
                 (push ",@" prefixes))
               (push "," prefixes)))
          ((and (char= character #\#) (eql next #\'))
           (source-reader--advance reader)
           (source-reader--advance reader)
           (push "#'" prefixes))
          ((and (char= character #\#) (member next '(#\+ #\-)))
           (source-reader--advance reader)
           (source-reader--advance reader)
           (source-reader--skip-blank reader)
           (push (list (format nil "#~C" next) (source-reader--read-node reader)) prefixes))
          ((and (char= character #\#) (member next '(#\. #\p #\P)))
           (source-reader--advance reader)
           (push (format nil "#~C" (source-reader--advance reader)) prefixes))
          (t
           (return)))
        (source-reader--skip-blank reader)))
    (nreverse prefixes)))

(-> source-reader--read-node (source-reader) (option source-node))
(defun source-reader--read-node (reader)
  "Read the next node at READER's position, or return NIL at a closing delimiter or the end."
  (source-reader--skip-blank reader)
  (let ((prefix-start  (source-reader-position reader))
        (prefix-line   (source-reader-line reader))
        (prefix-column (source-reader-column reader))
        (prefixes      (source-reader--read-prefixes reader)))
    (let ((character (source-reader--peek reader)))
      (cond
        ((or (null character) (char= character #\)))
         nil)
        (t
         (let ((start  (source-reader-position reader))
               (line   (source-reader-line reader))
               (column (source-reader-column reader)))
           (multiple-value-bind (kind children)
               (cond
                 ((or (char= character #\()
                      (and (char= character #\#) (eql (source-reader--peek reader 1) #\()))
                  (values ':list (source-reader--read-list reader)))
                 ((char= character #\")
                  (source-reader--read-string reader)
                  (values ':string nil))
                 (t
                  (source-reader--read-atom reader)
                  (values ':atom nil)))
             (make-instance 'source-node
                            :kind          kind
                            :text          (subseq (source-reader-text reader) start
                                                   (source-reader-position reader))
                            :start         start
                            :end           (source-reader-position reader)
                            :line          line
                            :column        column
                            :end-line      (source-reader-line reader)
                            :prefix-start  prefix-start
                            :prefix-line   prefix-line
                            :prefix-column prefix-column
                            :prefixes      prefixes
                            :children      children))))))))

(-> source-reader--read-list (source-reader) list)
(defun source-reader--read-list (reader)
  "Read a list up to its closing delimiter and return its element nodes."
  (when (char= (source-reader--peek reader) #\#)
    (source-reader--advance reader))
  (source-reader--advance reader)
  (let ((children nil))
    (loop
      (source-reader--skip-blank reader)
      (let ((character (source-reader--peek reader)))
        (cond
          ((null character)
           (return))
          ((char= character #\))
           (source-reader--advance reader)
           (return))
          (t
           (let ((child (source-reader--read-node reader)))
             (if child
                 (push child children)
                 (return)))))))
    (nreverse children)))

(-> source-reader--read-string (source-reader) null)
(defun source-reader--read-string (reader)
  "Read a string literal, honoring backslash escapes."
  (source-reader--advance reader)
  (loop
    (let ((character (source-reader--peek reader)))
      (cond
        ((null character)
         (return))
        ((char= character #\\)
         (source-reader--advance reader)
         (when (source-reader--peek reader)
           (source-reader--advance reader)))
        ((char= character #\")
         (source-reader--advance reader)
         (return))
        (t
         (source-reader--advance reader)))))
  nil)

(-> source-reader--read-atom (source-reader) null)
(defun source-reader--read-atom (reader)
  "Read a symbol, number, character, or other token up to a delimiter."
  (let ((start (source-reader-position reader)))
    (when (and (eql (source-reader--peek reader) #\#)
               (eql (source-reader--peek reader 1) #\\))
      ;; A character's first character is never a delimiter: #\( or #\Space.
      (source-reader--advance reader)
      (source-reader--advance reader)
      (when (source-reader--peek reader)
        (source-reader--advance reader)))
    (loop
      (let ((character (source-reader--peek reader)))
        (cond
          ((null character)
           (return))
          ((char= character #\|)
           (source-reader--advance reader)
           (loop while (and (source-reader--peek reader)
                            (char/= (source-reader--peek reader) #\|))
                 do (when (char= (source-reader--peek reader) #\\)
                      (source-reader--advance reader))
                    (when (source-reader--peek reader)
                      (source-reader--advance reader)))
           (when (source-reader--peek reader)
             (source-reader--advance reader)))
          ((char= character #\\)
           (source-reader--advance reader)
           (when (source-reader--peek reader)
             (source-reader--advance reader)))
          ((or (source-reader--blank-p character)
               (find character "()\";`")
               (and (find character ",'") (> (source-reader-position reader) start)))
           (return))
          (t
           (source-reader--advance reader)))))
    (when (= start (source-reader-position reader))
      (source-reader--advance reader)))
  nil)
