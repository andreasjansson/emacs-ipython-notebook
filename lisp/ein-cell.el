;;; ein-cell.el --- Cell module

;; (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-cell.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-cell.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-cell.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Implementation note.  Current implementation of cell has redundant
;;  and not-guaranteed-to-be consistent information: `element' and
;;  `ein2:$node'.  This part must be moved to ein-node.el module to
;;  make it well capsuled.

;; IPython has cell.js, codecell.js and textcell.js.
;; But let's start with one file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ansi-color)
(require 'comint)

(require 'ein-core)
(require 'ein-log)
(require 'ein-node)
(require 'ein-kernel)
(require 'ein-output-area)


;;; Faces

(defface ein2:cell-input-prompt
  '((t :inherit header-line))
  "Face for cell input prompt"
  :group 'ein)

(defface ein2:cell-input-area
  '((((class color) (background light))
     :background "honeydew1")
    (((class color) (background dark))
     :background "#383838"))
  "Face for cell input area"
  :group 'ein)

(defface ein2:cell-heading-1
  '((t :height 1.1 :inherit ein2:cell-heading-2))
  "Face for level 1 heading."
  :group 'ein)

(defface ein2:cell-heading-2
  '((t :height 1.1 :inherit ein2:cell-heading-3))
  "Face for level 2 heading."
  :group 'ein)

(defface ein2:cell-heading-3
  '((t :height 1.1 :inherit ein2:cell-heading-4))
  "Face for level 3 heading."
  :group 'ein)

(defface ein2:cell-heading-4
  '((t :height 1.1 :inherit ein2:cell-heading-5))
  "Face for level 4 heading."
  :group 'ein)

(defface ein2:cell-heading-5
  '((t :height 1.1 :inherit ein2:cell-heading-6))
  "Face for level 5 heading."
  :group 'ein)

(defface ein2:cell-heading-6
  '((t :weight bold :inherit (variable-pitch ein2:cell-input-area)))
  "Face for level 6 heading."
  :group 'ein)

(defface ein2:cell-output-prompt
  '((t :inherit header-line))
  "Face for cell output prompt"
  :group 'ein)

(defface ein2:cell-output-stderr
  '((((class color) (background light))
     :background "PeachPuff")
    (((class color) (background dark))
     :background "#8c5353"))
  "Face for stderr cell output"
  :group 'ein)

(defface ein2:pos-tip-face
  '((t (:inherit 'popup-tip-face)))
  "Face for tooltip when using pos-tip backend."
  :group 'ein)


;;; Customization

(defcustom ein2:cell-traceback-level 1
  "Number of traceback stack to show.
Hidden tracebacks are not discarded.  You can always view them
using the command `ein2:notebook-view-traceback'."
  :type '(choice (integer :tag "Number of stack to show" 1)
                 (const :tag "Show all traceback" nil))
  :group 'ein)

(defcustom ein2:cell-max-num-outputs nil
  "Number of maximum outputs to be shown by default.
To view full output, use `ein2:notebook-show-in-shared-output'."
  :type '(choice (integer :tag "Number of outputs to show" 5)
                 (const :tag "Show all traceback" nil))
  :group 'ein)

(defcustom ein2:cell-autoexec-prompt "⚡"
  "String shown in the cell prompt when the auto-execution flag
is on.  See also `ein2:connect-aotoexec-lighter'."
  :type 'string
  :group 'ein)

(defcustom ein2:slice-image nil
  "[EXPERIMENTAL] When non-`nil', use `insert-sliced-image' when
drawing images.  If it is of the form of ``(ROWS COLS)``, it is
passed to the corresponding arguments of `insert-sliced-image'.

.. FIXME: ROWS and COLS must be determined dynamically by measuring
   the size of iamge and Emacs window.

See also: https://github.com/tkf/emacs-ipython-notebook/issues/94"
  :type 'boolean
  :group 'ein)



;;; EIEIO related utils

(defmacro ein2:oset-if-empty (obj slot value)
  `(unless (and (slot-boundp ,obj ,slot) (oref ,obj ,slot))
     (oset ,obj ,slot ,value)))

(defmacro ein2:oref-safe (obj slot)
  `(when (slot-boundp ,obj ,slot)
     (oref ,obj ,slot)))


;;; Utils
(defvar ein2:mime-type-map
  '((image/svg . svg) (image/png . png) (image/jpeg . jpeg)))

(defun ein2:insert-image (&rest args)
  (let* ((img (apply #'create-image args)))
    (if ein2:slice-image
        (destructuring-bind (&optional rows cols)
            (when (listp ein2:slice-image) ein2:slice-image)
          (insert-sliced-image img nil nil (or rows 20) cols))
      (insert-image img))))


;;; Cell classes

(defclass ein2:basecell ()
  ((cell-type :initarg :cell-type :type string)
   (read-only :initarg :read-only :initform nil :type boolean)
   (ewoc :initarg :ewoc :type ewoc)
   (element :initarg :element :initform nil :type list
    :documentation "ewoc nodes")
   (element-names :initarg :element-names)
   (input :initarg :input :type string
    :documentation "Place to hold data until it is rendered via `ewoc'.")
   (outputs :initarg :outputs :initform nil :type list)
   (metadata :initarg :metadata :initform nil :type list) ;; For nbformat >= 4
   (events :initarg :events :type ein2:events)
   (cell-id :initarg :cell-id :initform (ein2:utils-uuid) :type string))
  "Notebook cell base class")

(defclass ein2:codecell (ein2:basecell)
  ((cell-type :initarg :cell-type :initform "code")
   (kernel :initarg :kernel :type ein2:$kernel)
   (element-names :initform (:prompt :input :output :footer))
   (input-prompt-number :initarg :input-prompt-number
                        :documentation "\
Integer or \"*\" (running state).
Implementation note:
Typed `:input-prompt-number' becomes a problem when reading a
notebook that saved "*".  So don't add `:type'!")
   (collapsed :initarg :collapsed :initform nil :type boolean)
   (running :initarg :running :initform nil :type boolean)
   (dynamic :initarg :dynamic :initform nil :type boolean
            :documentation "\
Whether cell output is evaluated dynamically or not.

Only Emacs lisp type output data will be affected by this
slot (Javascript will not be evaluated).  This value must be set
to `t' when executing cell.  See `ein2:notebook-execute-cell'.
In the implantation of IPython web client it is passed around via
argument, but since it is difficult to pass argument to EWOC
pretty printer, `ein2:codecell' instance holds this setting in a
slot.")
   (autoexec :initarg :autoexec :initform nil :type boolean
             :documentation "Auto-execution flag.

This cell is executed when the connected buffer is saved,
provided that (1) this flag is `t' and (2) corresponding
auto-execution mode flag in the connected buffer is `t'.")))


(defclass ein2:textcell (ein2:basecell)
  ((cell-type :initarg :cell-type :initform "text")
   (element-names :initform (:prompt :input :footer))))

(defclass ein2:htmlcell (ein2:textcell)
  ((cell-type :initarg :cell-type :initform "html")))

(defclass ein2:markdowncell (ein2:textcell)
  ((cell-type :initarg :cell-type :initform "markdown")))

(defclass ein2:rawcell (ein2:textcell)
  ((cell-type :initarg :cell-type :initform "raw")))

(defclass ein2:headingcell (ein2:textcell)
  ((cell-type :initarg :cell-type :initform "heading")
   (level :initarg :level :initform 1)))


;;; Cell factory

(defun ein2:cell-class-from-type (type)
  (ein2:case-equal type
    (("code") 'ein2:codecell)
    (("text") 'ein2:textcell)
    (("html") 'ein2:htmlcell)
    (("markdown") 'ein2:markdowncell)
    (("raw") 'ein2:rawcell)
    (("heading") 'ein2:headingcell)
    ;; Defined in ein-shared-output.el:
    (("shared-output") 'ein2:shared-output-cell)
    (t (error "No cell type called %S" type))))

(defun ein2:cell-from-type (type &rest args)
  (apply (ein2:cell-class-from-type type) "Cell" args))

(defun ein2:cell-from-json (data &rest args)
  (let ((cell (ein2:cell-init (apply #'ein2:cell-from-type
                                    (plist-get data :cell_type) args)
                             data)))
    (if (plist-get data :metadata)
        (ein2:oset-if-empty cell :metadata (plist-get data :metadata)))
    cell))

(defmethod ein2:cell-init ((cell ein2:codecell) data)
  (ein2:oset-if-empty cell :outputs (plist-get data :outputs))
  (ein2:oset-if-empty cell :input (or (plist-get data :input)
                                     (plist-get data :source)))
  (ein2:aif (plist-get data :prompt_number)
      (ein2:oset-if-empty cell :input-prompt-number it)
    (ein2:aif (plist-get data :execution_count)
        (ein2:oset-if-empty cell :input-prompt-number it)))
  (ein2:oset-if-empty cell :collapsed
                     (let ((v (or (plist-get data :collapsed)
                                  (plist-get (slot-value cell 'metadata)
                                             :collapsed))))
                       (if (eql v json-false) nil v)))
  cell)

(defmethod ein2:cell-init ((cell ein2:textcell) data)
  (ein2:aif (plist-get data :source)
      (oset cell :input it))
  cell)

(defmethod ein2:cell-init ((cell ein2:headingcell) data)
  (call-next-method)
  (ein2:aif (plist-get data :level)
      (oset cell :level it))
  cell)

(defmethod ein2:cell-convert ((cell ein2:basecell) type)
  (let ((new (ein2:cell-from-type type)))
    ;; copy attributes
    (loop for k in '(:read-only :ewoc)
          do (set-slot-value new k (slot-value cell k)))
    ;; copy input
    (oset new :input (if (ein2:cell-active-p cell)
                         (ein2:cell-get-text cell)
                       (oref cell :input)))
    ;; copy output when the new cell has it
    (when (memq :output (oref new :element-names))
      (oset new :outputs (mapcar 'identity (oref cell :outputs))))
    new))

(defmethod ein2:cell-convert ((cell ein2:codecell) type)
  (let ((new (call-next-method)))
    (when (and (ein2:codecell-child-p new)
               (slot-boundp cell :kernel))
      (oset new :kernel (oref cell :kernel)))
    new))

(defmethod ein2:cell-convert ((cell ein2:headingcell) type)
  (let ((new (call-next-method)))
    (when (ein2:headingcell-p new)
      (oset new :level (oref cell :level)))
    new))

(defmethod ein2:cell-copy ((cell ein2:basecell))
  (ein2:cell-convert cell (oref cell :cell-type)))

(defmethod ein2:cell-convert-inplace ((cell ein2:basecell) type)
  "Convert CELL to TYPE and redraw corresponding ewoc nodes."
  (let ((new (ein2:cell-convert cell type)))
    ;; copy element attribute
    (loop for k in (oref new :element-names)
          with old-element = (oref cell :element)
          do (oset new :element
                   (plist-put (oref new :element) k
                              (plist-get old-element k))))
    ;; setting ewoc nodes
    (loop for en in (ein2:cell-all-element cell)
          for node = (ewoc-data en)
          do (setf (ein2:$node-data node) new))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))         ; disable undo recording
      ;; delete ewoc nodes that is not copied
      (apply
       #'ewoc-delete (oref new :ewoc)
       (apply
        #'append
        (loop for name in (oref cell :element-names)
              unless (memq name (oref new :element-names))
              collect (let ((ens (ein2:cell-element-get cell name)))
                        (if (listp ens) ens (list ens))))))
      ;; draw ewoc node
      (loop with ewoc = (oref new :ewoc)
            for en in (ein2:cell-all-element new)
            do (ewoc-invalidate ewoc en)))
    new))

(defmethod ein2:cell-change-level ((cell ein2:headingcell) level)
  (assert (integerp level))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))         ; disable undo recording
    (oset cell :level level)
    ;; draw ewoc node
    (loop with ewoc = (oref cell :ewoc)
          for en in (ein2:cell-all-element cell)
          do (ewoc-invalidate ewoc en))))


;;; Getter/setter

(defmethod ein2:cell-num-outputs ((cell ein2:codecell))
  (length (oref cell :outputs)))

(defmethod ein2:cell-num-outputs ((cell ein2:textcell))
  0)

(defmethod ein2:cell-element-get ((cell ein2:basecell) prop &rest args)
  "Return ewoc node named PROP in CELL.
If PROP is `:output' a list of ewoc nodes is returned.
A specific node can be specified using optional ARGS."
  (if (memq prop (oref cell :element-names))
      (plist-get (oref cell :element) prop)
    (error "PROP %s is not supported." prop)))

(defmethod ein2:cell-element-get ((cell ein2:codecell) prop &optional index)
  (let ((element (oref cell :element)))
    (if index
        (progn
          (assert (eql prop :output))
          (nth index (plist-get element prop)))
      (case prop
        (:after-input
         (ein2:aif (nth 0 (plist-get element :output))
             it
           (plist-get element :footer)))
        (:after-output (plist-get element :footer))
        (:before-input (plist-get element :prompt))
        (:before-output (plist-get element :input))
        (:last-output
         (ein2:aif (plist-get element :output)
             (car (last it))
           (plist-get element :input)))
        (t (call-next-method))))))

(defmethod ein2:cell-element-get ((cell ein2:textcell) prop)
  (let ((element (oref cell :element)))
    (case prop
      (:after-input (plist-get element :footer))
      (:before-input (plist-get element :prompt))
      (t (call-next-method)))))

(defmethod ein2:cell-all-element ((cell ein2:basecell))
  (list (ein2:cell-element-get cell :prompt)
        (ein2:cell-element-get cell :input)
        (ein2:cell-element-get cell :footer)))

(defmethod ein2:cell-all-element ((cell ein2:codecell))
  (append (call-next-method)
          (ein2:cell-element-get cell :output)))

(defmethod ein2:cell-language ((cell ein2:basecell))
  "Programming language used for CELL.
Return language name as a string or `nil' when not defined.

\(fn cell)")

(defmethod ein2:cell-language ((cell ein2:codecell)) nil "python")
(defmethod ein2:cell-language ((cell ein2:markdowncell)) nil "markdown")
(defmethod ein2:cell-language ((cell ein2:htmlcell)) nil "html")
(defmethod ein2:cell-language ((cell ein2:rawcell)) nil "rst")


;; EWOC

(defun ein2:cell-make-element (make-node num-outputs)
  (let ((buffer-undo-list t))           ; disable undo recording
    (list
     :prompt (funcall make-node 'prompt)
     :input  (funcall make-node 'input)
     :output (loop for i from 0 below num-outputs
                   collect (funcall make-node 'output i))
     :footer (funcall make-node 'footer))))

(defmethod ein2:cell-enter-last ((cell ein2:basecell))
  (let* ((ewoc (oref cell :ewoc))
         ;; Use `cell' as data for ewoc.  Use the whole cell data even
         ;; if it is not used, to access it from the notebook buffer.
         ;; It is equivalent to `this.element.data("cell", this)' in
         ;; IPython.Cell (see cell.js).
         (make-node
          (lambda (&rest path)
            (ewoc-enter-last ewoc (ein2:node-new `(cell ,@path) cell))))
         (element (ein2:cell-make-element make-node
                                         (ein2:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defmethod ein2:cell-enter-first ((cell ein2:basecell))
  (let* ((ewoc (oref cell :ewoc))
         (node nil)
         (make-node
          (lambda (&rest path)
            (let ((ewoc-data (ein2:node-new `(cell ,@path) cell)))
              (setq node
                    (if node
                        (ewoc-enter-after ewoc node ewoc-data)
                      (ewoc-enter-first ewoc ewoc-data))))))
         (element (ein2:cell-make-element make-node
                                         (ein2:cell-num-outputs cell))))
    (oset cell :element element)
    cell))

(defmethod ein2:cell-insert-below ((base-cell ein2:basecell) other-cell)
  (let* ((ewoc (oref base-cell :ewoc))
         (node (ein2:cell-element-get base-cell :footer))
         (make-node
          (lambda (&rest path)
            (setq node (ewoc-enter-after
                        ewoc node (ein2:node-new `(cell ,@path) other-cell)))))
         (element (ein2:cell-make-element make-node
                                         (ein2:cell-num-outputs other-cell))))
    (oset other-cell :element element)
    other-cell))

(defun ein2:cell-pp (path data)
  (case (car path)
    (prompt (ein2:cell-insert-prompt data))
    (input  (ein2:cell-insert-input data))
    (output (ein2:cell-insert-output (cadr path) data))
    (footer (ein2:cell-insert-footer data))))

(defmethod ein2:cell-insert-prompt ((cell ein2:codecell))
  "Insert prompt of the CELL in the buffer.
Called from ewoc pretty printer via `ein2:cell-pp'."
  ;; Newline is inserted in `ein2:cell-insert-input'.
  (ein2:insert-read-only
   (concat
    (format "In [%s]:" (or (ein2:oref-safe cell :input-prompt-number)  " "))
    (when (oref cell :autoexec) " %s" ein2:cell-autoexec-prompt))
   'font-lock-face 'ein2:cell-input-prompt))

(defmethod ein2:cell-insert-prompt ((cell ein2:textcell))
  (ein2:insert-read-only
   (format "%s:" (oref cell :cell-type))
   'font-lock-face 'ein2:cell-input-prompt))

(defmethod ein2:cell-insert-prompt ((cell ein2:headingcell))
  (ein2:insert-read-only
   (format "h%s:" (oref cell :level))
   'font-lock-face 'ein2:cell-input-prompt))

(defmethod ein2:cell-insert-input ((cell ein2:basecell))
  "Insert input of the CELL in the buffer.
Called from ewoc pretty printer via `ein2:cell-pp'."
  (let ((start (1+ (point))))
    ;; Newlines must allow insertion before/after its position.
    (insert (propertize "\n" 'read-only t 'rear-nonsticky t)
            (or (ein2:oref-safe cell :input) "")
            (propertize "\n" 'read-only t))
    ;; Highlight background using overlay.
    (let ((ol (make-overlay start (point))))
      (overlay-put ol 'face (ein2:cell-get-input-area-face cell))
      ;; `evaporate' = `t': Overlay is deleted when the region become empty.
      (overlay-put ol 'evaporate t))))

(defmethod ein2:cell-get-input-area-face ((cell ein2:basecell))
  "Return the face (symbol) for input area."
  'ein2:cell-input-area)

(defmethod ein2:cell-get-input-area-face ((cell ein2:headingcell))
  (intern (format "ein2:cell-heading-%d" (oref cell :level))))

(defun ein2:cell-insert-output (index cell)
  "Insert INDEX-th output of the CELL in the buffer.
Called from ewoc pretty printer via `ein2:cell-pp'."
  (if (or (oref cell :collapsed)
          (and ein2:cell-max-num-outputs
               (>= index ein2:cell-max-num-outputs)))
      (progn
        (when (and (not (oref cell :collapsed))
                   (= index ein2:cell-max-num-outputs)
                   (> (point) (point-at-bol)))
          ;; The first output which exceeds `ein2:cell-max-num-outputs'.
          (ein2:insert-read-only "\n"))
        (ein2:insert-read-only "."))
    (let ((out (nth index (oref cell :outputs))))
      ;; Handle newline for previous stream output.
      ;; In IPython JS, it is handled in `append_stream' because JS
      ;; does not need to care about newline (DOM does it for JS).
      ;; FIXME: Maybe I should abstract ewoc in some way and get rid
      ;;        of this.
      (let ((last-out (and (> index 0)
                           (nth (1- index) (oref cell :outputs)))))
        ;; If previous output is stream type, consider adding newline
        (when (and last-out
                   (equal (plist-get last-out :output_type) "stream"))
          ;; Check if the last output is from the same stream.
          ;; If so, do *NOT* insert newline, otherwise insert newline.
          (unless (and (equal (plist-get out :output_type) "stream")
                       (equal (plist-get out      :stream)
                              (plist-get last-out :stream)))
            (ein2:cell-append-stream-text-fontified "\n" last-out))))
      ;; Finally insert real data
      (ein2:case-equal (plist-get out :output_type)
        (("pyout")        (ein2:cell-append-pyout        cell out))
        (("pyerr")        (ein2:cell-append-pyerr        cell out))
        (("display_data") (ein2:cell-append-display-data cell out))
        (("execute_result") (ein2:cell-append-display-data cell out))
        (("stream")       (ein2:cell-append-stream       cell out))))))

(defmethod ein2:cell-insert-footer ((cell ein2:basecell))
  "Insert footer (just a new line) of the CELL in the buffer.
Called from ewoc pretty printer via `ein2:cell-pp'."
  (ein2:insert-read-only "\n"))

(defmethod ein2:cell-insert-footer ((cell ein2:codecell))
  (if (or (oref cell :collapsed)
          (and ein2:cell-max-num-outputs
               (> (ein2:cell-num-outputs cell) ein2:cell-max-num-outputs)))
      ;; Add a newline after the last ".".
      (ein2:insert-read-only "\n")
    (let ((last-out (car (last (oref cell :outputs)))))
      (when (equal (plist-get last-out :output_type) "stream")
        (ein2:cell-append-stream-text-fontified "\n" last-out))))
  (call-next-method))


(defun ein2:cell-node-p (node &optional element-name)
  (let* ((path (ein2:$node-path node))
         (p0 (car path))
         (p1 (cadr path))
         (cell (ein2:$node-path node)))
    (and cell (eql p0 'cell) (or (not element-name) (eql p1 element-name)))))

(defun ein2:cell-ewoc-node-p (ewoc-node &optional element-name)
  (ein2:cell-node-p (ewoc-data ewoc-node) element-name))

(defun ein2:cell-from-ewoc-node (ewoc-node)
  (ein2:aand ewoc-node (ewoc-data it) (ein2:$node-data it)))

(defmethod ein2:cell-input-pos-min ((cell ein2:basecell))
  "Return editable minimum point in the input area of the CELL.
If the input area of the CELL does not exist, return `nil'"
  (let* ((input-node (ein2:cell-element-get cell :input)))
    ;; 1+ for skipping newline
    (when input-node (1+ (ewoc-location input-node)))))

(defmethod ein2:cell-input-pos-max ((cell ein2:basecell))
  "Return editable maximum point in the input area of the CELL.
If the input area of the CELL does not exist, return `nil'"
  (let* ((ewoc (oref cell :ewoc))
         (input-node (ein2:cell-element-get cell :input)))
    ;; 1- for skipping newline
    (when input-node (1- (ewoc-location (ewoc-next ewoc input-node))))))

(defmethod ein2:cell-get-text ((cell ein2:basecell))
  "Grab text in the input area of the cell at point."
  (if (ein2:cell-active-p cell)
      (let* ((beg (ein2:cell-input-pos-min cell))
             (end (ein2:cell-input-pos-max cell)))
        (buffer-substring beg end))
    (oref cell :input)))

(defmethod ein2:cell-set-text ((cell ein2:basecell) text)
  (let* ((input-node (ein2:cell-element-get cell :input))
         (ewoc (oref cell :ewoc))
           ;; 1+/1- is for skipping newline
         (beg (1+ (ewoc-location input-node)))
         (end (1- (ewoc-location (ewoc-next ewoc input-node)))))
    (save-excursion
      ;; probably it is better to set :input and update via ewoc?
      (goto-char beg)
      (delete-region beg end)
      (insert text))))

(defmethod ein2:cell-save-text ((cell ein2:basecell))
  (oset cell :input (ein2:cell-get-text cell)))

(defmethod ein2:cell-deactivate ((cell ein2:basecell))
  (oset cell :element nil)
  cell)

(defmethod ein2:cell-active-p ((cell ein2:basecell))
  (oref cell :element))

(defmethod ein2:cell-running-set ((cell ein2:codecell) running)
  ;; FIXME: change the appearance of the cell
  (oset cell :running running))

(defmethod ein2:cell-set-collapsed ((cell ein2:codecell) collapsed)
  "Set `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (unless (eq (oref cell :collapsed) collapsed)
    (oset cell :collapsed collapsed)
    (apply #'ewoc-invalidate
           (oref cell :ewoc)
           (ein2:cell-element-get cell :output))))

(defmethod ein2:cell-collapse ((cell ein2:codecell))
  (ein2:cell-set-collapsed cell t))

(defmethod ein2:cell-expand ((cell ein2:codecell))
  (ein2:cell-set-collapsed cell nil))

(defmethod ein2:cell-toggle-output ((cell ein2:codecell))
  "Toggle `:collapsed' slot of CELL and invalidate output ewoc nodes."
  (ein2:cell-set-collapsed cell (not (oref cell :collapsed))))

(defmethod ein2:cell-invalidate-prompt ((cell ein2:codecell))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))           ; disable undo recording
    (ewoc-invalidate (oref cell :ewoc)
                     (ein2:cell-element-get cell :prompt))))

(defmethod ein2:cell-set-input-prompt ((cell ein2:codecell) &optional number)
  (oset cell :input-prompt-number number)
  (ein2:cell-invalidate-prompt cell))

(defmethod ein2:cell-set-autoexec ((cell ein2:codecell) bool)
  "Set auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (oset cell :autoexec bool)
  (ein2:cell-invalidate-prompt cell))

(defmethod ein2:cell-autoexec-p ((cell ein2:basecell))
  "Auto-execution flag set to CELL.
Return `nil' always for non-code cells."
  nil)

(defmethod ein2:cell-autoexec-p ((cell ein2:codecell))
  (oref cell :autoexec))

(defmethod ein2:cell-toggle-autoexec ((cell ein2:codecell))
  "Toggle auto-execution flag of CELL to BOOL and invalidate the
prompt EWOC node."
  (ein2:cell-set-autoexec cell (not (ein2:cell-autoexec-p cell))))

(defmethod ein2:cell-goto ((cell ein2:basecell) &optional relpos prop)
  "Go to the input area of the given CELL.
RELPOS is the position relative to the input area.  Default is 0.
PROP is a name of cell element.  Default is `:input'.

\(fn cell relpos prop)"
  (unless relpos (setq relpos 0))
  (unless prop (setq prop :input))
  (ewoc-goto-node (oref cell :ewoc) (ein2:cell-element-get cell prop))
  (let ((offset (case prop
                  ((:input :before-output) 1)
                  (:after-input -1)
                  (t 0))))
    (forward-char (+ relpos offset))))

(defmethod ein2:cell-relative-point ((cell ein2:basecell) &optional pos)
  "Return the point relative to the input area of CELL.
If the position POS is not given, current point is considered."
  (unless pos (setq pos (point)))
  (- pos (1+ (ewoc-location (ein2:cell-element-get cell :input)))))

(defmethod ein2:cell-location ((cell ein2:basecell) &optional elm end)
  "Return the starting location of CELL.
ELM is a name (keyword) of element that `ein2:cell-element-get'
understands.  Note that you can't use `:output' since it returns
a list.  Use `:after-input' instead.
If END is non-`nil', return the location of next element."
  (unless elm (setq elm :prompt))
  (let ((element (oref cell :element)))
    (when end
      (setq elm (case elm
                  (:prompt :input)
                  (:input :after-input)
                  (:output :after-output)))
      (unless elm
        (setq cell (ein2:cell-next cell))
        (setq elm :prompt)))
    (if cell
        (ewoc-location (ein2:cell-element-get cell elm))
      (assert end)
      (point-max))))

(defmethod ein2:cell-buffer ((cell ein2:basecell))
  "Return a buffer associated by CELL (if any)."
  (ein2:aand (ein2:oref-safe cell :ewoc) (ewoc-buffer it)))


;; Data manipulation

(defmethod ein2:cell-clear-output ((cell ein2:codecell) stdout stderr other)
  ;; codecell.js in IPytohn implements it using timeout and callback.
  ;; As it is unclear why timeout is needed, just clear output
  ;; instantaneously for now.
  (ein2:log 'debug "cell-clear-output stdout=%s stderr=%s other=%s"
           stdout stderr other)
  (let ((ewoc (oref cell :ewoc))
        (output-nodes (ein2:cell-element-get cell :output)))
    (if (and stdout stderr other)
        (progn
          ;; clear all
          (let ((inhibit-read-only t)
                (buffer-undo-list t))   ; disable undo recording
            (apply #'ewoc-delete ewoc output-nodes))
          (plist-put (oref cell :element) :output nil)
          (oset cell :outputs nil))
      (let* ((ewoc-node-list
              (append
               (when stdout (ein2:node-filter output-nodes :is 'output-stdout))
               (when stderr (ein2:node-filter output-nodes :is 'output-stderr))
               (when stdout (ein2:node-filter output-nodes
                                             :is 'output-subarea
                                             :not 'output-stderr
                                             :not 'output-stdout))))
             (indices
              (mapcar (lambda (n) (last (ein2:$node-path (ewoc-data n))))
                      ewoc-node-list)))
        ;; remove from buffer
        (let ((inhibit-read-only t)
              (buffer-undo-list t))   ; disable undo recording
          (apply #'ewoc-delete ewoc ewoc-node-list))
        ;; remove from `:element'
        (let* ((element (oref cell :element))
               (old-output (plist-get element :output))
               (new-ouptut (ein2:remove-by-index old-output indices)))
          (plist-put element :output new-ouptut))
        ;; remove cleared outputs from internal data
        (oset cell :outputs
              (ein2:remove-by-index (oref cell :outputs) indices))))
    ;; Footer may have extra (possibly colored) newline due to the
    ;; last output type.  So invalidate it here.
    ;; See `ein2:cell-insert-footer' (for codecell).
    (ewoc-invalidate ewoc (ein2:cell-element-get cell :footer))))

(defun ein2:cell-output-json-to-class (json)
  (ein2:case-equal (plist-get json :output_type)
    (("pyout")
     '(output-subarea))
    (("pyerr")
     '(output-subarea))
    (("error")
     '(output-subarea))
    (("display_data")
     '(output-subarea))
    (("execute_result")
     '(output-subarea))
    (("stream")
     (list 'output-stream 'output-subarea
           (intern (format "output-%s" (plist-get json :stream)))))))

(defmethod ein2:cell-append-output ((cell ein2:codecell) json dynamic)
  (ein2:cell-expand cell)
  ;; (ein2:flush-clear-timeout)
  (oset cell :outputs
        (append (oref cell :outputs) (list json)))
  ;; enter last output element
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)           ; disable undo recording
         (ewoc (oref cell :ewoc))
         (index (1- (ein2:cell-num-outputs cell)))
         (path `(cell output ,index))
         (class (ein2:cell-output-json-to-class json))
         (data (ein2:node-new path cell class))
         (last-node (ein2:cell-element-get cell :last-output))
         (ewoc-node (ewoc-enter-after ewoc last-node data))
         (element (oref cell :element)))
    (plist-put element :output
               (append (plist-get element :output) (list ewoc-node)))
    (ewoc-invalidate ewoc (ein2:cell-element-get cell :footer))))

(defmethod ein2:cell-append-pyout ((cell ein2:codecell) json)
  "Insert pyout type output in the buffer.
Called from ewoc pretty printer via `ein2:cell-insert-output'."
  (ein2:insert-read-only (format "Out [%s]:"
                                (or (plist-get json :prompt_number) " "))
                        'font-lock-face 'ein2:cell-output-prompt)
  (ein2:insert-read-only "\n")
  (ein2:cell-append-mime-type json (oref cell :dynamic))
  (ein2:insert-read-only "\n"))

(defmethod ein2:cell-append-pyerr ((cell ein2:codecell) json)
  "Insert pyerr type output in the buffer.
Called from ewoc pretty printer via `ein2:cell-insert-output'."
  (mapc (lambda (tb)
          (ein2:cell-append-text tb)
          (ein2:cell-append-text "\n"))
        (let ((tb (plist-get json :traceback))
              (level ein2:cell-traceback-level))
          (if (and level (> (- (length tb) 2) level))
              (cons "\nTruncated Traceback (Use C-c C-x to view full TB):"
                    (last tb (1+ level)))
            tb)))
  (ein2:insert-read-only "\n"))

(ein2:deflocal ein2:%cell-append-stream-last-cell% nil
  "The last cell in which `ein2:cell-append-stream' is used.")

(defmethod ein2:cell-append-stream ((cell ein2:codecell) json)
  "Insert stream type output in the buffer.
Called from ewoc pretty printer via `ein2:cell-insert-output'."
  (unless (plist-get json :stream)
    (plist-put json :stream "stdout"))
  (unless (eq cell ein2:%cell-append-stream-last-cell%)
    ;; Avoid applying unclosed ANSI escape code in the cell.  Note
    ;; that I don't need to distinguish stdout/stderr because it looks
    ;; like normal terminal does not.
    (setq ansi-color-context nil))
  (let ((start (point)))
    (ein2:cell-append-stream-text-fontified (or (plist-get json :text) "") json)
    (comint-carriage-motion start (point)))
  ;; NOTE: newlines for stream is handled in `ein2:cell-insert-output'.
  ;; So do not insert newline here.
  (setq ein2:%cell-append-stream-last-cell% cell))

(defun ein2:cell-append-stream-text-fontified (text json)
  "Insert TEXT with font properties defined by JSON data."
  (if (equal (plist-get json :stream) "stderr")
      (ein2:cell-append-text text 'font-lock-face 'ein2:cell-output-stderr)
    (ein2:cell-append-text text)))

(defmethod ein2:cell-append-display-data ((cell ein2:codecell) json)
  "Insert display-data type output in the buffer.
Called from ewoc pretty printer via `ein2:cell-insert-output'."
  (ein2:cell-append-mime-type json (oref cell :dynamic))
  (ein2:insert-read-only "\n"))

(defcustom ein2:output-type-preference
  (if (and (fboundp 'shr-insert-document)
           (fboundp 'libxml-parse-xml-region))
      #'ein2:output-type-prefer-pretty-text-over-html
    '(emacs-lisp svg image/svg png image/png jpeg image/jpeg text text/plain html text/html latex text/latex javascript text/javascript))
  "Output types to be used in notebook.
First output-type found in this list will be used.
This variable can be a list or a function returning a list given
DATA plist.
See also `ein2:output-type-prefer-pretty-text-over-html'.

**Example**:
If you prefer HTML type over text type, you can set it as::

    (setq ein2:output-type-preference
          '(emacs-lisp svg png jpeg html text latex javascript))

Note that ``html`` comes before ``text``."
  :group 'ein)

(defun ein2:output-type-prefer-pretty-text-over-html (data)
  "Use text type if it is a \"prettified\" text instead of HTML.
This is mostly for *not* using HTML table for pandas but using
HTML for other object.

If the text type output contains a newline, it is assumed be a
prettified text thus be used instead of HTML type."
  (if (ein2:aand (plist-get data :text) (string-match-p "\n" it))
      '(emacs-lisp svg image/svg png image/png jpeg image/jpeg text text/plain html text/html latex text/latex javascript text/javascript)
    '(emacs-lisp svg image/svg png image/png jpeg image/jpeg html text/html text text/plain latex text/latex javascript text/javascript)))

(defun ein2:fix-mime-type (type)
  (ein2:aif (assoc type ein2:mime-type-map)
      (cdr it)
    type))

(defun ein2:cell-append-mime-type (json dynamic)
  (when (plist-get json :data)
    (setq json (plist-get json :data))) ;; For nbformat v4 support.
  (loop
   for key in (cond
               ((functionp ein2:output-type-preference)
                (funcall ein2:output-type-preference json))
               (t ein2:output-type-preference))
   for type = (intern (format ":%s" key)) ; something like `:text'
   for value = (plist-get json type)      ; FIXME: optimize
   when (plist-member json type)
   return
   (case key
     ;; NOTE: Normally `javascript' and `html' will not be inserted as
     ;; they come out after `text'.  Maybe it is better to inform user
     ;; when one of them is inserted.
     ((javascript text/javascript)
      (when dynamic
        (ein2:log 'info (concat "ein2:cell-append-mime-type does not support "
                               "dynamic javascript. got: %s") value))
      (ein2:insert-read-only (plist-get json type)))
     (emacs-lisp
      (when dynamic
        (ein2:cell-safe-read-eval-insert (plist-get json type))))
     ((html text/html)
      (funcall (ein2:output-area-get-html-renderer) (plist-get json type)))
     ((latex text/latex text text/plain)
      (ein2:insert-read-only (plist-get json type)))
     ((svg image/svg)
      (ein2:insert-image value (ein2:fix-mime-type key) t))
     ((png image/png jpeg image/jpeg)
      (ein2:insert-image (base64-decode-string value) (ein2:fix-mime-type key) t)))))

(defun ein2:cell-append-text (data &rest properties)
  ;; escape ANSI in plaintext:
  (apply #'ein2:insert-read-only (ansi-color-apply data) properties))

(defun ein2:cell-safe-read-eval-insert (text)
  (ein2:insert-read-only
   (condition-case err
       (save-excursion
         ;; given code can be `pop-to-buffer' or something.
         (format "%S" (eval (read text))))
     (error
      (ein2:log 'warn "Got an error while executing: '%s'"
               text)
      (format "Error: %S" err)))))

(defmethod ein2:cell-to-json ((cell ein2:codecell) &optional discard-output)
  "Return json-ready alist."
  `((input . ,(ein2:cell-get-text cell))
    (cell_type . "code")
    ,@(ein2:aif (ein2:oref-safe cell :input-prompt-number)
          `((prompt_number . ,it)))
    (outputs . ,(if discard-output [] (apply #'vector (oref cell :outputs))))
    (language . "python")
    (collapsed . ,(if (oref cell :collapsed) t json-false))))

(defvar ein2:output-type-map
  '((:svg . :image/svg) (:png . :image/png) (:jpeg . :image/jpeg)
    (:text . :text/plain)
    (:html . :text/html) (:latex . :text/latex) (:javascript . :text/javascript)))

(defun ein2:output-property-p (maybe-property)
  (assoc maybe-property ein2:output-type-map))

(defmethod ein2:cell-to-nb4-json ((cell ein2:codecell) &optional discard-output)
  (let ((metadata `((collapsed . ,(if (oref cell :collapsed) t json-false))))
        (outputs (if discard-output []
                   (oref cell :outputs)))
        (renamed-outputs '()))
    (unless discard-output
      (dolist (output outputs)
        (let ((otype (plist-get output :output_type)))
          (ein2:log 'info "Saving output of type %S" otype)
          (if (and (or (equal otype "display_data")
                       (equal otype "execute_result"))
                   (null (plist-get output :metadata)))
              (plist-put output :metadata (make-hash-table)))
          (push (let ((ocopy (copy-list output))
                      (new-output '()))
                  (loop while ocopy
                        do (let ((prop (pop ocopy))
                                 (value (pop ocopy)))
                             (ein2:log 'info "Checking property %s for output type '%s'"
                                      prop otype)
                             (cond
                              ((equal prop :stream) (progn (push value new-output)
                                                                (push :name new-output)))

                              ((and (equal otype "display_data")
                                    (ein2:output-property-p prop))
                               (let ((new-prop (cdr (ein2:output-property-p prop))))
                                 (push (list new-prop (list value)) new-output)
                                 (push :data new-output)))

                              ((and (equal otype "display_data")
                                    (equal prop :text))
                               (ein2:log 'info "SAVE-NOTEBOOK: Skipping unnecessary :text data."))

                              ((and (equal otype "execute_result")
                                    (equal prop :text))
                               (ein2:log 'info "Fixing execute_result (%s?)." otype)
                               (let ((new-prop (cdr (ein2:output-property-p prop))))
                                 (push (list new-prop (list value)) new-output)
                                 (push :data new-output)))

                              ((and (equal otype "execute_result")
                                    (equal prop :prompt_number))
                               (ein2:log 'info "SAVE-NOTEBOOK: Fixing prompt_number property.")
                               (push value new-output)
                               (push :execution_count new-output))

                              (t (progn (push value new-output) (push prop new-output)))))
                        finally return new-output))
                renamed-outputs))))
    `((source . ,(ein2:cell-get-text cell))
      (cell_type . "code")
      ,@(ein2:aif (ein2:oref-safe cell :input-prompt-number)
            `((execution_count . ,it))
          `((execution_count)))
      (outputs . ,(apply #'vector (or renamed-outputs outputs)))
      (metadata . ,metadata))))

(defmethod ein2:cell-to-json ((cell ein2:textcell) &optional discard-output)
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein2:cell-get-text cell))))

(defmethod ein2:cell-to-nb4-json ((cell ein2:textcell) &optional discard-output)
  `((cell_type . ,(oref cell :cell-type))
    (source    . ,(ein2:cell-get-text cell))
    (metadata . ((collapsed . t)))))

(defmethod ein2:cell-to-json ((cell ein2:headingcell) &optional discard-output)
  (let ((json (call-next-method)))
    (append json `((level . ,(oref cell :level))))))

(defmethod ein2:cell-next ((cell ein2:basecell))
  "Return next cell of the given CELL or nil if CELL is the last one."
  (ein2:aif (ewoc-next (oref cell :ewoc)
                      (ein2:cell-element-get cell :footer))
      (let ((cell (ein2:$node-data (ewoc-data it))))
        (when (ein2:basecell-child-p cell)
          cell))))

(defmethod ein2:cell-prev ((cell ein2:basecell))
  "Return previous cell of the given CELL or nil if CELL is the first one."
  (ein2:aif (ewoc-prev (oref cell :ewoc)
                      (ein2:cell-element-get cell :prompt))
      (let ((cell (ein2:$node-data (ewoc-data it))))
        (when (ein2:basecell-child-p cell)
          cell))))


;;; Kernel related calls.

(defmethod ein2:cell-set-kernel ((cell ein2:codecell) kernel)
  (oset cell :kernel kernel))


(defmethod ein2:cell-execute ((cell ein2:codecell))
  (ein2:cell-execute-internal cell
                             (oref cell :kernel)
                             (ein2:cell-get-text cell)
                             :silent nil))

(defmethod ein2:cell-execute-internal ((cell ein2:codecell)
                                      kernel code &rest args)
  (ein2:cell-clear-output cell t t t)
  (ein2:cell-set-input-prompt cell "*")
  (ein2:cell-running-set cell t)
  (oset cell :dynamic t)
  (apply #'ein2:kernel-execute kernel code (ein2:cell-make-callbacks cell) args))

(defmethod ein2:cell-make-callbacks ((cell ein2:codecell))
  (list
   :execute_reply  (cons #'ein2:cell--handle-execute-reply  cell)
   :output         (cons #'ein2:cell--handle-output         cell)
   :clear_output   (cons #'ein2:cell--handle-clear-output   cell)
   :set_next_input (cons #'ein2:cell--handle-set-next-input cell)))

(defmethod ein2:cell--handle-execute-reply ((cell ein2:codecell) content
                                           -metadata-not-used-)
  (ein2:cell-set-input-prompt cell (plist-get content :execution_count))
  (ein2:cell-running-set cell nil)
  (let ((events (oref cell :events)))
    (ein2:events-trigger events 'set_dirty.Worksheet (list :value t :cell cell))
    (ein2:events-trigger events 'maybe_reset_undo.Worksheet cell)))

(defmethod ein2:cell--handle-set-next-input ((cell ein2:codecell) text)
  (let ((events (oref cell :events)))
    (ein2:events-trigger events 'set_next_input.Worksheet
                        (list :cell cell :text text))
    (ein2:events-trigger events 'maybe_reset_undo.Worksheet cell)))



;;; Output area

;; These function should go to ein-output-area.el.  But as cell and
;; EWOC is connected in complicated way, I will leave them in
;; ein-cell.el.

(defmethod ein2:cell--handle-output ((cell ein2:codecell) msg-type content
                                    -metadata-not-used-)
  (let* ((json (list :output_type msg-type)))
    (ein2:case-equal msg-type
      (("stream")
       (plist-put json :text (or (plist-get content :data)
                                 (plist-get content :text))) ;; Horrible hack to deal with version 5.0 of messaging protocol.
       (plist-put json :stream (plist-get content :name)))
      (("display_data" "pyout" "execute_result") ;; in v4 nbformat execute_result == pyout
       (when (or (equal msg-type "pyout")
                 (equal msg-type "execute_result"))
         (plist-put json :prompt_number (plist-get content :execution_count)))
       (setq json (ein2:output-area-convert-mime-types
                   json (plist-get content :data)))
       )
      (("pyerr" "error")
       (plist-put json :ename (plist-get content :ename))
       (plist-put json :evalue (plist-get content :evalue))
       (plist-put json :traceback (plist-get content :traceback))))
    (ein2:cell-append-output cell json t)
    ;; (oset cell :dirty t)
    (ein2:events-trigger (oref cell :events) 'maybe_reset_undo.Worksheet cell)))


(defun ein2:output-area-convert-mime-types (json data)
  (loop for (prop . mime) in '((:text       . :text/plain)
                               (:html       . :text/html)
                               (:svg        . :image/svg+xml)
                               (:png        . :image/png)
                               (:jpeg       . :image/jpeg)
                               (:latex      . :text/latex)
                               (:json       . :application/json)
                               (:javascript . :application/javascript)
                               (:emacs-lisp . :application/emacs-lisp))
        when (plist-member data mime)
        do (plist-put json prop (plist-get data mime)))
  json)


(defmethod ein2:cell--handle-clear-output ((cell ein2:codecell) content
                                          -metadata-not-used-)
  (ein2:cell-clear-output cell
                         (plist-get content :stdout)
                         (plist-get content :stderr)
                         (plist-get content :other))
  (ein2:events-trigger (oref cell :events) 'maybe_reset_undo.Worksheet cell))


;;; Misc.

(defmethod ein2:cell-has-image-ouput-p ((cell ein2:codecell))
  "Return `t' if given cell has image output, `nil' otherwise."
  (loop for out in (oref cell :outputs)
        when (or (plist-member out :svg)
                 (plist-member out :image/svg)
                 (plist-member out :png)
                 (plist-member out :image/png)
                 (plist-member out :jpeg)
                 (plist-member out :image/jpeg))
        return t))

(defmethod ein2:cell-has-image-ouput-p ((cell ein2:textcell))
  nil)

(defmethod ein2:cell-get-tb-data ((cell ein2:codecell))
  (loop for out in (oref cell :outputs)
        when (equal (plist-get out :output_type) "pyerr")
        return (plist-get out :traceback)))

(provide 'ein-cell)

;;; ein-cell.el ends here
