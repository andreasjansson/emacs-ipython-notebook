;;; ein-worksheet.el --- Worksheet module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-worksheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-worksheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)

(require 'ein-core)
(require 'ein-cell)
(require 'ein-kill-ring)


;;; Configuration

(define-obsolete-variable-alias
  'ein2:notebook-enable-undo 'ein2:worksheet-enable-undo "0.2.0")

(defcustom ein2:worksheet-enable-undo 'yes
  "Configure undo in notebook buffers.

`no' : symbol
    Do not use undo in notebook buffers.  It is the safest option.
`yes' : symbol
    Enable undo in notebook buffers.  You can't undo after
    modification of cell (execution, add, remove, etc.).  This
    is default.
`full' : symbol
    Enable full undo in notebook buffers.  It is powerful but
    sometime (typically after the cell specific commands) undo
    mess up notebook buffer.  Use it on your own risk.  When the
    buffer is messed up, you can just redo and continue editing,
    or save it once and reopen it if you want to be careful.

You need to reopen the notebook buffer to reflect the change of
this value."
  :type '(choice (const :tag "No" no)
                 (const :tag "Yes" yes)
                 (const :tag "Full" full))
  :group 'ein)


;;; Configuration getter

(defun ein2:worksheet-empty-undo-maybe ()
  "Empty `buffer-undo-list' if `ein2:worksheet-enable-undo' is `yes'."
  (when (eq ein2:worksheet-enable-undo 'yes)
    (setq buffer-undo-list nil)))


;;; Class and variable

(defvar ein2:worksheet-buffer-name-template "*ein2: %s/%s*")

(defclass ein2:worksheet ()
  ((nbformat :initarg :nbformat :type integer)
   (get-notebook-name :initarg :get-notebook-name :type cons)
   ;; This slot introduces too much complexity so therefore must be
   ;; removed later.  This is here only for backward compatible
   ;; reason.
   (discard-output-p :initarg :discard-output-p)
   (saved-cells :initarg :saved-cells :initform nil
                :documentation
                "Slot to cache cells for worksheet without buffer")
   (dont-save-cells :initarg :dont-save-cells :initform nil :type boolean
                    :documentation "Don't cache cells when this flag is on.")
   (ewoc :initarg :ewoc :type ewoc)
   (kernel :initarg :kernel :type ein2:$kernel)
   (dirty :initarg :dirty :type boolean :initform nil)
   (metadata :initarg :metadata :initform nil)
   (events :initarg :events)))

(ein2:deflocal ein2:%worksheet% nil
  "Buffer local variable to store an instance of `ein2:worksheet'.")


;;; Initialization of object and buffer

(defun ein2:worksheet-new (nbformat get-notebook-name discard-output-p
                                   kernel events &rest args)
  (apply #'make-instance 'ein2:worksheet
         :nbformat nbformat :get-notebook-name get-notebook-name
         :discard-output-p discard-output-p :kernel kernel :events events
         args))

(defmethod ein2:worksheet-bind-events ((ws ein2:worksheet))
  (with-slots (events) ws
    ;; Bind events for sub components:
    (mapc (lambda (cell) (oset cell :events events))
          (ein2:worksheet-get-cells ws))))

(defun ein2:worksheet-class-bind-events (events)
  "Binds event handlers which are not needed to be bound per instance."
  (ein2:events-on events
                 'maybe_reset_undo.Worksheet
                 (lambda (-ignore- cell)
                   (ein2:with-live-buffer (ein2:cell-buffer cell)
                     (ein2:worksheet-empty-undo-maybe))))
  (ein2:events-on events 'set_next_input.Worksheet
                 #'ein2:worksheet--set-next-input)
  (ein2:events-on events 'set_dirty.Worksheet #'ein2:worksheet--set-dirty))

(defun ein2:worksheet--set-next-input (-ignore- data)
  (destructuring-bind (&key cell text) data
    (ein2:with-live-buffer (ein2:cell-buffer cell)
      (ein2:and-let* ((ws ein2:%worksheet%)
                     (new-cell
                      (ein2:worksheet-insert-cell-below ws 'code cell)))
        (ein2:cell-set-text new-cell text)
        (oset ws :dirty t)))))

(defun ein2:worksheet--set-dirty (-ignore- data)
  "Set dirty flag of worksheet in which CELL in DATA locates."
  (destructuring-bind (&key value cell) data
    (ein2:with-live-buffer (ein2:cell-buffer cell)
      (ein2:worksheet-set-modified-p ein2:%worksheet% value))))

(defmethod ein2:worksheet-notebook-name ((ws ein2:worksheet))
  (ein2:funcall-packed (oref ws :get-notebook-name)))

(defmethod ein2:worksheet-url-or-port ((ws ein2:worksheet))
  (ein2:kernel-url-or-port (oref ws :kernel)))

(defmethod ein2:worksheet-name ((ws ein2:worksheet))
  (plist-get (oref ws :metadata) :name))

(defmethod ein2:worksheet-set-name ((ws ein2:worksheet) name)
  "Set worksheet name.

\(fn ws name)"
  (assert (stringp name) nil "NAME must be a string.  Got: %S" name)
  (oset ws :metadata (plist-put (oref ws :metadata) :name name)))

(defmethod ein2:worksheet-full-name ((ws ein2:worksheet))
  (let ((nb-name (ein2:worksheet-notebook-name ws)))
    (ein2:aif (ein2:worksheet-name ws)
        (concat nb-name "/" it)
      nb-name)))

(defmethod ein2:worksheet-buffer ((ws ein2:worksheet))
  (ein2:and-let* (((slot-boundp ws :ewoc))
                 (ewoc (oref ws :ewoc))
                 (buffer (ewoc-buffer ewoc))
                 ((buffer-live-p buffer)))
    buffer))

(defmethod ein2:worksheet--buffer-name ((ws ein2:worksheet))
  (format ein2:worksheet-buffer-name-template
          (ein2:worksheet-url-or-port ws)
          (ein2:worksheet-full-name ws)))

(defmethod ein2:worksheet--get-buffer ((ws ein2:worksheet))
  (or (ein2:worksheet-buffer ws)
      (generate-new-buffer (ein2:worksheet--buffer-name ws))))

(defmethod ein2:worksheet-set-buffer-name ((ws ein2:worksheet))
  (ein2:with-live-buffer (ein2:worksheet-buffer ws)
    (rename-buffer (ein2:worksheet--buffer-name ws) t)))

(defmethod ein2:worksheet-set-modified-p ((ws ein2:worksheet) dirty)
  (ein2:with-live-buffer (ein2:worksheet-buffer ws)
    (set-buffer-modified-p dirty))
  (oset ws :dirty dirty))

(defmethod ein2:worksheet-render ((ws ein2:worksheet))
  (with-current-buffer (ein2:worksheet--get-buffer ws)
    (setq ein2:%worksheet% ws)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ein2:ewoc-create 'ein2:worksheet-pp
                                   (ein2:propertize-read-only "\n")
                                   nil t))
            (cells (oref ws :saved-cells)))
        (oset ws :ewoc ewoc)
        (if cells
            (mapc (lambda (c)
                    (oset c :ewoc ewoc)
                    (ein2:cell-enter-last c))
                  cells)
          (ein2:worksheet-insert-cell-below ws 'code nil t))))
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)  ; clear undo history
    (when (eq ein2:worksheet-enable-undo 'no)
      (setq buffer-undo-list t))
    (ein2:worksheet-bind-events ws)
    (ein2:worksheet-set-kernel ws)
    (ein2:log 'info "Worksheet %s is ready" (ein2:worksheet-full-name ws))))

(defun ein2:worksheet-pp (ewoc-data)
  (let ((path (ein2:$node-path ewoc-data))
        (data (ein2:$node-data ewoc-data)))
    (case (car path)
      (cell (ein2:cell-pp (cdr path) data)))))


;;; Persistance and loading

(defmethod ein2:worksheet-from-json ((ws ein2:worksheet) data)
  (destructuring-bind (&key cells metadata &allow-other-keys) data
    (oset ws :metadata metadata)
    (oset ws :saved-cells
          (mapcar (lambda (data) (ein2:cell-from-json data)) cells)))
  ws)

(defmethod ein2:worksheet-from-cells ((ws ein2:worksheet) cells)
  )

(defmethod ein2:worksheet-to-json ((ws ein2:worksheet))
  "Convert worksheet WS into JSON ready alist.
It sets buffer internally so that caller doesn not have to set
current buffer."
  (let* ((discard-output-p (oref ws :discard-output-p))
         (cells (ein2:with-possibly-killed-buffer (ein2:worksheet-buffer ws)
                  (mapcar (lambda (c)
                            (ein2:cell-to-json
                             c (ein2:funcall-packed discard-output-p c)))
                          (ein2:worksheet-get-cells ws)))))
    `((cells . ,(apply #'vector cells))
      ,@(ein2:aand (oref ws :metadata) `((metadata . ,it))))))

(defmethod ein2:worksheet-to-nb4-json ((ws ein2:worksheet))
  (let* ((discard-output-p (oref ws :discard-output-p))
         (cells (ein2:with-possibly-killed-buffer (ein2:worksheet-buffer ws)
                  (mapcar (lambda (c)
                            (ein2:cell-to-nb4-json
                             c (ein2:funcall-packed discard-output-p c)))
                          (ein2:worksheet-get-cells ws)))))
    cells))

(defmethod ein2:worksheet-save-cells ((ws ein2:worksheet) &optional deactivate)
  "Save cells in worksheet buffer in cache before killing the buffer.

.. warning:: After called with non-nil DEACTIVATE flag is given,
   cells in worksheet cannot be used anymore.  Use only just
   before killing the buffer.

You don't need to set current buffer to call this function.
Do nothing when the worksheet WS has no buffer.

If the `:dont-save-cells' slot is non-nil (meaning that
`ein2:worksheet-dont-save-cells' has been called), cells in the
worksheet buffer are not saved.  When the DEACTIVATE option is
given, cached cells are deactivated instead of the cells in
buffer.  Calling this function unconditionally resets
`:dont-save-cells' flag to nil to make caching work when the
worksheet WS is reopened.

\(fn ws deactivate)"
  (when (ein2:worksheet-has-buffer-p ws)
    (unless (oref ws :dont-save-cells)
      (let ((cells (ein2:worksheet-get-cells ws)))
        (with-current-buffer (ein2:worksheet-buffer ws)
          (mapc #'ein2:cell-save-text cells))
        (when deactivate (mapc #'ein2:cell-deactivate cells))
        (oset ws :saved-cells cells)))
    (when deactivate
      (mapc #'ein2:cell-deactivate (oref ws :saved-cells))))
  (oset ws :dont-save-cells nil))

(defmethod ein2:worksheet-dont-save-cells ((ws ein2:worksheet))
  "Turn on `:dont-save-cells' flag so that next call on
`ein2:worksheet-save-cells' actually do nothing.

\(fn ws)"
  (oset ws :dont-save-cells t))


;;; Cell indexing, retrieval, etc.

(defmethod ein2:worksheet-cell-from-type ((ws ein2:worksheet) type &rest args)
  "Create a cell of TYPE (symbol or string)."
  ;; FIXME: unify type of TYPE to symbol or string.
  (apply #'ein2:cell-from-type
         (format "%s" type)
         :ewoc (oref ws :ewoc)
         :events (oref ws :events)
         args))

(defmethod ein2:worksheet-get-cells ((ws ein2:worksheet))
  (if (ein2:worksheet-has-buffer-p ws)
      (let* ((ewoc (oref ws :ewoc))
             (nodes (ewoc-collect ewoc
                                  (lambda (n) (ein2:cell-node-p n 'prompt)))))
        (mapcar #'ein2:$node-data nodes))
    (oref ws :saved-cells)))

(defmethod ein2:worksheet-ncells ((ws ein2:worksheet))
  (length (ein2:worksheet-get-cells ws)))

(defun ein2:worksheet-get-ewoc (&optional ws)
  (ein2:aand (or ws ein2:%worksheet%) (oref it :ewoc)))

(defun ein2:worksheet-get-current-ewoc-node (&optional pos)
  (ein2:aand (ein2:worksheet-get-ewoc) (ewoc-locate it pos)))

(defun ein2:worksheet-get-nearest-cell-ewoc-node (&optional pos max cell-p)
  (ein2:and-let* ((ewoc-node (ein2:worksheet-get-current-ewoc-node pos)))
    ;; FIXME: can be optimized using the argument `max'
    (while (and ewoc-node
                (not (and (ein2:cell-ewoc-node-p ewoc-node)
                          (if cell-p
                              (funcall cell-p
                                       (ein2:cell-from-ewoc-node ewoc-node))
                            t))))
      (setq ewoc-node (ewoc-next (oref ein2:%worksheet% :ewoc) ewoc-node)))
    ewoc-node))

(defun* ein2:worksheet-get-current-cell (&key pos noerror
                                             (cell-p #'ein2:basecell-child-p))
  "Return a cell at POS.  If POS is not given, it is assumed be the
current cursor position.  When the current buffer is not worksheet
buffer or there is no cell in the current buffer, return `nil'."
  (let ((cell (ein2:cell-from-ewoc-node
               (ein2:worksheet-get-current-ewoc-node pos))))
    (if (funcall cell-p cell)
        cell
      (unless noerror
        (error "No cell found at pos=%s" pos)))))

(defun ein2:worksheet-at-codecell-p ()
  (ein2:worksheet-get-current-cell :noerror t :cell-p #'ein2:codecell-p))

(defun ein2:worksheet-get-cells-in-region (beg end)
  (ein2:clip-list (ein2:aand ein2:%worksheet% (ein2:worksheet-get-cells it))
                 (ein2:worksheet-get-current-cell :pos beg)
                 (ein2:worksheet-get-current-cell :pos end)))

(defun* ein2:worksheet-get-cells-in-region-or-at-point
    (&key noerror (cell-p #'ein2:basecell-child-p))
  (or (ein2:filter cell-p
                  (if (region-active-p)
                      (ein2:worksheet-get-cells-in-region (region-beginning)
                                                         (region-end))
                    (list (ein2:worksheet-get-current-cell))))
      (unless noerror
        (error "Cell not found"))))


;;; Insertion and deletion of cells

(defun ein2:worksheet--get-ws-or-error ()
  (or ein2:%worksheet% (error "Not in worksheet buffer.")))

(defun ein2:worksheet-focus-cell ()
  (ein2:aand (ein2:worksheet-get-current-cell :noerror t) (ein2:cell-goto it)))

(defun ein2:worksheet-delete-cell (ws cell &optional focus)
  "Delete a cell.  \(WARNING: no undo!)
This command has no key binding because there is no way to undo
deletion.  Use kill to play on the safe side.

If you really want use this command, you can do something like this
\(but be careful when using it!)::

  \(define-key ein2:notebook-mode-map \"\\C-c\\C-d\"
              'ein2:worksheet-delete-cell)"
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     t))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))        ; disable undo recording
    (apply #'ewoc-delete
           (oref ws :ewoc)
           (ein2:cell-all-element cell)))
  (oset ws :dirty t)
  (ein2:worksheet-empty-undo-maybe)
  (when focus (ein2:worksheet-focus-cell)))

(defun ein2:worksheet-kill-cell (ws cells &optional focus)
  "Kill (\"cut\") the cell at point or cells in region.
Note that the kill-ring for cells is not shared with the default
kill-ring of Emacs (kill-ring for texts)."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-cells-in-region-or-at-point)
                     t))
  (when cells
    (mapc (lambda (c)
            (ein2:cell-save-text c)
            (ein2:worksheet-delete-cell ws c)
            (ein2:cell-deactivate c))
          cells)
    (ein2:kill-new cells)
    (when focus
      (deactivate-mark)
      (ein2:worksheet-focus-cell))))

(defun ein2:worksheet-copy-cell (cells)
  "Copy the cell at point.  (Put the current cell into the kill-ring.)"
  (interactive
   (list (when (ein2:worksheet--get-ws-or-error)
           (prog1 (ein2:worksheet-get-cells-in-region-or-at-point)
             (deactivate-mark)))))
  (let ((cells (mapcar
                (lambda (c)
                  (ein2:cell-deactivate (ein2:cell-copy c))) cells)))
    (ein2:log 'info "%s cells are copied." (length  cells))
    (ein2:kill-new cells)))

(defun ein2:worksheet-insert-clone-below (ws cell pivot)
  (let ((clone (ein2:cell-copy cell)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (oset clone :ewoc (oref ws :ewoc))
    (ein2:worksheet-insert-cell-below ws clone pivot)
    clone))

(defun ein2:worksheet-yank-cell (ws &optional n)
  "Insert (\"paste\") the latest killed cell.
Prefixes are act same as the normal `yank' command."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (let ((arg current-prefix-arg))
                       (cond ((listp arg) 0)
                             ((eq arg '-) -2)
                             (t (1- arg))))))
  (let* ((cell (ein2:worksheet-get-current-cell :noerror t)) ; can be nil
         (killed (ein2:current-kill n)))
    (loop for c in killed
          with last = cell
          do (setq last (ein2:worksheet-insert-clone-below ws c last))
          finally (ein2:cell-goto last))))

(defun ein2:worksheet-maybe-new-cell (ws type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (let ((cell (if (ein2:basecell-child-p type-or-cell)
                  type-or-cell
                (ein2:worksheet-cell-from-type ws type-or-cell))))
    ;; When newly created or copied, kernel is not attached or not the
    ;; kernel of this worksheet.  So reset it here.
    (when (ein2:codecell-p cell)
      (oset cell :kernel (oref ws :kernel)))
    (oset cell :events (oref ws :events))
    cell))

(defun ein2:worksheet-insert-cell-below (ws type-or-cell pivot &optional focus)
  "Insert cell below.  Insert markdown cell instead of code cell
when the prefix argument is given.

When used as a lisp function, insert a cell of TYPE-OR-CELL just
after PIVOT and return the new cell."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ein2:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ein2:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((= (ein2:worksheet-ncells ws) 0)
      (ein2:cell-enter-last cell))
     (pivot
      (ein2:cell-insert-below pivot cell))
     (t (error
         "PIVOT is `nil' but ncells != 0.  There is something wrong...")))
    (ein2:worksheet-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ein2:cell-goto cell))
    cell))

(defun ein2:worksheet-insert-cell-above (ws type-or-cell pivot &optional focus)
  "Insert cell above.  Insert markdown cell instead of code cell
when the prefix argument is given.
See also: `ein2:worksheet-insert-cell-below'."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ein2:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ein2:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((< (ein2:worksheet-ncells ws) 2)
      (ein2:cell-enter-first cell))
     (pivot
      (let ((prev-cell (ein2:cell-prev pivot)))
        (if prev-cell
            (ein2:cell-insert-below prev-cell cell)
          (ein2:cell-enter-first cell))))
     (t (error
         "PIVOT is `nil' but ncells > 0.  There is something wrong...")))
    (ein2:worksheet-empty-undo-maybe)
    (oset ws :dirty t)
    (when focus (ein2:cell-goto cell))
    cell))

(defun ein2:worksheet-toggle-cell-type (ws cell &optional focus)
  "Toggle the cell type of the cell at point.
Use `ein2:worksheet-change-cell-type' to change the cell type
directly."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     t))
  (let ((type (case (oref ws :nbformat)
                (2 (ein2:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "code")))
                (3 (ein2:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "heading")
                     (("heading") "code")))
                (4 (ein2:case-equal (oref cell :cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "code"))))))
    (let ((relpos (ein2:cell-relative-point cell))
          (new (ein2:cell-convert-inplace cell type)))
      (when (ein2:codecell-p new)
        (oset new :kernel (oref ws :kernel)))
      (ein2:worksheet-empty-undo-maybe)
      (when focus (ein2:cell-goto new relpos)))))

(defun ein2:worksheet-change-cell-type (ws cell type &optional level focus)
  "Change the cell type of the current cell.
Prompt will appear in the minibuffer.

When used in as a Lisp function, TYPE (string) should be chose
from \"code\", \"markdown\", \"raw\" and \"heading\".  LEVEL is
an integer used only when the TYPE is \"heading\"."
  (interactive
   (let* ((ws (ein2:worksheet--get-ws-or-error))
          (cell (ein2:worksheet-get-current-cell))
          (choices (case (oref ws :nbformat)
                     (2 "cm")
                     (3 "cmr123456")
                     (4 "cmr")))
          (key (ein2:ask-choice-char
                (format "Cell type [%s]: " choices) choices))
          (type (case key
                  (?c "code")
                  (?m "markdown")
                  (?r "raw")
                  (t "heading")))
          (level (when (equal type "heading")
                   (string-to-number (char-to-string key)))))
     (list ws cell type level t)))

  (let ((relpos (ein2:cell-relative-point cell))
        (new (ein2:cell-convert-inplace cell type)))
    (when (ein2:codecell-p new)
      (oset new :kernel (oref ws :kernel)))
    (when level
      (ein2:cell-change-level new level))
    (ein2:worksheet-empty-undo-maybe)
    (when focus (ein2:cell-goto new relpos))))

(defun ein2:worksheet-split-cell-at-point (ws cell &optional no-trim focus)
  "Split cell at current position. Newlines at the splitting
point will be removed. This can be omitted by giving a prefix
argument \(C-u)."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  ;; FIXME: should I inhibit undo?
  (let* ((beg (set-marker (make-marker) (ein2:cell-input-pos-min cell)))
         (pos (point-marker))
         (head (buffer-substring beg pos))
         (new (ein2:worksheet-insert-cell-above ws
                                               (oref cell :cell-type)
                                               cell)))
    (when (ein2:headingcell-p cell)
      (ein2:cell-change-level new (oref cell :level)))
    (delete-region beg pos)
    (unless no-trim
      (setq head (ein2:trim-right head "\n"))
      (save-excursion
        (goto-char pos)
        (let ((end (set-marker (make-marker) (ein2:cell-input-pos-max cell))))
          (while (and (looking-at-p "\n") (< (point) end))
            (delete-char 1)))))
    (ein2:cell-set-text new head)
    (ein2:worksheet-empty-undo-maybe)
    (when focus (ein2:cell-goto cell))))

(defun ein2:worksheet-merge-cell (ws cell &optional next focus)
  "Merge previous cell into current cell.
If prefix is given, merge current cell into next cell."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  (unless next
    (setq cell (ein2:cell-prev cell))
    (unless cell (error "No previous cell"))
    (ein2:cell-goto cell))
  (let* ((next-cell (ein2:cell-next cell))
         (head (ein2:cell-get-text cell)))
    (assert next-cell nil "No cell to merge.")
    (ein2:worksheet-delete-cell ws cell)
    (save-excursion
      (goto-char (ein2:cell-input-pos-min next-cell))
      (insert head "\n"))
    (ein2:worksheet-empty-undo-maybe)
    (when focus (ein2:cell-goto next-cell))))


;;; Cell selection.

(defun* ein2:worksheet-next-input-cell (ewoc-node &optional up (nth 1))
  "Return a cell containing the next input node after EWOC-NODE.
When UP is non-`nil', do the same for the *previous* input node.
When NTH is specified, return NTH cell.  Note that this function is
*not* defined for NTH=0; it returns nil."
  (unless (= nth 0)
    (when (< nth 0)
      (setq nth (* nth -1))
      (setq up (not up)))
    (let ((cell (ein2:worksheet-next-input-cell-1 ewoc-node up)))
      (loop repeat (1- nth)
            with next = (if up #'ein2:cell-prev #'ein2:cell-next)
            if (funcall next cell)
            do (setq cell it)
            else
            return nil)
      cell)))

(defun ein2:worksheet-next-input-cell-1 (ewoc-node &optional up)
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein2:$node-data ewoc-data))
         (path (ein2:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element (if up '(output footer) '(prompt)))
        cell
      (funcall (if up #'ein2:cell-prev #'ein2:cell-next) cell))))

(defun ein2:worksheet-goto-input (ewoc-node up)
  (ein2:aif (ein2:worksheet-next-input-cell ewoc-node up)
      (ein2:cell-goto it)
    (error "No %s input!" (if up "previous" "next"))))

(defun ein2:worksheet-goto-next-input (ewoc-node)
  (interactive (list (and (ein2:worksheet--get-ws-or-error)
                          (ein2:worksheet-get-current-ewoc-node))))
  (ein2:worksheet-goto-input ewoc-node nil))

(defun ein2:worksheet-goto-prev-input (ewoc-node)
  (interactive (list (and (ein2:worksheet--get-ws-or-error)
                          (ein2:worksheet-get-current-ewoc-node))))
  (ein2:worksheet-goto-input ewoc-node t))

(defun ein2:worksheet-goto-next-cell-element (&optional nth up relpos prop)
  "Go to NTH next cell element named PROP and shift cursor by RELPOS.
Go to previous cell if UP is t.
Return t when the movement is succeeded."
  (unless prop (setq prop :input))
  (ein2:and-let* ((current-node (ein2:worksheet-get-current-ewoc-node))
                 (current-cell (ein2:cell-from-ewoc-node current-node))
                 (target-cell
                  (if (and (= nth 1)
                           (eq (ein2:cell-element-get current-cell :input)
                               current-node)
                           (not (and up
                                     (= (1+ (ewoc-location current-node))
                                        (point)))))
                      current-cell
                    (ein2:worksheet-next-input-cell current-node up nth))))
    (ein2:cell-goto target-cell relpos prop)
    t))

(defun ein2:worksheet-beginning-of-cell-input (&optional arg)
  "Move backward to the beginning of a cell.
This function is for `beginning-of-defun-function', so behaves
similarly with `beginning-of-defun'.
It is set in `ein2:notebook-multilang-mode'."
  (ein2:worksheet-goto-next-cell-element (or arg 1) t))

(defun ein2:worksheet-end-of-cell-input (&optional arg)
  "Move forward to the end of a cell.
This function is for `end-of-defun-function', so behaves
similarly with `end-of-defun'.
It is set in `ein2:notebook-multilang-mode'."
  (ein2:worksheet-goto-next-cell-element (or arg 1) nil 0 :after-input))


;;; Cell movement

(defun ein2:worksheet-move-cell (ws cell up)
  (ein2:aif (if up (ein2:cell-prev cell) (ein2:cell-next cell))
      (let ((inhibit-read-only t)
            (pivot-cell it))
        (ein2:cell-save-text cell)
        (ein2:worksheet-delete-cell ws cell)
        (funcall (if up
                     #'ein2:worksheet-insert-cell-above
                   #'ein2:worksheet-insert-cell-below)
                 ws cell pivot-cell)
        (ein2:cell-goto cell)
        (oset ws :dirty t))
    (error "No %s cell" (if up "previous" "next"))))

(defun ein2:worksheet-move-cell-up (ws cell)
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)))
  (ein2:worksheet-move-cell ws cell t))

(defun ein2:worksheet-move-cell-down (ws cell)
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)))
  (ein2:worksheet-move-cell ws cell nil))


;;; Cell collapsing and output clearing

(defun ein2:worksheet-toggle-output (ws cell)
  "Toggle the visibility of the output of the cell at point.
This does not alter the actual data stored in the cell."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell
                      :cell-p #'ein2:codecell-p)))
  (ein2:cell-toggle-output cell)
  (ein2:worksheet-empty-undo-maybe)
  (oset ws :dirty t))

(defun ein2:worksheet-set-output-visibility-all (ws &optional collapsed)
  "Show all cell output.  When prefix is given, hide all cell output."
  (interactive (list (ein2:worksheet--get-ws-or-error) current-prefix-arg))
  (when collapsed (setq collapsed t))   ; force it to be a boolean
  (mapc (lambda (c)
          (when (ein2:codecell-p c) (ein2:cell-set-collapsed c collapsed)))
        (ein2:worksheet-get-cells ws))
  (ein2:worksheet-empty-undo-maybe)
  (oset ws :dirty t))

(defun ein2:worksheet-clear-output (cell &optional preserve-input-prompt)
  "Clear output from the current cell at point.
Do not clear input prompt when the prefix argument is given."
  (interactive (list (ein2:worksheet-get-current-cell
                      :cell-p #'ein2:codecell-p)
                     current-prefix-arg))
  (ein2:cell-clear-output cell t t t)
  (unless preserve-input-prompt
    (ein2:cell-set-input-prompt cell))
  (ein2:worksheet-empty-undo-maybe))

(defun ein2:worksheet-clear-all-output (ws &optional preserve-input-prompt)
  "Clear output from all cells.
Do not clear input prompts when the prefix argument is given."
  (interactive (list (ein2:worksheet--get-ws-or-error) current-prefix-arg))
  (mapc (lambda (c) (ein2:worksheet-clear-output c preserve-input-prompt))
        (ein2:filter #'ein2:codecell-p (ein2:worksheet-get-cells ws))))


;;; Kernel related things

(defmethod ein2:worksheet-set-kernel ((ws ein2:worksheet))
  (mapc (lambda (cell) (oset cell :kernel (oref ws :kernel)))
        (ein2:filter #'ein2:codecell-p (ein2:worksheet-get-cells ws))))

(defun ein2:worksheet-execute-cell (ws cell)
  "Execute code type CELL."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell
                      :cell-p #'ein2:codecell-p)))
  (ein2:kernel-if-ready (oref ws :kernel)
    (ein2:cell-execute cell)
    (oset ws :dirty t)
    cell))

(defun ein2:worksheet-execute-cell-and-goto-next (ws cell &optional insert)
  "Execute cell at point if it is a code cell and move to the
next cell, or insert if none."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)))
  (when (ein2:codecell-p cell)
    (ein2:worksheet-execute-cell ws cell))
  (ein2:aif (and (not insert) (ein2:cell-next cell))
      (ein2:cell-goto it)
    (ein2:worksheet-insert-cell-below ws 'code cell t)))

(defun ein2:worksheet-execute-cell-and-insert-below (ws cell)
  "Execute cell at point if it is a code cell and insert a
cell bellow."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)))
  (ein2:worksheet-execute-cell-and-goto-next ws cell t))

(defun ein2:worksheet-execute-all-cell (ws)
  "Execute all cells in the current worksheet buffer."
  (interactive (list (ein2:worksheet--get-ws-or-error)))
  (mapc #'ein2:cell-execute
        (ein2:filter #'ein2:codecell-p (ein2:worksheet-get-cells ws))))

(defun ein2:worksheet-insert-last-input-history (ws cell index)
  "Insert INDEX-th previous history into CELL in worksheet WS."
  (ein2:kernel-history-request
   (oref ws :kernel)
   (list
    :history_reply
    (cons
     (lambda (cell content -metadata-not-used-)
       (destructuring-bind (session line-number input)
           (car (plist-get content :history))
         (if (eq (ein2:worksheet-get-current-cell) cell)
             (ein2:cell-set-text cell input)
           (ein2:log 'warning
             "Cursor moved from the cell after history request."))
         (ein2:log 'info "Input history inserted: session:%d line:%d"
                  session line-number)))
     cell))
   :hist-access-type "range"
   :session 0
   :start (- index)
   :stop (- 1 index)))

(defvar ein2:worksheet--history-index 1)

(defun ein2:worksheet--get-history-index (inc)
  "Increment history index by (possibly negative) INC.
Get history index for `ein2:worksheet-previous-input-history' and
`ein2:worksheet-next-input-history'.  Raise error if caller tries
to decrement index to less than or equal to 1."
  (if (or (eq last-command 'ein2:worksheet-previous-input-history)
          (eq last-command 'ein2:worksheet-next-input-history))
      (progn
        (setq ein2:worksheet--history-index
              (+ ein2:worksheet--history-index inc))
        (when (< ein2:worksheet--history-index 1)
          (setq ein2:worksheet--history-index 1)
          (error "This is the latest input"))
        ein2:worksheet--history-index)
    (setq ein2:worksheet--history-index 1)))

(defun ein2:worksheet-previous-input-history (ws cell index)
  "Insert the previous input in the execution history.
You can go back further in the history by repeating this command.
Use `ein2:worksheet-next-input-history' to go forward in the
history."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     (ein2:worksheet--get-history-index +1)))
  (ein2:worksheet-insert-last-input-history ws cell index))

(defun ein2:worksheet-next-input-history (ws cell index)
  "Insert next input in the execution history.
You can go forward further in the history by repeating this
command.  Use `ein2:worksheet-previous-input-history' to go back
in the history."
  (interactive (list (ein2:worksheet--get-ws-or-error)
                     (ein2:worksheet-get-current-cell)
                     (ein2:worksheet--get-history-index -1)))
  (ein2:worksheet-insert-last-input-history ws cell index))


;;; Metadata

(defun ein2:worksheet-rename-sheet (ws name)
  "Change worksheet name (*not* notebook name)."
  (interactive (let ((ws (ein2:worksheet--get-ws-or-error)))
                 (list ws
                       (read-from-minibuffer
                        "New worksheet name: " (ein2:worksheet-name ws)))))
  (unless (equal name (or (ein2:worksheet-name ws) ""))
    (ein2:worksheet-set-name ws name)
    (ein2:worksheet-set-modified-p ws t)
    (ein2:worksheet-set-buffer-name ws)))


;;; Generic getter

(defun ein2:get-url-or-port--worksheet ()
  (when (ein2:worksheet-p ein2:%worksheet%)
    (ein2:worksheet-url-or-port ein2:%worksheet%)))

(defun ein2:get-kernel--worksheet ()
  (when (ein2:worksheet-p ein2:%worksheet%) (oref ein2:%worksheet% :kernel)))

(defun ein2:get-cell-at-point--worksheet ()
  (ein2:worksheet-get-current-cell :noerror t))

(defun ein2:get-traceback-data--worksheet ()
  (ein2:aand (ein2:get-cell-at-point--worksheet) (ein2:cell-get-tb-data it)))


;;; Predicate

(defun ein2:worksheet-buffer-p ()
  "Return non-`nil' if the current buffer is a worksheet buffer."
  ein2:%worksheet%)

(defmethod ein2:worksheet-has-buffer-p ((ws ein2:worksheet))
  (ein2:aand (ein2:worksheet-buffer ws) (buffer-live-p it)))

(defmethod ein2:worksheet-modified-p ((ws ein2:worksheet))
  (let ((buffer (ein2:worksheet-buffer ws)))
    (and (buffer-live-p buffer)
         (or (oref ws :dirty)
             (buffer-modified-p buffer)))))


;;; Utility commands

(defun ein2:worksheet-dedent-cell-text (cell)
  "Dedent text in CELL."
  (interactive (list (ein2:worksheet-get-current-cell)))
  (let* ((beg (ein2:cell-input-pos-min cell))
         (end (ein2:cell-input-pos-max cell)))
    (indent-rigidly
     beg end (- (ein2:find-leftmot-column beg end)))))


;;; Auto-execution

(defun ein2:worksheet-toggle-autoexec (cell)
  "Toggle auto-execution flag of the cell at point."
  (interactive (list (ein2:worksheet-get-current-cell #'ein2:codecell-p)))
  (ein2:cell-toggle-autoexec cell))

(defun ein2:worksheet-turn-on-autoexec (cells &optional off)
  "Turn on auto-execution flag of the cells in region or cell at point.
When the prefix argument is given, turn off the flag instead.

To use autoexec feature, you need to turn on auto-execution mode
in connected buffers, using the `ein2:connect-toggle-autoexec'
command."
  (interactive
   (list (ein2:worksheet-get-cells-in-region-or-at-point
          :cell-p #'ein2:codecell-p)
         current-prefix-arg))
  (mapc (lambda (c) (ein2:cell-set-autoexec c (not off))) cells)
  (ein2:log 'info "Turn %s auto-execution flag of %s cells."
           (if off "off" "on")
           (length cells)))

(defun ein2:worksheet-execute-autoexec-cells (ws)
  "Execute cells of which auto-execution flag is on.
This function internally sets current buffer to the worksheet
buffer, so you don't need to set current buffer to call this
function."
  (interactive (list (ein2:worksheet--get-ws-or-error)))
  (ein2:with-live-buffer (ein2:worksheet-buffer ws)
    (ein2:kernel-if-ready (oref ws :kernel)
      (mapc #'ein2:cell-execute
            (ein2:filter #'ein2:cell-autoexec-p
                        (ein2:worksheet-get-cells ws))))))


;;; Imenu

(defun ein2:worksheet-imenu-create-index ()
  "`imenu-create-index-function' for notebook buffer."
  ;; As Imenu does not provide the way to represent level *and*
  ;; position, use #'s to do that.
  (loop for cell in (when (ein2:worksheet-p ein2:%worksheet%)
                      (ein2:filter #'ein2:headingcell-p
                                  (ein2:worksheet-get-cells ein2:%worksheet%)))
        for sharps = (loop repeat (oref cell :level) collect "#")
        for text = (ein2:cell-get-text cell)
        for name = (ein2:join-str "" (append sharps (list " " text)))
        collect (cons name (ein2:cell-input-pos-min cell))))

(defun ein2:worksheet-imenu-setup ()
  "Called via notebook mode hooks."
  (setq imenu-create-index-function #'ein2:worksheet-imenu-create-index))


;;; Workarounds

(defadvice fill-paragraph (around ein2:worksheet-fill-paragraph activate)
  "Prevent \"Text is read-only\" error when filling paragraph in
EIN worksheet."
  (if ein2:%worksheet%
      (let* ((cell (ein2:worksheet-get-current-cell))
             (beg (copy-marker (ein2:cell-input-pos-min cell))))
        (save-excursion
          (goto-char beg)
          (insert "\n"))
        (unwind-protect
            ad-do-it
          (save-excursion
            (goto-char beg)
            (delete-char 1))))
    ad-do-it))

(provide 'ein-worksheet)

;;; ein-worksheet.el ends here
