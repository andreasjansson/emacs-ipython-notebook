;;; ein-shared-output.el --- Output buffer for ein-connect.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-shared-output.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-shared-output.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-shared-output.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When executing code from outside of notebook, some place for output
;; is needed.  This module buffer containing one special cell for that
;; purpose.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-cell)


;;; Classes and variables

(defclass ein2:shared-output-cell (ein2:codecell)
  ((cell-type :initarg :cell-type :initform "shared-output")
   ;; (element-names :initform (:prompt :output :footer))
   (popup :initarg :popup :initform nil :type boolean)
   )
  "A singleton cell to show output from non-notebook buffers.")

(defclass ein2:shared-output ()
  ((cell :initarg :cell :type ein2:shared-output-cell)
   (events :initarg :events :type ein2:events)
   (ewoc :initarg :ewoc :type ewoc)))

(defvar ein2:%shared-output% nil
  "Hold an instance of `ein2:shared-output'.")

(defconst ein2:shared-output-buffer-name "*ein2:shared-output*")


;;; Cell related

(defmethod ein2:cell-execute ((cell ein2:shared-output-cell) kernel code
                             &optional popup &rest args)
  (unless (plist-get args :silent)
    (setq args (plist-put args :silent nil)))
  (oset cell :popup popup)
  (oset cell :kernel kernel)
  (apply #'ein2:cell-execute-internal cell kernel code args))

(defmethod ein2:cell--handle-output ((cell ein2:shared-output-cell)
                                    msg-type content -metadata-not-used-)
  ;; Show short message
  (ein2:case-equal msg-type
    (("pyout")
     (let ((num (plist-get content :execution_count))
           (text (plist-get (plist-get content :data) :text/plain)))
       (when text
         (ein2:log 'info "Out[%s]: %s" num (car (split-string text "\n"))))))
    (("stream")
     (let ((stream (or (plist-get content :stream) "stdout"))
           (text (plist-get content :data)))
       (when text
         (ein2:log 'info "%s: %s" stream (car (split-string text "\n"))))))
    (t
     (ein2:log 'info "Got output '%s' in the shared buffer." msg-type)))
  ;; Open `ein2:shared-output-buffer-name' if necessary
  (when (oref cell :popup)
    (pop-to-buffer (ein2:shared-output-create-buffer)))
  ;; Finally do the normal drawing
  (call-next-method))


;;; Main

(defun ein2:shared-output-create-buffer ()
  "Get or create the shared output buffer."
  (get-buffer-create ein2:shared-output-buffer-name))

(defun ein2:shared-output-buffer ()
  "Get the buffer associated with `ein2:%shared-output%'."
  (ewoc-buffer (oref ein2:%shared-output% :ewoc)))

(defun ein2:shared-output-buffer-p (&optional buffer)
  "Return non-`nil' when BUFFER (or current buffer) is shared-output buffer."
  (eq (or buffer (current-buffer)) (ein2:shared-output-buffer)))

(defun ein2:shared-output-healthy-p ()
  (and (ein2:shared-output-p ein2:%shared-output%)
       (buffer-live-p (ein2:shared-output-buffer))))

(defun ein2:shared-output-get-or-create ()
  (if (ein2:shared-output-healthy-p)
      ein2:%shared-output%
    (with-current-buffer (ein2:shared-output-create-buffer)
      ;; FIXME: This is a duplication of `ein2:worksheet-render'.
      ;;        Must be merged.
      (let* ((inhibit-read-only t)
             ;; Enable nonsep for ewoc object (the last argument is non-nil).
             ;; This is for putting read-only text properties to the newlines.
             (ewoc (ein2:ewoc-create 'ein2:worksheet-pp
                                    (ein2:propertize-read-only "\n")
                                    nil t))
             (events (ein2:events-new))
             (cell (ein2:shared-output-cell "SharedOutputCell"
                                           :ewoc ewoc
                                           :events events)))
        (erase-buffer)
        (ein2:shared-output-bind-events events)
        (setq ein2:%shared-output%
              (ein2:shared-output "SharedOutput" :ewoc ewoc :cell cell
                                  :events events))
        (ein2:cell-enter-last cell))
      (setq buffer-read-only t)
      (ein2:shared-output-mode)
      ein2:%shared-output%)))

(defun ein2:shared-output-bind-events (events)
  "Add dummy event handlers."
  (ein2:events-on events 'set_dirty.Worksheet #'ignore)
  (ein2:events-on events 'maybe_reset_undo.Worksheet #'ignore))

(defun ein2:shared-output-get-cell ()
  "Get the singleton shared output cell.
Create a cell if the buffer has none."
  (oref (ein2:shared-output-get-or-create) :cell))

(defun ein2:shared-output-get-kernel ()
  (let ((cell (ein2:shared-output-get-cell)))
    (when (slot-boundp cell :kernel)
      (oref cell :kernel))))

;;;###autoload
(defun ein2:shared-output-pop-to-buffer ()
  "Open shared output buffer."
  (interactive)
  (ein2:shared-output-get-or-create)
  (pop-to-buffer (ein2:shared-output-create-buffer)))

(defmethod ein2:shared-output-show-code-cell ((cell ein2:codecell))
  "Show code CELL in shared-output buffer.
Note that this function assumed to be called in the buffer
where CELL locates."
  (let ((new (ein2:cell-convert cell "shared-output")))
    ;; Make sure `ein2:%shared-output%' is initialized:
    (ein2:shared-output-get-or-create)
    (with-current-buffer (ein2:shared-output-create-buffer)
      (let ((inhibit-read-only t)
            (ein2:cell-max-num-outputs nil))
        (oset new :ewoc (oref ein2:%shared-output% :ewoc))
        (oset new :events (oref ein2:%shared-output% :events))
        (erase-buffer)  ; because there are only one cell anyway
        (oset ein2:%shared-output% :cell new)
        (ein2:cell-enter-last new)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun ein2:shared-output-show-code-cell-at-point ()
  "Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein2:cell-max-num-outputs'."
  (interactive)
  (let ((cell (ein2:get-cell-at-point)))
    (if (ein2:codecell-p cell)
        (ein2:shared-output-show-code-cell cell)
      (error "No code cell at point."))))

(defvar ein2:shared-output-eval-string-history nil
  "History of the `ein2:shared-output-eval-string' prompt.")

;;;###autoload
(defun ein2:shared-output-eval-string (code &optional popup verbose kernel
                                           &rest args)
  "Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ein2:shared-output-pop-to-buffer'.

.. ARGS is passed to `ein2:kernel-execute'.  Unlike `ein2:kernel-execute',
   `:silent' is `nil' by default."
  (interactive
   (let ((kernel (ein2:get-kernel-or-error))
         ;; ... so error will be raised before user typing code if it
         ;; is impossible to execute
         (code (read-string
                "IP[y]: "
                (when (region-active-p)
                  (buffer-substring (region-beginning) (region-end)))
                'ein2:shared-output-eval-string-history)))
     (list code nil t kernel)))
  (unless kernel (setq kernel (ein2:get-kernel-or-error)))
  (let ((cell (ein2:shared-output-get-cell)))
    (apply #'ein2:cell-execute cell kernel (ein2:trim-indent code) popup args))
  (when verbose
    (ein2:log 'info "Code \"%s\" is sent to the kernel." code)))


;;; Generic getter

(defun ein2:get-url-or-port--shared-output ()
  (ein2:aand (ein2:get-kernel--shared-output) (ein2:kernel-url-or-port it)))

;; (defun ein2:get-notebook--shared-output ())

(defun ein2:get-kernel--shared-output ()
  (let ((cell (ein2:get-cell-at-point--shared-output)))
    (when (and (object-p cell) (slot-boundp cell :kernel))
      (oref cell :kernel))))

(defun ein2:get-cell-at-point--shared-output ()
  (when (and (ein2:shared-output-p ein2:%shared-output%)
             (ein2:shared-output-buffer-p))
    (oref ein2:%shared-output% :cell)))

(defun ein2:get-traceback-data--shared-output ()
  (ein2:aand (ein2:get-cell-at-point--shared-output) (ein2:cell-get-tb-data it)))


;;; ein2:shared-output-mode

(define-derived-mode ein2:shared-output-mode fundamental-mode "ein2:so"
  "Shared output mode."
  (font-lock-mode))

(let ((map ein2:shared-output-mode-map))
  (define-key map "\C-c\C-x" 'ein2:tb-show)
  (define-key map "\M-."          'ein2:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein2:pytools-jump-to-source-command)
  (define-key map "q" 'bury-buffer))

(add-hook 'ein2:shared-output-mode-hook 'ein2:truncate-lines-on)


(provide 'ein-shared-output)

;;; ein-shared-output.el ends here
