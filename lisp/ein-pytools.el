;;; ein-pytools.el --- Python tools build on top of kernel

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pytools.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pytools.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pytools.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

;; for `ein2:pytools-pandas-to-ses'
(declare-function ses-yank-tsf "ses")
(declare-function ses-command-hook "ses")

(require 'ein-kernel)

(defun ein2:goto-file (filename lineno &optional other-window)
  "Jump to file FILEAME at line LINENO.
If OTHER-WINDOW is non-`nil', open the file in the other window."
  (funcall (if other-window #'find-file-other-window #'find-file) filename)
  (goto-char (point-min))
  (forward-line (1- lineno)))

(defun ein2:goto-marker (marker &optional other-window)
  (funcall (if other-window #'pop-to-buffer #'switch-to-buffer)
           (marker-buffer marker))
  (goto-char marker))

(defcustom ein2:propagate-connect t
  "Set to `t' to connect to the notebook after jumping to a buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

(defun ein2:pytools-setup-hooks (kernel)
  (push (cons #'ein2:pytools-add-sys-path kernel)
        (ein2:$kernel-after-start-hook kernel)))

(defun ein2:pytools-add-sys-path (kernel)
  (ein2:kernel-execute
   kernel
   (format "__import__('sys').path.append('%s')" ein2:source-dir)))


;;; Tooltip and help

(defun ein2:pytools-request-tooltip (kernel func)
  (interactive (list (ein2:get-kernel-or-error)
                     (ein2:object-at-point-or-error)))
  (ein2:kernel-object-info-request
   kernel func (list :object_info_reply
                     (cons #'ein2:pytools-finish-tooltip nil))))

(declare-function pos-tip-show "pos-tip")
(declare-function popup-tip "popup")

(defun ein2:pytools-finish-tooltip (-ignore- content -metadata-not-used-)
  ;; See: Tooltip.prototype._show (tooltip.js)
  (let ((tooltip (ein2:kernel-construct-help-string content))
        (defstring (ein2:kernel-construct-defstring content))
        (name (plist-get content :name)))
    (if tooltip
        (cond
         ((and window-system (featurep 'pos-tip))
          (pos-tip-show tooltip 'ein2:pos-tip-face nil nil 0))
         ((featurep 'popup)
          (popup-tip tooltip))
         (t (when (stringp defstring)
              (message (ein2:trim (ansi-color-apply defstring))))))
      (ein2:log 'info "no info for %s" name))))

(defun ein2:pytools-request-help (kernel func)
  (interactive (list (ein2:get-kernel-or-error)
                     (ein2:object-at-point-or-error)))
  (ein2:kernel-execute kernel
                      (format "%s?" func) ; = code
                      nil                 ; = callbacks
                      ;; It looks like that magic command does
                      ;; not work in silent mode.
                      :silent nil))

(defun ein2:pytools-request-tooltip-or-help (&optional pager)
  "Show the help for the object at point using tooltip.
When the prefix argument ``C-u`` is given, open the help in the
pager buffer.  You can explicitly specify the object by selecting it."
  (interactive "P")
  (call-interactively (if pager
                          #'ein2:pytools-request-help
                        #'ein2:pytools-request-tooltip)))


;;; Source jump

(defvar ein2:pytools-jump-stack nil)

(defvar ein2:pytools-jump-to-source-not-found-regexp
  (ein2:join-str "\\|"
                (list "^WARNING: .*"
                      "^Traceback (most recent call last):\n"
                      "^.*<ipython-input-[^>\n]+>\n"
                      "^\n")))

(defun ein2:pytools-jump-to-source (kernel object &optional
                                          other-window notebook)
  (ein2:log 'info "Jumping to the source of %s..." object)
  (let ((last (car ein2:pytools-jump-stack)))
    (if (ein2:aand last (eql (current-buffer) (marker-buffer it)))
        (unless (equal (point) (marker-position last))
          (push (point-marker) ein2:pytools-jump-stack))
      (setq ein2:pytools-jump-stack (list (point-marker)))))
  (ein2:kernel-execute
   kernel
   (format "__import__('ein').find_source('%s')" object)
   (list
    :output
    (cons
     (lambda (packed msg-type content -metadata-not-used-)
       (destructuring-bind (kernel object other-window notebook)
           packed
         (ein2:case-equal msg-type
           (("stream")
            (ein2:aif (plist-get content :data)
                (if (string-match ein2:pytools-jump-to-source-not-found-regexp
                                  it)
                    (ein2:log 'info
                      "Jumping to the source of %s...Not found" object)
                  (destructuring-bind (filename &optional lineno &rest ignore)
                      (split-string it "\n")
                    (setq lineno (string-to-number lineno))
                    (setq filename
                          (ein2:kernel-filename-from-python kernel filename))
                    (let ((ein2:connect-default-notebook nil))
                      ;; Avoid auto connection to connect to the
                      ;; NOTEBOOK instead of the default one.
                      (ein2:goto-file filename lineno other-window))
                    ;; Connect current buffer to NOTEBOOK. No reconnection.
                    (ein2:connect-buffer-to-notebook notebook nil t)
                    (push (point-marker) ein2:pytools-jump-stack)
                    (ein2:log 'info
                      "Jumping to the source of %s...Done" object)))))
           (("pyerr")
            (ein2:log 'info
              "Jumping to the source of %s...Not found" object)))))
     (list kernel object other-window notebook)))))

(defun ein2:pytools-jump-to-source-command (&optional other-window)
  "Jump to the source code of the object at point.
When the prefix argument ``C-u`` is given, open the source code
in the other window.  You can explicitly specify the object by
selecting it."
  (interactive "P")
  (let ((kernel (ein2:get-kernel))
        (object (ein2:object-at-point)))
    (assert (ein2:kernel-live-p kernel) nil "Kernel is not ready.")
    (assert object nil "Object at point not found.")
    (ein2:pytools-jump-to-source kernel object other-window
                                (when ein2:propagate-connect
                                  (ein2:get-notebook)))))

(defun ein2:pytools-jump-back-command (&optional other-window)
  "Go back to the point where `ein2:pytools-jump-to-source-command'
is executed last time.  When the prefix argument ``C-u`` is
given, open the last point in the other window."
  (interactive "P")
  (when (ein2:aand (car ein2:pytools-jump-stack)
                  (equal (point) (marker-position it)))
    (setq ein2:pytools-jump-stack (cdr ein2:pytools-jump-stack)))
  (ein2:aif (car ein2:pytools-jump-stack)
      (ein2:goto-marker it other-window)
    (ein2:log 'info "Nothing on stack.")))

(define-obsolete-function-alias
  'ein2:pytools-eval-string-internal
  'ein2:shared-output-eval-string "0.1.2")

(defun ein2:pytools-doctest ()
  "Do the doctest of the object at point."
  (interactive)
  (let ((object (ein2:object-at-point)))
    (ein2:shared-output-eval-string
     (format "__import__('ein').run_docstring_examples(%s)" object)
     t)))

(defun ein2:pytools-whos ()
  "Execute ``%whos`` magic command and popup the result."
  (interactive)
  (ein2:shared-output-eval-string "%whos" t))

(defun ein2:pytools-hierarchy (&optional ask)
  "Draw inheritance graph of the class at point.
hierarchymagic_ extension is needed to be installed.
You can explicitly specify the object by selecting it.

.. _hierarchymagic: https://github.com/tkf/ipython-hierarchymagic"
  (interactive "P")
  (let ((object (ein2:object-at-point)))
    (when ask
      (setq object (read-from-minibuffer "class or object: " object)))
    (assert (and object (not (equal object "")))
            nil "Object at point not found.")
    (ein2:shared-output-eval-string (format "%%hierarchy %s" object) t)))

(defun ein2:pytools-pandas-to-ses (dataframe)
  "View pandas_ DataFrame in SES_ (Simple Emacs Spreadsheet).
Open a `ses-mode' buffer and import DataFrame object into it.

SES_ is distributed with Emacs since Emacs 22, so you don't need
to install it if you are using newer Emacs.

.. _pandas: http://pandas.pydata.org
.. _SES: http://www.gnu.org/software/emacs/manual/html_node/ses/index.html"
  (interactive (list (read-from-minibuffer "pandas DataFrame "
                                           (ein2:object-at-point))))
  (let ((buffer (get-buffer-create
                 (generate-new-buffer-name "*ein2:ses pandas*"))))
    ;; fetch TSV (tab separated values) via stdout
    (ein2:kernel-request-stream
     (ein2:get-kernel)
     (concat dataframe ".to_csv(__import__('sys').stdout, sep='\\t')")
     (lambda (tsv buffer)
       (with-current-buffer buffer
         (cl-flet ((y-or-n-p
                    (prompt)
                    (if (string-prefix-p "Yank will insert " prompt)
                        t
                      (error "Unexpected prompt: %s" prompt))))
           ;; Import DataFrame as TSV
           (ses-yank-tsf tsv nil))
         ;; Force SES to update (equivalent to run `post-command-hook').
         (ses-command-hook)))
     (list buffer))
    ;; Open `ses-mode' buffer
    (with-current-buffer buffer
      (ses-mode))
    (pop-to-buffer buffer)))

(provide 'ein-pytools)

;;; ein-pytools.el ends here
