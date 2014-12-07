;;; ein-connect.el --- Connect external buffers to IPython

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-connect.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-connect.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-connect.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: There is a problem when connected notebook is closed.
;;        This can be fixed in some ways:
;; * Turn off ein2:connect when the command that uses kernel is invoked
;;   but corresponding notebook was closed already.
;; * Connect directly to ein2:kernel and make its destructor to care
;;   about connecting buffers.

;;; Code:

(require 'eieio)
(eval-when-compile (require 'auto-complete nil t))

(require 'ein-notebook)

(declare-function ein2:notebooklist-list-notebooks "ein-notebooklist")
(declare-function ein2:notebooklist-open-notebook-global "ein-notebooklist")


;;; Utils

(defun ein2:maybe-save-buffer (option)
  "Conditionally save current buffer.
Return `t' if the buffer is unmodified or `nil' otherwise.
If the buffer is modified, buffer is saved depending on the value
of OPTION:
  ask  : Ask whether the buffer should be saved.
  yes  : Save buffer always.
  no   : Do not save buffer."
  (if (not (buffer-modified-p))
      t
    (case option
      (ask (when (y-or-n-p "Save buffer? ")
             (save-buffer)
             t))
      (yes (save-buffer)
           t)
      (t nil))))


;;; Configuration

(defcustom ein2:connect-run-command "%run"
  "``%run`` magic command used for `ein2:connect-run-buffer'.
Types same as `ein2:console-security-dir' are valid."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ein)

(defcustom ein2:connect-reload-command "%run -n"
  "Setting for `ein2:connect-reload-buffer'.
Same as `ein2:connect-run-command'."
  :type '(choice
          (string :tag "command" "%run")
          (alist :tag "command mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (string :tag "command" "%run"))
          (function :tag "command getter"
                    (lambda (url-or-port) (format "%%run -n -i -t -d"))))
  :group 'ein)

(defun ein2:connect-run-command-get ()
  (ein2:choose-setting 'ein2:connect-run-command
                      (ein2:$notebook-url-or-port (ein2:connect-get-notebook))))

(defcustom ein2:connect-save-before-run 'yes
  "Whether the buffer should be saved before `ein2:connect-run-buffer'."
  :type '(choice (const :tag "Always save buffer" yes)
                 (const :tag "Always do not save buffer" no)
                 (const :tag "Ask" ask))
  :group 'ein)

(defcustom ein2:connect-aotoexec-lighter nil
  "String appended to the lighter of `ein2:connect-mode' (`ein2:c')
when auto-execution mode is on.  When `nil', use the same string
as `ein2:cell-autoexec-prompt'."
  :type '(choice (string :tag "String appended to ein2:c" "@")
                 (const :tag "Use `ein2:cell-autoexec-prompt'." nil))
  :group 'ein)

(defcustom ein2:connect-default-notebook nil
  "Notebook to be connect when `ein2:connect-to-default-notebook' is called.

Example setting to connect to \"My_Notebook\" in the server at
port 8888 when opening any buffer in `python-mode'::

  (setq ein2:connect-default-notebook \"8888/My_Notebook\")
  (add-hook 'python-mode-hook 'ein2:connect-to-default-notebook)

`ein2:connect-default-notebook' can also be a function without any
argument.  This function must return a string (notebook path of
the form \"URL-OR-PORT/NOTEBOOK-NAME\").

As `ein2:connect-to-default-notebook' requires notebook list to be
loaded, consider using `ein2:notebooklist-load' to load notebook
list if you want to connect to notebook without manually opening
notebook list."
  :type '(choice (string :tag "URL-OR-PORT/NOTEBOOK-NAME")
                 (function :tag "Notebook path getter"))
  :group 'ein)


;;; Class

(ein2:deflocal ein2:%connect% nil
  "Buffer local variable to store an instance of `ein2:connect'")
(define-obsolete-variable-alias 'ein2:@connect 'ein2:%connect% "0.1.2")

(defclass ein2:connect ()
  ((notebook :initarg :notebook :type ein2:$notebook)
   (buffer :initarg :buffer :type buffer)
   (autoexec :initarg :autoexec :initform nil :type boolean
             :document "Auto-execution mode flag.

See also the document of the `autoexec' slot of `ein2:codecell'
class.")))

(defun ein2:connect-setup (notebook buffer)
  (with-current-buffer buffer
    (setq ein2:%connect%
          (ein2:connect "Connect" :notebook notebook :buffer buffer))
    ein2:%connect%))


;;; Methods

;; FIXME: Clarify names of these `connect-to-*' functions:

;;;###autoload
(defun ein2:connect-to-notebook-command (&optional not-yet-opened)
  "Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks."
  (interactive "P")
  (call-interactively (if not-yet-opened
                          #'ein2:connect-to-notebook
                        #'ein2:connect-to-notebook-buffer)))

;;;###autoload
(defun ein2:connect-to-notebook (nbpath &optional buffer no-reconnection)
  "Connect any buffer to notebook and its kernel."
  (interactive
   (list
    (completing-read
     "Notebook to connect [URL-OR-PORT/NAME]: "
     (ein2:notebooklist-list-notebooks))))
  (ein2:notebooklist-open-notebook-global
   nbpath
   (lambda (notebook -ignore- buffer no-reconnection)
     (ein2:connect-buffer-to-notebook notebook buffer no-reconnection))
   (list (or buffer (current-buffer)) no-reconnection)))

;;;###autoload
(defun ein2:connect-to-notebook-buffer (buffer-or-name)
  "Connect any buffer to opened notebook and its kernel."
  (interactive (list (completing-read "Notebook buffer to connect: "
                                      (ein2:notebook-opened-buffer-names))))
  (let ((notebook
         (buffer-local-value 'ein2:%notebook% (get-buffer buffer-or-name))))
    (ein2:connect-buffer-to-notebook notebook)))

;;;###autoload
(defun ein2:connect-buffer-to-notebook (notebook &optional buffer
                                                no-reconnection)
  "Connect BUFFER to NOTEBOOK."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (if (or (not no-reconnection)
            (not ein2:%connect%))
        (let ((connection (ein2:connect-setup notebook buffer)))
          (when (ein2:eval-if-bound 'ac-sources)
            (push 'ac-source-ein-async ac-sources))
          (ein2:connect-mode)
          (ein2:log 'info "Connected to %s"
                   (ein2:$notebook-notebook-name notebook))
          connection)
      (ein2:log 'info "Buffer is already connected to notebook."))))

(defun ein2:connect-get-notebook ()
  (oref ein2:%connect% :notebook))

(defun ein2:connect-get-kernel ()
  (ein2:$notebook-kernel (ein2:connect-get-notebook)))

(defun ein2:connect-eval-buffer ()
  "Evaluate the whole buffer.  Note that this will run the code
inside the ``if __name__ == \"__main__\":`` block."
  (interactive)
  (ein2:shared-output-eval-string (buffer-string) nil nil nil :silent t)
  (ein2:connect-execute-autoexec-cells)
  (ein2:log 'info "Whole buffer is sent to the kernel."))

(defun ein2:connect-run-buffer (&optional ask-command)
  "Run buffer using ``%run``.  Ask for command if the prefix ``C-u`` is given.
Variable `ein2:connect-run-command' sets the default command."
  (interactive "P")
  (ein2:aif (ein2:aand (ein2:get-url-or-port)
                     (ein2:filename-to-python it (buffer-file-name)))
      (let* ((default-command (ein2:connect-run-command-get))
             (command (if ask-command
                          (read-from-minibuffer "Command: " default-command)
                        default-command))
             (cmd (format "%s %s" command it)))
        (if (ein2:maybe-save-buffer ein2:connect-save-before-run)
            (progn
              (ein2:shared-output-eval-string cmd nil nil nil :silent t)
              (ein2:connect-execute-autoexec-cells)
              (ein2:log 'info "Command sent to the kernel: %s" cmd))
          (ein2:log 'info "Buffer must be saved before %%run.")))
    (error (concat "This buffer has no associated file.  "
                   "Use `ein2:connect-eval-buffer' instead."))))

(defun ein2:connect-run-or-eval-buffer (&optional eval)
  "Run buffer using the ``%run`` magic command or eval whole
buffer if the prefix ``C-u`` is given.
Variable `ein2:connect-run-command' sets the command to run.
You can change the command and/or set the options.
See also: `ein2:connect-run-buffer', `ein2:connect-eval-buffer'."
  (interactive "P")
  (if eval
      (ein2:connect-eval-buffer)
    (ein2:connect-run-buffer)))

(defun ein2:connect-reload-buffer ()
  "Reload buffer using the command set by `ein2:connect-reload-command'."
  (interactive)
  (let ((ein2:connect-run-command ein2:connect-reload-command))
    (call-interactively #'ein2:connect-run-buffer)))

(defun ein2:connect-eval-region (start end)
  (interactive "r")
  (ein2:shared-output-eval-string (buffer-substring start end))
  (ein2:log 'info "Selected region is sent to the kernel."))

(define-obsolete-function-alias
  'ein2:connect-eval-string-internal
  'ein2:shared-output-eval-string "0.1.2")

(define-obsolete-function-alias
  'ein2:connect-request-tool-tip-or-help-command
  'ein2:pytools-request-tooltip-or-help "0.1.2")

(defun ein2:connect-pop-to-notebook ()
  (interactive)
  (ein2:connect-assert-connected)
  (pop-to-buffer (ein2:notebook-buffer (ein2:connect-get-notebook))))


;;; Generic getter

(defun ein2:get-url-or-port--connect ()
  (ein2:aand (ein2:get-notebook--connect) (ein2:$notebook-url-or-port it)))

(defun ein2:get-notebook--connect ()
  (when (ein2:connect-p ein2:%connect%)
    (oref ein2:%connect% :notebook)))

(defun ein2:get-kernel--connect ()
  (ein2:aand (ein2:get-notebook--connect) (ein2:$notebook-kernel it)))

(defun ein2:get-traceback-data--connect ()
  ;; FIXME: Check if the TB in shared-output buffer is originated from
  ;;        the current buffer.
  (ein2:aand (ein2:shared-output-get-cell) (ein2:cell-get-tb-data it)))
(autoload 'ein2:shared-output-get-cell "ein-shared-output") ; FIXME: Remove!


;;; Auto-execution

(defun ein2:connect-assert-connected ()
  (assert (ein2:connect-p ein2:%connect%) nil
          "Current buffer (%s) is not connected to IPython notebook."
          (buffer-name))
  (assert (ein2:notebook-live-p (oref ein2:%connect% :notebook)) nil
          "Connected notebook is not live (probably already closed)."))

(defun ein2:connect-execute-autoexec-cells ()
  "Call `ein2:notebook-execute-autoexec-cells' via `after-save-hook'."
  (ein2:connect-assert-connected)
  (when (oref ein2:%connect% :autoexec)
    (ein2:notebook-execute-autoexec-cells (ein2:connect-get-notebook))))

(defun ein2:connect-toggle-autoexec ()
  "Toggle auto-execution mode of the current connected buffer.

When auto-execution mode is on, cells in connected notebook will
be automatically executed whenever run, eval or reload command [#]_
is called in this buffer.

.. [#] Namely, one of

   * `ein2:connect-run-buffer'
   * `ein2:connect-eval-buffer'
   * `ein2:connect-run-or-eval-buffer'
   * `ein2:connect-reload-buffer'

Note that you need to set cells to run in the connecting buffer
or no cell will be executed.
Use the `ein2:worksheet-turn-on-autoexec' command in notebook to
change the cells to run."
  (interactive)
  (ein2:connect-assert-connected)
  (let ((autoexec-p (not (oref ein2:%connect% :autoexec))))
    (oset ein2:%connect% :autoexec autoexec-p)
    (ein2:log 'info "Auto-execution mode is %s."
             (if autoexec-p "enabled" "disabled"))))


;;; Auto-connect

;;;###autoload
(defun ein2:connect-to-default-notebook ()
  "Connect to the default notebook specified by
`ein2:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook."
  (ein2:log 'verbose "CONNECT-TO-DEFAULT-NOTEBOOK")
  (ein2:and-let* ((nbpath ein2:connect-default-notebook)
                 ((not (ein2:worksheet-buffer-p))))
    (when (functionp nbpath)
      (setq nbpath (funcall nbpath)))
    (ein2:connect-to-notebook nbpath nil t)))



;;; ein2:connect-mode

(defvar ein2:connect-mode-map (make-sparse-keymap))

(let ((map ein2:connect-mode-map))
  (define-key map "\C-c\C-c" 'ein2:connect-run-or-eval-buffer)
  (define-key map "\C-c\C-l" 'ein2:connect-reload-buffer)
  (define-key map "\C-c\C-r" 'ein2:connect-eval-region)
  (define-key map (kbd "C-:") 'ein2:shared-output-eval-string)
  (define-key map "\C-c\C-f" 'ein2:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ein2:completer-complete)
  (define-key map "\C-c\C-z" 'ein2:connect-pop-to-notebook)
  (define-key map "\C-c\C-a" 'ein2:connect-toggle-autoexec)
  (define-key map "\C-c\C-o" 'ein2:console-open)
  (define-key map "\C-c\C-x" 'ein2:tb-show)
  (define-key map "\M-."          'ein2:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein2:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein2:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein2:pytools-jump-back-command)
  (define-key map (kbd "C-c C-/") 'ein2:notebook-scratchsheet-open)
  map)

(defun ein2:connect-mode-get-lighter ()
  (if (oref ein2:%connect% :autoexec)
      (format " ein2:c%s" (or ein2:connect-aotoexec-lighter
                             ein2:cell-autoexec-prompt))
    " ein2:c"))

(define-minor-mode ein2:connect-mode
  "Minor mode for communicating with IPython notebook.

\\{ein2:connect-mode-map}"
  :lighter (:eval (ein2:connect-mode-get-lighter))
  :keymap ein2:connect-mode-map
  :group 'ein
  (ein2:complete-on-dot-install ein2:connect-mode-map))

(put 'ein2:connect-mode 'permanent-local t)


(provide 'ein-connect)

;;; ein-connect.el ends here
