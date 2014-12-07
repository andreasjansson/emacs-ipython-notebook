;;; ein-mumamo.el --- MuMaMo for notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-mumamo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-mumamo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-mumamo.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mumamo)

(require 'ein-worksheet)



;;; Customization

(defcustom ein2:mumamo-codecell-mode 'python-mode
  "Major Mode for Code Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-textcell-mode 'text-mode
  "Major Mode for Text Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-htmlcell-mode 'html-mode
  "Major Mode for HTML Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-markdowncell-mode 'markdown-mode
  "Major Mode for Markdown Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-rawcell-mode 'rst-mode
  "Major Mode for Raw Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-headingcell-mode 'text-mode
  "Major Mode for Heading Cell."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:mumamo-fallback-mode 'text-mode
  "Fallback Major Mode."
  :type '(symbol :tag "Major Mode")
  :group 'ein)

(defcustom ein2:use-mumamo-indent-line-function-workaround t
  "Turn on workaround for `mumamo-indent-line-function'.

In code cell, hitting TAB or C-j at the end of input area causes
error from MuMaMo.  When this variable is non-`nil', EIN patches
`mumamo-indent-line-function' to workaround this problem.  This
workaround is on by default.

Note that python-mode's indentation function has other problems
with MuMaMo.  For example, hitting TAB twice, which decreases the
indentation level by one in normal Python buffer, causes similar
error in code cell.  The current workaround does not fix this
problem."
  :type 'boolean
  :group 'ein)

(defcustom ein2:mumamo-indent-line-function-dummy-code "
def ein_dummy():
    return"
  "Dummy code block for `mumamo-indent-line-function' workaround.
This code block will be inserted at the end of cell input before
indentation and then removed afterward (so user will not see this
code).

This is ugly but... \"practicality beats purity\"...
I guess somebody should fix python.el and/or MuMaMo, in order to
remove this ugliness.

To make the workaround less aggressive, you can set a newline
\"\\n\" for this variable.  In that case, you will be affected by
`issue 24`_.

.. _issue 24: https://github.com/tkf/emacs-ipython-notebook/issues/24"
  :type 'boolean
  :group 'ein)



;;; Workaround

(defadvice mumamo-indent-line-function
  (around ein2:mumamo-indent-line-function-workaround)
  "Workaround the indentation problem when the cursor is in the
code cell."
  (let ((cell (ein2:worksheet-get-current-cell)))
    ;; Check if the current buffer is notebook AND the current cell is
    ;; code cell.
    (if (ein2:codecell-p cell)
        (let ((cur (copy-marker (point)))
              (end (copy-marker (1+ (ein2:cell-input-pos-max cell)))))
          ;;             v-- execute `delete-char' here
          ;; ... [] ......DUMMY
          ;;      ^- cur       ^- end (non-inclusive end of cell)
          ;;      ^- `ad-do-it' here
          (unwind-protect
              (progn
                (goto-char (1- end))
                (insert ein2:mumamo-indent-line-function-dummy-code)
                (goto-char cur)
                ad-do-it)
            (save-excursion
              (let ((len (length ein2:mumamo-indent-line-function-dummy-code)))
                (goto-char (- end 1 len))
                (delete-char len)))))
      ad-do-it)))

(defun ein2:mumamo-indent-line-function-workaround-turn-on ()
  "Activate advice for `mumamo-indent-line-function'.
Called via `ein2:notebook-mumamo-mode-hook'."
  (when ein2:use-mumamo-indent-line-function-workaround
    (ad-enable-advice 'mumamo-indent-line-function 'around
                      'ein2:mumamo-indent-line-function-workaround)
    (ad-activate 'mumamo-indent-line-function)))

(defun ein2:mumamo-imenu-setup-maybe ()
  "Set `imenu-create-index-function' if the current buffer is the
notebook buffer.
This function is called via `after-change-major-mode-hook', to set
the variable every time visiting the different chunks.

.. note:: Making `imenu-create-index-function' permanent-local
   also solves the problem.  However, this will make the variable
   permanent-local in *any* buffer, including the buffers
   irrelevant to EIN.  Therefore, the current approach is taken.

This is the same workaround as `ein2:ac-setup-maybe'."
  (when (ein2:worksheet-buffer-p)
    (ein2:worksheet-imenu-setup)))

(add-hook 'after-change-major-mode-hook 'ein2:mumamo-imenu-setup-maybe)



;;; `ein2:notebook-mumamo-mode'

(define-derived-mode ein2:notebook-bg-mode fundamental-mode "ein2:bg"
  "Background mode for `ein2:notebook-mumamo-mode'."
  (setq font-lock-defaults '(nil t))
  (font-lock-mode))

(define-mumamo-multi-major-mode ein2:notebook-mumamo-mode
  "IPython notebook mode."
  ("IPython notebook familiy" ein2:notebook-bg-mode
   (ein2:mumamo-chunk-codecell
    ein2:mumamo-chunk-textcell
    ein2:mumamo-chunk-htmlcell
    ein2:mumamo-chunk-markdowncell
    ein2:mumamo-chunk-rawcell
    ein2:mumamo-chunk-headingcell
    )))

(add-hook 'ein2:notebook-mumamo-mode-hook
          'ein2:mumamo-indent-line-function-workaround-turn-on)



;;; Chunk functions

(defmacro ein2:mumamo-define-chunk (name)
  (let ((funcname (intern (format "ein2:mumamo-chunk-%s" name)))
        (mode (intern (format "ein2:mumamo-%s-mode" name)))
        (cell-p (intern (format "ein2:%s-p" name))))
    `(defun ,funcname (pos max)
       (mumamo-possible-chunk-forward
        pos max
        (lambda (pos max) "CHUNK-START-FUN"
          (ein2:log 'blather "CHUNK-START-FUN(pos=%s max=%s)" pos max)
          (ein2:aif (ein2:mumamo-find-edge pos max nil #',cell-p)
              (list it (if (functionp ,mode)
                           ,mode
                         ein2:mumamo-fallback-mode)
                    nil)))
        (lambda (pos max) "CHUNK-END-FUN"
          (ein2:log 'blather "CHUNK-END-FUN(pos=%s max=%s)" pos max)
          (ein2:mumamo-find-edge pos max t #',cell-p))))))

(ein2:mumamo-define-chunk codecell)
(ein2:mumamo-define-chunk textcell)
(ein2:mumamo-define-chunk htmlcell)
(ein2:mumamo-define-chunk markdowncell)
(ein2:mumamo-define-chunk rawcell)
(ein2:mumamo-define-chunk headingcell)

(defun ein2:mumamo-find-edge (pos max end cell-p)
  "Helper function for `ein2:mumamo-chunk-codecell'.

Return the point of beginning of the input element of cell after
the point POS.  Return `nil' if it cannot be found before the point
MAX.  If END is non-`nil', end of the input element is returned."
  (ein2:log 'blather "EIN2:MUMAMO-FIND-EDGE(pos=%s max=%s end=%s cell-p=%s)"
           pos max end cell-p)
  (let* ((ewoc-node
          (ein2:worksheet-get-nearest-cell-ewoc-node pos max cell-p))
         (_ (ein2:log 'blather "(null ewoc-node) = %s" (null ewoc-node)))
         (cell (ein2:aand ewoc-node
                         (ein2:$node-data (ewoc-data it))))
         (_ (ein2:log 'blather "(null cell) = %s" (null cell)))
         (find
          (lambda (c)
            (ein2:aand c
                      (ein2:cell-element-get it (if end :after-input :input))
                      (progn
                        (ein2:log 'blather "(null it) = %s" (null it))
                        (ewoc-location it))
                      (if end it (1+ it)))))
         (input-pos (funcall find cell)))
    (ein2:log 'blather "input-pos (1) = %s" input-pos)
    (when (and input-pos (< input-pos pos))
      (setq input-pos (ein2:aand (ein2:cell-next cell)
                                (when (funcall cell-p it) (funcall find it)))))
    (ein2:log 'blather "input-pos (2) = %s" input-pos)
    (when (and input-pos (> input-pos max))
      (setq input-pos nil))
    (ein2:log 'blather "input-pos (3) = %s" input-pos)
    input-pos))

(provide 'ein-mumamo)

;;; ein-mumamo.el ends here
