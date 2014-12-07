;;; ein-multilang.el --- Notebook mode with multiple language fontification

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-multilang.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-multilang.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-multilang.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (defvar markdown-mode-map))

(require 'ein-worksheet)
(require 'ein-multilang-fontify)

(defun ein2:ml-fontify (limit)
  "Fontify next input area comes after the current point then
return `t' or `nil' if not found.
See info node `(elisp) Search-based Fontification'."
  (ein2:log-ignore-errors
    (ein2:ml-fontify-1 limit)))

(defun ein2:ml-current-or-next-input-cell (ewoc-node)
  "Almost identical to `ein2:worksheet-next-input-cell' but return
the current cell if EWOC-NODE is the input area node."
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein2:$node-data ewoc-data))
         (path (ein2:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element '(prompt input))
        cell
      (ein2:cell-next cell))))

(defun ein2:ml-fontify-1 (limit)
  "Actual implementation of `ein2:ml-fontify'.
This function may raise an error."
  (ein2:and-let* ((pos (point))
                 (node (ein2:worksheet-get-nearest-cell-ewoc-node pos limit))
                 (cell (ein2:ml-current-or-next-input-cell node))
                 (start (ein2:cell-input-pos-min cell))
                 (end   (ein2:cell-input-pos-max cell))
                 ((<= end limit))
                 ((< start end))
                 (lang (ein2:cell-language cell)))
    (let ((inhibit-read-only t))
      (ein2:mlf-font-lock-fontify-block lang start end)
      ;; Emacs fontification mechanism requires the function to move
      ;; the point.  Do *not* use `(goto-char end)'.  As END is in the
      ;; input area, fontification falls into an infinite loop.
      (ewoc-goto-node (oref cell :ewoc) (ein2:cell-element-get cell :footer)))
    t))

(defun ein2:ml-back-to-prev-node ()
  (ein2:aand (ein2:worksheet-get-ewoc) (ewoc-goto-prev it 1)))

(defvar ein2:ml-font-lock-keywords
  '((ein2:ml-fontify))
  "Default `font-lock-keywords' for `ein2:notebook-multilang-mode'.")

(defun ein2:ml-set-font-lock-defaults ()
  (set (make-local-variable 'font-lock-defaults)
       '(ein2:ml-font-lock-keywords
         ;; The following are adapted from org-mode but I am not sure
         ;; if I need them:
         t nil nil
         ein2:ml-back-to-prev-node)))

;;;###autoload
(define-derived-mode ein2:notebook-multilang-mode fundamental-mode "ein2:ml"
  "Notebook mode with multiple language fontification."
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'parse-sexp-lookup-properties)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (setq beginning-of-defun-function 'ein2:worksheet-beginning-of-cell-input)
  (setq end-of-defun-function 'ein2:worksheet-end-of-cell-input)
  (ein2:ml-lang-setup-python)
  (ein2:ml-set-font-lock-defaults))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'ein2:notebook-multilang-mode))


;;; Language setup functions

(defun ein2:ml-lang-setup-python ()
  (setq comment-start "# ")
  (setq comment-start-skip "#+\\s-*")
  (setq parse-sexp-lookup-properties t)
  (setq parse-sexp-ignore-comments t)

  (when (boundp 'python-mode-map)
    (set-keymap-parent ein2:notebook-multilang-mode-map python-mode-map))
  (cond
   ((featurep 'python)
    (setq indent-line-function #'python-indent-line-function)
    (setq indent-region-function #'python-indent-region))
   ((featurep 'python-mode)
    ;; FIXME: write keymap setup for python-mode.el
    )))

(defun ein2:ml-lang-setup-markdown ()
  "Use `markdown-mode-map'.  NOTE: This function is not used now."
  (when (featurep 'markdown-mode)
    (set-keymap-parent ein2:notebook-multilang-mode-map markdown-mode-map)))

;; FIXME: dynamically call ein2:ml-lang-setup-LANG using
;;        `post-command-hook'.
;; FIMXE: add more ein2:ml-lang-setup-LANG to switch kaymap.


;;; yasnippet

(defvar ein2:ml-yasnippet-parents '(python-mode markdown-mode)
  "Parent modes for `ein2:notebook-multilang-mode' to register in yasnippet.")

(defun ein2:ml-setup-yasnippet ()
  (loop for define-parents in '(yas/define-parents
                                yas--define-parents)
        when (fboundp define-parents)
        do (ignore-errors
             ;; `let' is for workaround the bug in yasnippet
             (let ((mode-sym 'ein2:notebook-multilang-mode))
               (funcall define-parents
                        mode-sym
                        ein2:ml-yasnippet-parents)))))

(eval-after-load "yasnippet" '(ein2:ml-setup-yasnippet))

(provide 'ein-multilang)

;;; ein-multilang.el ends here
