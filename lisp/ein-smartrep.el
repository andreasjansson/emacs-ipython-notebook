;;; ein-smartrep.el --- smartrep integration

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-smartrep.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-smartrep.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-smartrep.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'smartrep nil t)
(require 'ein-notebook)

(defcustom ein2:smartrep-notebook-mode-alist
  '(("C-t" . ein2:worksheet-toggle-cell-type)
    ("C-l" . ein2:worksheet-clear-output)
    ("C-k" . ein2:worksheet-kill-cell)
    ("C-y" . ein2:worksheet-yank-cell)
    ("C-a" . ein2:worksheet-insert-cell-above)
    ("C-b" . ein2:worksheet-insert-cell-below)
    ("C-n" . ein2:worksheet-goto-next-input)
    ("C-p" . ein2:worksheet-goto-prev-input)
    ("C-m" . ein2:worksheet-merge-cell)
    ("<up>" . ein2:worksheet-move-cell-up)
    ("<down>" . ein2:worksheet-move-cell-down)
    )
  "alist passed to `smartrep-define-key'."
  :group 'ein)

(defun ein2:smartrep-config ()
  (interactive)
  (smartrep-define-key
      ein2:notebook-mode-map
      "C-c"
    ein2:smartrep-notebook-mode-alist))


(defvar ein2:smartrep-config-once-called nil)

(defun ein2:smartrep-config-once ()
  (unless ein2:smartrep-config-once-called
    (setq ein2:smartrep-config-once-called t)
    (ein2:smartrep-config)))

(provide 'ein-smartrep)

;;; ein-smartrep.el ends here
