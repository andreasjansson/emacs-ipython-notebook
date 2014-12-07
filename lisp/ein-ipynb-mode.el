;;; ein-ipynb-mode.el --- A simple mode for ipynb file

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-ipynb-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-ipynb-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-ipynb-mode.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-notebooklist)


(defvar ein2:ipynb-parent-mode 'js-mode
  "A mode (a symbol) to use for parent mode for `ein2:ipynb-mode'.
Note that this variable must be set *before* compiling EIN.")

(defalias 'ein2:ipynb-parent-mode ein2:ipynb-parent-mode)

;;;###autoload
(define-derived-mode ein2:ipynb-mode ein2:ipynb-parent-mode "ein2:ipynb"
  "A simple mode for ipynb file.")

(let ((map ein2:ipynb-mode-map))
  (define-key map "\C-c\C-z" 'ein2:notebooklist-open-notebook-by-file-name)
  (define-key map "\C-c\C-o" 'ein2:notebooklist-open-notebook-by-file-name)
  (easy-menu-define ein2:ipynb-menu map "EIN IPyNB Mode Menu"
    `("EIN IPyNB File"
      ,@(ein2:generate-menu
         '(("Open notebook" ein2:notebooklist-open-notebook-by-file-name))))))

;;;###autoload
(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ein2:ipynb-mode))


(provide 'ein-ipynb-mode)

;;; ein-ipynb-mode.el ends here
