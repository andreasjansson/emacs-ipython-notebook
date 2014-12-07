;;; ein-junk.el --- Open a notebook to do random things

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-junk.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-junk.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-junk.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-notebooklist)


(define-obsolete-variable-alias 'ein2:scratch-notebook-name-template
  'ein2:junk-notebook-name-template "0.2.0")

(defcustom ein2:junk-notebook-name-template "junk-%Y-%m-%d-%H%M%S"
  "Junk notebook name template.
This value is used from `ein2:notebooklist-new-scratch-notebook'
and `ein2:notebook-rename-to-scratch-command'.  This must be a
format string which can be passed to `format-time-string'."
  :type '(string :tag "Format string")
  :group 'ein)

(defun ein2:junk-notebook-name ()
  "Generate new scratch notebook name based on `current-time' and
`ein2:junk-notebook-name-template'."
  (format-time-string ein2:junk-notebook-name-template (current-time)))


(define-obsolete-function-alias 'ein2:notebooklist-new-scratch-notebook
  'ein2:junk-new)

;;;###autoload
(defun ein2:junk-new (name url-or-port)
  "Open a notebook to try random thing.
Notebook name is determined based on
`ein2:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use."
  (interactive (let ((name (ein2:junk-notebook-name))
                     (url-or-port (or (ein2:get-url-or-port)
                                      (ein2:default-url-or-port))))
                 (setq name (read-string "Open notebook as: " name))
                 (when current-prefix-arg
                   (setq url-or-port (ein2:notebooklist-ask-url-or-port)))
                 (list name url-or-port)))
  (ein2:notebooklist-new-notebook-with-name name url-or-port))


(define-obsolete-function-alias ' ein2:notebook-rename-to-scratch-command
  'ein2:junk-rename)

;;;###autoload
(defun ein2:junk-rename (name)
  "Rename the current notebook based on `ein2:junk-notebook-name-template'
and save it immediately."
  (interactive
   (list (read-string "Rename notebook: "
                      (ein2:junk-notebook-name))))
  (ein2:notebook-rename-command name))

(provide 'ein-junk)

;;; ein-junk.el ends here
