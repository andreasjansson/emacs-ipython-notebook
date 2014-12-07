;;; ein-pager.el --- Pager module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-pager.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-pager.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-pager.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ansi-color)

(require 'ein-core)
(require 'ein-events)

;; FIXME: Make a class with `:get-notebook-name' slot like `ein2:worksheet'

(defun ein2:pager-new (name events)
  ;; currently pager = name.
  (ein2:pager-bind-events name events)
  name)

(defun ein2:pager-bind-events (pager events)
  "Bind events related to PAGER to the event handler EVENTS."
  (ein2:events-on events
                 'open_with_text.Pager
                 #'ein2:pager--open-with-text
                 pager))

(defun ein2:pager--open-with-text (pager data)
  (let ((text (plist-get data :text)))
    (unless (equal (ein2:trim text) "")
      (ein2:pager-clear pager)
      (ein2:pager-expand pager)
      (ein2:pager-append-text pager text))))

(defun ein2:pager-clear (pager)
  (ein2:with-read-only-buffer (get-buffer-create pager)
    (erase-buffer)))

(defun ein2:pager-expand (pager)
  (pop-to-buffer (get-buffer-create pager))
  (goto-char (point-min)))

(defun ein2:pager-append-text (pager text)
  (ein2:with-read-only-buffer (get-buffer-create pager)
    (insert (ansi-color-apply text))
    (ein2:pager-mode)))

;; FIXME: this should be automatically called when opening pager.
(defun ein2:pager-goto-docstring-bset-loc ()
  "Goto the best location of the documentation."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Docstring:")
  (beginning-of-line 0)
  (recenter 0))

(define-derived-mode ein2:pager-mode fundamental-mode "ein2:pager"
  "IPython notebook pager mode."
  (view-mode)
  (font-lock-mode))

(setq ein2:pager-mode-map (make-sparse-keymap))

(let ((map ein2:pager-mode-map))
  (define-key map "\C-c\C-b" 'ein2:pager-goto-docstring-bset-loc)
  (define-key map "q" 'bury-buffer)
  map)

(provide 'ein-pager)

;;; ein-pager.el ends here
