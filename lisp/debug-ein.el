;;; debug-ein2.el --- Debug ein2.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; debug-ein2.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; debug-ein2.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with debug-ein2.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs -Q -L path/to/nxhtml/util/ -l debug-ein2.el

;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'ein2)
(require 'ein2-dev)

(ein2:dev-start-debug)
(ein2:notebooklist-open)


;;; Extra stuff

(require 'markdown-mode nil t)
(require 'rst nil t)

(declare-function ein2:ac-config "ein2-ac")
(declare-function global-auto-complete-mode "auto-complete")
(when (featurep 'auto-complete)
  (global-auto-complete-mode t)
  (setq ein2:use-auto-complete-superpack t))

(declare-function ein2:smartrep-config "ein2-smartrep")
(when (featurep 'smartrep)
  (setq ein2:use-smartrep t))

(custom-set-faces
   ;; Turn off background color for mumamo major chunk, to see
   ;; highlighting of prompt and stderr.
   '(mumamo-background-chunk-major
     ((((class color) (min-colors 88) (background dark)) nil)))
   ;; '(mumamo-background-chunk-submode1
   ;;   ((((class color) (min-colors 88) (background dark)) nil)))
   )


;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
;; See: http://stackoverflow.com/a/5470584/727827
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))

;;; debug-ein2.el ends here
