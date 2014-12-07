;;; ein-completer.el --- Completion module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-completer.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-completer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-completer.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(declare-function ac-cursor-on-diable-face-p "auto-complete")

(eval-when-compile (require 'cl))

(require 'ein-core)
(require 'ein-log)
(require 'ein-subpackages)
(require 'ein-kernel)

(defun ein2:completer-choose ()
  (when (require 'auto-complete nil t)
    (require 'ein-ac))
  (cond
   ((and (or ein2:use-auto-complete
             ein2:use-auto-complete-superpack)
         (ein2:eval-if-bound 'auto-complete-mode)
         (fboundp 'ein2:completer-finish-completing-ac))
    #'ein2:completer-finish-completing-ac)
   (t
    #'ein2:completer-finish-completing-default)))

(defun ein2:completer-beginning (matched-text)
  (save-excursion
    (re-search-backward (concat matched-text "\\="))))

(defun ein2:completer-finish-completing (args content -metadata-not-used-)
  (ein2:log 'debug "COMPLETER-FINISH-COMPLETING: content=%S" content)
  (let ((matched-text (plist-get content :matched_text))
        (matches (plist-get content :matches))
        (completer (ein2:completer-choose)))
    (ein2:log 'debug "COMPLETER-FINISH-COMPLETING: completer=%s" completer)
    (apply completer matched-text matches args)))

(defun ein2:completer-finish-completing-default (matched-text matches
                                                             &rest -ignore-)
  (let* ((end (point))
         (beg (ein2:completer-beginning matched-text))
         (word (if (and beg matches)
                   (completing-read "Complete: " matches
                                    nil nil matched-text))))
    (when word
      (delete-region beg end)
      (insert word))))

(defun* ein2:completer-complete
    (kernel &rest args &key callbacks &allow-other-keys)
  "Start completion for the code at point.

.. It sends `:complete_request' to KERNEL.
   CALLBACKS is passed to `ein2:kernel-complete'.

   If you specify CALLBACKS explicitly (i.e., you are not using
   `ein2:completer-finish-completing'), keyword argument will be
   ignored.  Otherwise, ARGS are passed as additional arguments
   to completer callback functions.  ARGS **must** be keyword
   arguments.

   EXPAND keyword argument is supported by
   `ein2:completer-finish-completing-ac'.  When it is specified,
   it overrides `ac-expand-on-auto-complete' when calling
   `auto-complete'."
  (interactive (list (ein2:get-kernel)))
  (unless callbacks
    (setq callbacks
          (list :complete_reply
                (cons #'ein2:completer-finish-completing
                      (ein2:plist-exclude args '(:callbacks))))))
  (ein2:kernel-complete kernel
                       (thing-at-point 'line)
                       (current-column)
                       callbacks))

(defun ein2:completer-dot-complete ()
  "Insert dot and request completion."
  (interactive)
  (insert ".")
  (ein2:and-let* ((kernel (ein2:get-kernel))
                 ((not (ac-cursor-on-diable-face-p)))
                 ((ein2:kernel-live-p kernel)))
    (ein2:completer-complete kernel :expand nil)))

(defcustom ein2:complete-on-dot t
  "Start completion when inserting a dot.  Note that
`ein2:use-auto-complete' (or `ein2:use-auto-complete-superpack')
must be `t' to enable this option.  This variable has effect on
notebook buffers and connected buffers."
  :type 'boolean
  :group 'ein)

(defun ein2:complete-on-dot-install (map &optional func)
  (if (and ein2:complete-on-dot
           (featurep 'auto-complete)
           (or ein2:use-auto-complete
               ein2:use-auto-complete-superpack))
      (define-key map "." (or func #'ein2:completer-dot-complete))
    (define-key map "." nil)))

(provide 'ein-completer)

;;; ein-completer.el ends here
