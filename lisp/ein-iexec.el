;;; ein-iexec.el --- Instant execution mode for notebook

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-iexec.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-iexec.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-iexec.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ein-worksheet)

(defcustom ein2:iexec-delay 0.3
  "Delay before executing cell after change in second."
  :type 'number
  :group 'ein)

(defvar ein2:iexec-timer nil)

(defun ein2:iexec-execute-cell (cell)
  "Call `ein2:notebook-execute-cell' after `ein2:iexec-delay' second.
If the previous execution timer is not fired yet, cancel the timer."
  (when ein2:iexec-timer
    (cancel-timer ein2:iexec-timer))
  (setq ein2:iexec-timer
        (run-with-idle-timer ein2:iexec-delay nil
                             #'ein2:worksheet-execute-cell
                             ein2:%worksheet% cell)))

(defun ein2:iexec-should-execute-p (cell beg end)
  "Return non-`nil' if CELL should be executed by the change within
BEG and END."
  (and (ein2:codecell-p cell)
       this-command
       (ein2:aif (ein2:cell-input-pos-min cell) (<= it beg))
       (ein2:aif (ein2:cell-input-pos-max cell) (>= it end))))

(defun ein2:iexec-after-change (beg end -ignore-len-)
  "Called via `after-change-functions' hook."
  (let ((cell (ein2:worksheet-get-current-cell :pos beg)))
    (when (ein2:iexec-should-execute-p cell beg end)
      (ein2:iexec-execute-cell cell))))

;;;###autoload
(define-minor-mode ein2:iexec-mode
  "Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area."
  :lighter " ein2:i"
  :group 'ein
  (if ein2:iexec-mode
      (add-hook 'after-change-functions 'ein2:iexec-after-change nil t)
    (remove-hook 'after-change-functions 'ein2:iexec-after-change t)))

;; To avoid MuMaMo to discard `ein2:iexec-after-change', make it
;; permanent local.
(put 'ein2:iexec-after-change 'permanent-local-hook t)
(put 'ein2:iexec-mode 'permanent-local t)

(provide 'ein-iexec)

;;; ein-iexec.el ends here
