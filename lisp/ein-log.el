;;; ein-log.el --- Logging module for ein.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-log.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-log.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-log.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ein-core)


(defvar ein2:log-all-buffer-name "*ein2:log-all*")

(defvar ein2:log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")
;; Some names are stolen from supervisord (http://supervisord.org/logging.html)

(defvar ein2:log-level 30)
(defvar ein2:log-message-level 20)

(defvar ein2:log-print-level 1 "`print-level' for `ein2:log'")
(defvar ein2:log-print-length 10 "`print-length' for `ein2:log'")
(defvar ein2:log-max-string 1000)


(defun ein2:log-set-level (level)
  (setq ein2:log-level (ein2:log-level-name-to-int level)))

(defun ein2:log-set-message-level (level)
  (setq ein2:log-message-level (ein2:log-level-name-to-int level)))

(defun ein2:log-level-int-to-name (int)
  (loop for (n . i) in ein2:log-level-def
        when (>= int i)
        return n
        finally 'error))

(defun ein2:log-level-name-to-int (name)
  (cdr (assq name ein2:log-level-def)))

(defun ein2:log-wrapper (level func)
  (setq level (ein2:log-level-name-to-int level))
  (when (<= level ein2:log-level)
    (let* ((levname (ein2:log-level-int-to-name level))
           (print-level ein2:log-print-level)
           (print-length ein2:log-print-length)
           (msg (format "[%s] %s"  levname (funcall func)))
           (orig-buffer (current-buffer)))
      (if (and ein2:log-max-string
               (> (length msg) ein2:log-max-string))
          (setq msg (substring msg 0 ein2:log-max-string)))
      (ein2:with-read-only-buffer (get-buffer-create ein2:log-all-buffer-name)
        (goto-char (point-max))
        (insert msg (format " @%S" orig-buffer) "\n"))
      (when (<= level ein2:log-message-level)
        (message "ein2: %s" msg)))))

(defmacro ein2:log (level string &rest args)
  (declare (indent 1))
  `(ein2:log-wrapper ,level (lambda () (format ,string ,@args))))

;; FIXME: this variable must go to somewhere more central
(defvar ein2:debug nil
  "Set to non-`nil' to raise errors instead of suppressing it.
Change the behavior of `ein2:log-ignore-errors'.")

(defmacro ein2:log-ignore-errors (&rest body)
  "Execute BODY; if an error occurs, log the error and return nil.
Otherwise, return result of last form in BODY."
  (declare (debug t) (indent 0))
  `(if ein2:debug
       (progn ,@body)
     (condition-case err
         (progn ,@body)
       (error
        (ein2:log 'debug "Error: %S" err)
        (ein2:log 'error (error-message-string err))
        nil))))

(defun ein2:log-pop-to-all-buffer ()
  (interactive)
  (pop-to-buffer (get-buffer-create ein2:log-all-buffer-name)))

(provide 'ein-log)

;;; ein-log.el ends here
