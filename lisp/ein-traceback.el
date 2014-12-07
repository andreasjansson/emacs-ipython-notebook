;;; ein-traceback.el --- Traceback module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-traceback.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-traceback.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-traceback.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)
(require 'ansi-color)

(require 'ein-core)

(defclass ein2:traceback ()
  ((tb-data :initarg :tb-data :type list)
   (buffer-name :initarg :buffer-name :type string)
   (buffer :initarg :buffer :type buffer)
   (ewoc :initarg :ewoc :type ewoc)))

(ein2:deflocal ein2:%traceback% nil
  "Buffer local variable to store an instance of `ein2:traceback'.")

(defvar ein2:tb-buffer-name-template "*ein2:tb %s/%s*")

(defun ein2:tb-new (buffer-name)
  (ein2:traceback "Traceback" :buffer-name buffer-name))

(defmethod ein2:tb-get-buffer ((traceback ein2:traceback))
  (unless (and (slot-boundp traceback :buffer)
               (buffer-live-p (oref traceback :buffer)))
    (let ((buf (get-buffer-create (oref traceback :buffer-name))))
      (oset traceback :buffer buf)))
  (oref traceback :buffer))

(defun ein2:tb-pp (ewoc-data)
  (insert (ansi-color-apply ewoc-data)))

(defmethod ein2:tb-render ((traceback ein2:traceback) tb-data)
  (with-current-buffer (ein2:tb-get-buffer traceback)
    (setq ein2:%traceback% traceback)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t)
          (ewoc (ein2:ewoc-create #'ein2:tb-pp)))
      (erase-buffer)
      (oset traceback :ewoc ewoc)
      (oset traceback :tb-data tb-data)
      (mapc (lambda (data) (ewoc-enter-last ewoc data)) tb-data))
    (ein2:traceback-mode)))

(defmethod ein2:tb-popup ((traceback ein2:traceback) tb-data)
  (ein2:tb-render traceback tb-data)
  (pop-to-buffer (ein2:tb-get-buffer traceback)))

;;;###autoload
(defun ein2:tb-show ()
  "Show full traceback in traceback viewer."
  (interactive)
  (unless
      (ein2:and-let* ((tb-data (ein2:get-traceback-data))
                     (url-or-port (ein2:get-url-or-port))
                     (kernel (ein2:get-kernel))
                     (kr-id (ein2:kernel-id kernel))
                     (tb-name (format ein2:tb-buffer-name-template
                                      url-or-port kr-id)))
        (ein2:tb-popup (ein2:tb-new tb-name) tb-data)
        t)
    (error "No traceback is available.")))

(defmethod ein2:tb-range-of-node-at-point ((traceback ein2:traceback))
  (let* ((ewoc (oref traceback :ewoc))
         (ewoc-node (ewoc-locate ewoc))
         (beg (ewoc-location ewoc-node))
         (end (ein2:aand (ewoc-next ewoc ewoc-node) (ewoc-location it))))
    (list beg end)))

(defmethod ein2:tb-file-path-at-point ((traceback ein2:traceback))
  (destructuring-bind (beg end)
      (ein2:tb-range-of-node-at-point traceback)
    (let* ((file-tail
            (if (>= emacs-major-version 24)
                (next-single-property-change beg 'font-lock-face nil end)
              ;; For Emacs 23.x:
              (next-single-property-change beg 'face nil end)))
           (file (when file-tail
                   (buffer-substring-no-properties beg file-tail))))
      (if (string-match "\\.pyc$" file)
          (concat (file-name-sans-extension file) ".py")
        file))))

(defmethod ein2:tb-file-lineno-at-point ((traceback ein2:traceback))
  (destructuring-bind (beg end)
      (ein2:tb-range-of-node-at-point traceback)
    (when (save-excursion
            (goto-char beg)
            (search-forward-regexp "^[-]+> \\([0-9]+\\)" end t))
      (string-to-number (match-string 1)))))

(defmethod ein2:tb-jump-to-source-at-point ((traceback ein2:traceback)
                                           &optional select)
  (let ((file (ein2:tb-file-path-at-point traceback))
        (lineno (ein2:tb-file-lineno-at-point traceback)))
    (assert (file-exists-p file) nil "File %s does not exist." file)
    (let ((buf (find-file-noselect file))
          (scroll (lambda ()
                    (goto-char (point-min))
                    (forward-line (1- lineno)))))
      (if select
          (progn (pop-to-buffer buf)
                 (funcall scroll))
        (with-selected-window (display-buffer buf)
          (funcall scroll))))))

(defun ein2:tb-jump-to-source-at-point-command (&optional select)
  (interactive "P")
  (ein2:tb-jump-to-source-at-point ein2:%traceback% select))


;;; ein2:traceback-mode

(defun ein2:tb-prev-item ()
  (interactive)
  (ewoc-goto-prev (oref ein2:%traceback% :ewoc) 1))

(defun ein2:tb-next-item ()
  (interactive)
  (ewoc-goto-next (oref ein2:%traceback% :ewoc) 1))

(define-derived-mode ein2:traceback-mode fundamental-mode "ein2:tb"
  (font-lock-mode))

(add-hook 'ein2:traceback-mode-hook 'ein2:truncate-lines-on)

(let ((map ein2:traceback-mode-map))
  (define-key map (kbd "RET") 'ein2:tb-jump-to-source-at-point-command)
  (define-key map "p" 'ein2:tb-prev-item)
  (define-key map "n" 'ein2:tb-next-item)
  (define-key map "q" 'bury-buffer))

(provide 'ein-traceback)

;;; ein-traceback.el ends here
