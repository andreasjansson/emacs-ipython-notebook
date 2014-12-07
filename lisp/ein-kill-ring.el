;;; ein-kill-ring.el --- Kill-ring for cells

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-kill-ring.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-kill-ring.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kill-ring.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stolen from simple.el.

;;; Code:

(defvar ein2:kill-ring nil)
(defvar ein2:kill-ring-yank-pointer nil)
(defvar ein2:kill-ring-max kill-ring-max)

(defun ein2:kill-new (obj)
  "Make OBJ the latest kill in the kill ring `ein2:kill-ring'.
Set `ein2:kill-ring-yank-pointer' to point to it."
  (push obj ein2:kill-ring)
  (if (> (length ein2:kill-ring) ein2:kill-ring-max)
      (setcdr (nthcdr (1- ein2:kill-ring-max) ein2:kill-ring) nil))
  (setq ein2:kill-ring-yank-pointer ein2:kill-ring))

(defun ein2:current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually
move the yanking point; just return the Nth kill forward."
  (unless ein2:kill-ring (error "Kill ring is empty"))
  (let ((ARGth-kill-element
         (nthcdr (mod (- n (length ein2:kill-ring-yank-pointer))
                      (length ein2:kill-ring))
                 ein2:kill-ring)))
    (unless do-not-move
      (setq ein2:kill-ring-yank-pointer ARGth-kill-element))
    (car ARGth-kill-element)))

(provide 'ein-kill-ring)

;;; ein-kill-ring.el ends here
