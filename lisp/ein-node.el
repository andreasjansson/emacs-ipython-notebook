;;; ein-node.el --- Structure to hold data in ewoc node

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-node.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-node.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-node.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ewoc)

(require 'ein-core)


(defstruct ein2:$node
  path                                  ; list of path
  data                                  ; actual data
  class                                 ; list
  )

(defun ein2:node-new (path data &optional class &rest args)
  (apply #'make-ein2:$node :path path :data data :class class args))

(defun ein2:node-add-class (node &rest classes)
  (mapc (lambda (c) (add-to-list (ein2:$node-class node) c)) classes))

(defun ein2:node-remove-class (node &rest classes)
  (let ((node-class (ein2:$node-class node)))
    (mapc (lambda (c) (setq node-class (delq c node-class))) classes)
    (setf (ein2:$node-class node) node-class)))

(defun ein2:node-has-class (node class)
  (memq class (ein2:$node-class node)))

(defun ein2:node-filter (ewoc-node-list &rest args)
  (loop for (key . class) in (ein2:plist-iter args)
        do (setq ewoc-node-list
                 (loop for ewoc-node in ewoc-node-list
                       for node = (ewoc-data ewoc-node)
                       when (case key
                              (:is (ein2:node-has-class node class))
                              (:not (not (ein2:node-has-class node class)))
                              (t (error "%s is not supported" key)))
                       collect ewoc-node)))
  ewoc-node-list)

(provide 'ein-node)

;;; ein-node.el ends here
