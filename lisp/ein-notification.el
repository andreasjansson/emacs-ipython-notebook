;;; ein-notification.el --- Notification widget for Notebook

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notification.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notification.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notification.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)

(require 'ein-core)
(require 'ein-events)


;; Class and variable

(ein2:deflocal ein2:%notification% nil
  "Buffer local variable to hold an instance of `ein2:notification'.")
(define-obsolete-variable-alias 'ein2:@notification 'ein2:%notification% "0.1.2")

(defvar ein2:header-line-format '(:eval (ein2:header-line)))
(defvar ein2:header-line-tab-map (make-sparse-keymap))
(defvar ein2:header-line-insert-tab-map (make-sparse-keymap))
(defvar ein2:header-line-tab-help
  "\
mouse-1 (left click) : switch to this tab
mouse-3 (right click) : pop to this tab
mouse-2 (middle click) : delete this tab
M-mouse-1/3 (Alt + left/right click): insert new tab to left/right
S-mouse-1/3 (Shift + left/right click): move this tab to left/right"
  "Help message.")
;; Note: can't put this below of `ein2:notification-setup'...

(defclass ein2:notification-status ()
  ((status :initarg :status :initform nil)
   (message :initarg :message :initform nil)
   (s2m :initarg :s2m))
  "Hold status and it's string representation (message).")

(defclass ein2:notification-tab ()
  ((get-list :initarg :get-list :type function)
   (get-current :initarg :get-current :type function)
   (get-name :initarg :get-name :type function)
   (get-buffer :initarg :get-buffer :type function)
   (delete :initarg :delete :type function)
   (insert-prev :initarg :insert-prev :type function)
   (insert-next :initarg :insert-next :type function)
   (move-prev :initarg :move-prev :type function)
   (move-next :initarg :move-next :type function)
   )
  ;; These "methods" are for not depending on what the TABs for.
  ;; Probably I'd want change this to be a separated Emacs lisp
  ;; library at some point.
  "See `ein2:notification-setup' for explanation.")

(defclass ein2:notification ()
  ((buffer :initarg :buffer :type buffer :document "Notebook buffer")
   (tab :initarg :tab :type ein2:notification-tab)
   (execution-count
    :initform "y" :initarg :execution-count
    :documentation "Last `execution_count' sent by `execute_reply'.")
   (notebook
    :initarg :notebook
    :initform
    (ein2:notification-status
     "NotebookStatus"
     :s2m
     '((notebook_saving.Notebook       . "Saving Notebook...")
       (notebook_saved.Notebook        . "Notebook is saved")
       (notebook_save_failed.Notebook  . "Failed to save Notebook!")))
    :type ein2:notification-status)
   (kernel
    :initarg :kernel
    :initform
    (ein2:notification-status
     "KernelStatus"
     :s2m
     '((status_idle.Kernel . nil)
       (status_busy.Kernel . "Kernel is busy...")
       (status_dead.Kernel . "Kernel is dead. Need restart.")))
    :type ein2:notification-status))
  "Notification widget for Notebook.")

(defmethod ein2:notification-status-set ((ns ein2:notification-status) status)
  (let* ((message (cdr (assoc status (oref ns :s2m)))))
    (oset ns :status status)
    (oset ns :message message)))

(defmethod ein2:notification-bind-events ((notification ein2:notification)
                                         events)
  "Bind a callback to events of the event handler EVENTS which
just set the status \(= event-type):
    \(ein2:notification-status-set NS EVENT-TYPE)
where NS is `:kernel' or `:notebook' slot of NOTIFICATION."
  (loop for ns in (list (oref notification :kernel)
                        (oref notification :notebook))
        for statuses = (mapcar #'car (oref ns :s2m))
        do (loop for st in statuses
                 do (ein2:events-on events
                                   st   ; = event-type
                                   #'ein2:notification--callback
                                   (cons ns st))))
  (ein2:events-on events
                 'notebook_saved.Notebook
                 #'ein2:notification--fadeout-callback
                 (list (oref notification :notebook)
                       "Notebook is saved"
                       'notebook_saved.Notebook
                       nil))
  (ein2:events-on events
                 'execution_count.Kernel
                 #'ein2:notification--set-execution-count
                 notification)
  (ein2:events-on events
                 'status_restarting.Kernel
                 #'ein2:notification--fadeout-callback
                 (list (oref notification :kernel)
                       "Restarting kernel..."
                       'status_restarting.Kernel
                       'status_idle.Kernel)))

(defun ein2:notification--callback (packed data)
  (let ((ns (car packed))
        (status (cdr packed)))
    (ein2:notification-status-set ns status)))

(defun ein2:notification--set-execution-count (notification count)
  (oset notification :execution-count count))

(defun ein2:notification--fadeout-callback (packed data)
  ;; FIXME: I can simplify this.
  ;;        Do not pass around message, for exmaple.
  (let ((ns (nth 0 packed))
        (message (nth 1 packed))
        (status (nth 2 packed))
        (next (nth 3 packed)))
    (oset ns :status status)
    (oset ns :message message)
    (apply #'run-at-time
           1 nil
           (lambda (ns message status next)
             (when (equal (oref ns :status) status)
               (ein2:notification-status-set ns next)
               ;; (ein2:with-live-buffer (oref ns :buffer)
               ;;   (force-mode-line-update))
               ))
           packed)))

(defun ein2:notification-setup (buffer events &rest tab-slots)
  "Setup a new notification widget in the BUFFER.
This function saves the new notification widget instance in the
local variable of the BUFFER.

Rest of the arguments are for TABs in `header-line'.

GET-LIST : function
  Return a list of worksheets.

GET-CURRENT : function
  Return the current worksheet.

GET-NAME : function
  Return a name of the worksheet given as its argument.

GET-BUFFER : function
  Get a buffer of given worksheet.  Render it if needed.

DELETE : function
  Remove a given worksheet.

INSERT-PREV / INSERT-NEXT : function
  Insert new worksheet before/after the specified worksheet.

MOVE-PREV / MOVE-NEXT : function
  Switch this worksheet to the previous/next one.

\(fn buffer events &key get-list get-current get-name get-buffer delete \
insert-prev insert-next move-prev move-next)"
  (with-current-buffer buffer
    (setq ein2:%notification%
          (ein2:notification "NotificationWidget" :buffer buffer))
    (setq header-line-format ein2:header-line-format)
    (ein2:notification-bind-events ein2:%notification% events)
    (oset ein2:%notification% :tab
          (apply #'make-instance 'ein2:notification-tab tab-slots))
    ein2:%notification%))


;;; Tabs

(defface ein2:notification-tab-selected
  '((t :inherit (header-line match) :underline t))
  "Face for headline selected tab."
  :group 'ein)

(defface ein2:notification-tab-normal
  '((t :inherit (header-line) :underline t :height 0.8))
  "Face for headline selected tab."
  :group 'ein)

(defmethod ein2:notification-tab-create-line ((tab ein2:notification-tab))
  (let ((list (funcall (oref tab :get-list)))
        (current (funcall (oref tab :get-current)))
        (get-name (oref tab :get-name)))
    (ein2:join-str
     " "
     (append
      (loop for i from 1
            for elem in list
            if (eq elem current)
            collect (propertize
                     (or (ein2:and-let* ((name (funcall get-name elem)))
                           (format "/%d: %s\\" i name))
                         (format "/%d\\" i))
                     'ein2:worksheet elem
                     'keymap ein2:header-line-tab-map
                     'help-echo ein2:header-line-tab-help
                     'mouse-face 'highlight
                     'face 'ein2:notification-tab-selected)
            else
            collect (propertize
                     (format "/%d\\" i)
                     'ein2:worksheet elem
                     'keymap ein2:header-line-tab-map
                     'help-echo ein2:header-line-tab-help
                     'mouse-face 'highlight
                     'face 'ein2:notification-tab-normal))
      (list
       (propertize "[+]"
                   'keymap ein2:header-line-insert-tab-map
                   'help-echo "Click (mouse-1) to insert a new tab."
                   'mouse-face 'highlight
                   'face 'ein2:notification-tab-normal))))))


;;; Header line

(let ((map ein2:header-line-tab-map))
  (define-key map [header-line M-mouse-1] 'ein2:header-line-insert-prev-tab)
  (define-key map [header-line M-mouse-3] 'ein2:header-line-insert-next-tab)
  (define-key map [header-line S-mouse-1] 'ein2:header-line-move-prev-tab)
  (define-key map [header-line S-mouse-3] 'ein2:header-line-move-next-tab)
  (define-key map [header-line mouse-1] 'ein2:header-line-switch-to-this-tab)
  (define-key map [header-line mouse-2] 'ein2:header-line-delete-this-tab)
  (define-key map [header-line mouse-3] 'ein2:header-line-pop-to-this-tab))

(define-key ein2:header-line-insert-tab-map
  [header-line mouse-1] 'ein2:header-line-insert-new-tab)

(defmacro ein2:with-destructuring-bind-key-event (key-event &rest body)
  (declare (debug (form &rest form))
           (indent 1))
  ;; See: (info "(elisp) Click Events")
  `(destructuring-bind
       (event-type
        (window pos-or-area (x . y) timestamp
                object text-pos (col . row)
                image (dx . dy) (width . height)))
       ,key-event
     ,@body))

(defun ein2:header-line-select-window (key-event)
  (ein2:with-destructuring-bind-key-event key-event (select-window window)))

(defun ein2:header-line-key-event-get-worksheet (key-event)
  (ein2:with-destructuring-bind-key-event key-event
    (get-char-property (cdr object) 'ein2:worksheet (car object))))

(defun ein2:header-line-key-event-get-buffer (key-event)
  (funcall (oref (oref ein2:%notification% :tab) :get-buffer)
           (ein2:header-line-key-event-get-worksheet key-event)))

(defun ein2:header-line-switch-to-this-tab (key-event)
  (interactive "e")
  (ein2:header-line-select-window key-event)
  (switch-to-buffer (ein2:header-line-key-event-get-buffer key-event)))

(defun ein2:header-line-pop-to-this-tab (key-event)
  (interactive "e")
  (ein2:header-line-select-window key-event)
  (pop-to-buffer (ein2:header-line-key-event-get-buffer key-event)))

(defun ein2:header-line-do-slot-function (key-event slot)
  "Call SLOT function on worksheet instance fetched from KEY-EVENT."
  (ein2:header-line-select-window key-event)
  (funcall (slot-value (oref ein2:%notification% :tab) slot)
           (ein2:header-line-key-event-get-worksheet key-event)))

(defmacro ein2:header-line-define-mouse-commands (&rest name-slot-list)
  `(progn
     ,@(loop for (name slot) on name-slot-list by 'cddr
             collect
             `(defun ,name (key-event)
                ,(format "Run slot %s
Generated by `ein2:header-line-define-mouse-commands'" slot)
                (interactive "e")
                (ein2:header-line-do-slot-function key-event ,slot)))))

(ein2:header-line-define-mouse-commands
 ein2:header-line-delete-this-tab :delete
 ein2:header-line-insert-prev-tab :insert-prev
 ein2:header-line-insert-next-tab :insert-next
 ein2:header-line-move-prev-tab :move-prev
 ein2:header-line-move-next-tab :move-next
 )

(defun ein2:header-line-insert-new-tab (key-event)
  "Insert new tab."
  (interactive "e")
  (ein2:header-line-select-window key-event)
  (let ((notification (oref ein2:%notification% :tab)))
    (funcall (oref notification :insert-next)
             (car (last (funcall (oref notification :get-list)))))))

(defun ein2:header-line ()
  (format
   "IP[%s]: %s"
   (oref ein2:%notification% :execution-count)
   (ein2:join-str
    " | "
    (ein2:filter
     'identity
     (list (oref (oref ein2:%notification% :notebook) :message)
           (oref (oref ein2:%notification% :kernel) :message)
           (ein2:notification-tab-create-line
            (oref ein2:%notification% :tab)))))))

(defun ein2:header-line-setup-maybe ()
  "Setup `header-line-format' for mumamo.
As `header-line-format' is buffer local variable, it must be set
for each chunk when in
See also `ein2:ac-setup-maybe'."
  (and (ein2:eval-if-bound 'ein2:%notebook%)
       (ein2:eval-if-bound 'mumamo-multi-major-mode)
       (setq header-line-format ein2:header-line-format)))
(add-hook 'after-change-major-mode-hook 'ein2:header-line-setup-maybe)

(provide 'ein-notification)

;;; ein-notification.el ends here
