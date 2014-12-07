;;; ein-notebooklist.el --- Notebook list buffer

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notebooklist.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notebooklist.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebooklist.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'widget)

(require 'ein-core)
(require 'ein-notebook)
(require 'ein-subpackages)

(defcustom ein2:notebooklist-first-open-hook nil
  "Hooks to run when the notebook list is opened at first time.

Example to open a notebook named _scratch_ when the notebook list
is opened at first time.::

  (add-hook
   'ein2:notebooklist-first-open-hook
   (lambda () (ein2:notebooklist-open-notebook-by-name \"_scratch_\")))

"
  :type 'hook
  :group 'ein)

(defstruct ein2:$notebooklist
  "Hold notebooklist variables.

`ein2:$notebooklist-url-or-port'
  URL or port of IPython server.

`ein2:$notbooklist-path'
  The path for the notebooklist.

`ein2:$notebooklist-data'
  JSON data sent from the server.
`ein2:$notebooklist-api-version'
  Major version of the IPython notebook server we are talking to."
  url-or-port
  path
  data
  api-version)

(ein2:deflocal ein2:%notebooklist% nil
  "Buffer local variable to store an instance of `ein2:$notebooklist'.")
(define-obsolete-variable-alias 'ein2:notebooklist 'ein2:%notebooklist% "0.1.2")

(defvar ein2:notebooklist-buffer-name-template "*ein2:notebooklist %s*")

(defvar ein2:notebooklist-map (make-hash-table :test 'equal)
  "Data store for `ein2:notebooklist-list'.
Mapping from URL-OR-PORT to an instance of `ein2:$notebooklist'.")

(defun ein2:notebooklist-list ()
  "Get a list of opened `ein2:$notebooklist'."
  (ein2:hash-vals ein2:notebooklist-map))

(defun ein2:notebooklist-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ein2:notebooklist-map'."
  (puthash (ein2:$notebooklist-url-or-port nblist)
           nblist
           ein2:notebooklist-map))

(defun ein2:notebooklist-list-get (url-or-port)
  "Get an instance of `ein2:$notebooklist' by URL-OR-PORT as a key."
  (gethash url-or-port ein2:notebooklist-map))

(defun ein2:notebooklist-open-notebook-by-name (name &optional url-or-port
                                                    callback cbargs)
  "Open notebook named NAME in the server URL-OR-PORT.
If URL-OR-PORT is not given or `nil', and the current buffer is
the notebook list buffer, the notebook is searched in the
notebook list of the current buffer.

When used in lisp, CALLBACK and CBARGS are passed to `ein2:notebook-open'.
To suppress popup, you can pass a function `ein2:do-nothing' as CALLBACK."
  (loop with nblist = (if url-or-port
                          (ein2:notebooklist-list-get url-or-port)
                        ein2:%notebooklist%)
        for note in (ein2:$notebooklist-data nblist)
        for notebook-name = (plist-get note :name)
        for notebook-path = (plist-get note :path)
        for notebook-id = notebook-name ;(plist-get note :notebook_id)
        when (equal notebook-name name)
        return (ein2:notebook-open (ein2:$notebooklist-url-or-port nblist)
                                  (ein2:$notebooklist-api-version nblist)
                                  notebook-id notebook-path callback cbargs)))

(defun ein2:notebooklist-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((= version 3) "api/contents"))))
    (if path
        (ein2:url url-or-port base-path (or path ""))
      (ein2:url url-or-port base-path))))

(defun ein2:notebooklist-new-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((= version 3) "api/contents"))))
    (ein2:log 'info "New notebook. Port: %s, Path: %s" url-or-port path)
    (if (and path (not (string= path "")))
        (ein2:url url-or-port base-path path)
      (ein2:url url-or-port base-path))))

(defun ein2:notebooklist-get-buffer (url-or-port)
  (get-buffer-create
   (format ein2:notebooklist-buffer-name-template url-or-port)))

(defun ein2:notebooklist-ask-url-or-port ()
  (let* ((url-or-port-list (mapcar (lambda (x) (format "%s" x))
                                   ein2:url-or-port))
         (default (format "%s" (ein2:aif (ein2:get-notebook)
                                   (ein2:$notebook-url-or-port it)
                                 (ein2:aif ein2:%notebooklist%
                                     (ein2:$notebooklist-url-or-port it)
                                   (ein2:default-url-or-port)))))
         (url-or-port
          (completing-read (format "URL or port number (default %s): " default)
                           url-or-port-list
                           nil nil nil nil
                           default)))
    (if (string-match "^[0-9]+$" url-or-port)
        (string-to-number url-or-port)
      url-or-port)))

;;;###autoload
(defun ein2:notebooklist-open (&optional url-or-port path no-popup)
  "Open notebook list buffer."
  (interactive (list (ein2:notebooklist-ask-url-or-port)))
  (unless url-or-port (setq url-or-port (ein2:default-url-or-port)))
  (unless path (setq path ""))
  (ein2:subpackages-load)
  (let ((api-version (ein2:query-ipython-version url-or-port))
        (success
         (if no-popup
             #'ein2:notebooklist-url-retrieve-callback
           (lambda (&rest args)
             (pop-to-buffer
              (apply #'ein2:notebooklist-url-retrieve-callback args))))))
    (ein2:query-singleton-ajax
     (list 'notebooklist-open url-or-port api-version path)
     (ein2:notebooklist-url url-or-port api-version path)
     :parser #'ein2:json-read
     :error (apply-partially #'ein2:notebooklist-open-error url-or-port api-version path)
     :success (apply-partially success url-or-port api-version path)))
  (ein2:notebooklist-get-buffer url-or-port))

(defun* ein2:notebooklist-url-retrieve-callback (url-or-port
                                                api-version
                                                path
                                                &key
                                                data
                                                &allow-other-keys)
  "Called via `ein2:notebooklist-open'."
  (with-current-buffer (ein2:notebooklist-get-buffer url-or-port)
    (let ((already-opened-p (ein2:notebooklist-list-get url-or-port))
          (orig-point (point)))
      (setq ein2:%notebooklist%
            (make-ein2:$notebooklist :url-or-port url-or-port
                                    :path path
                                    :data data
                                    :api-version api-version))
      (ein2:notebooklist-list-add ein2:%notebooklist%)
      (ein2:notebooklist-render)
      (goto-char orig-point)
      (ein2:log 'info "Opened notebook list at %s with path %s" url-or-port path)
      (unless already-opened-p
        (run-hooks 'ein2:notebooklist-first-open-hook))
      (current-buffer))))

(defun* ein2:notebooklist-open-error (url-or-port path
                                     &key symbol-status response
                                     &allow-other-keys)
  (ein2:log 'verbose
    "Error thrown: %S" (request-response-error-thrown response))
  (ein2:log 'error
    "Error (%s) while opening notebook list with path %s at the server %s."
    symbol-status path url-or-port))

;;;###autoload
(defun ein2:notebooklist-reload ()
  "Reload current Notebook list."
  (interactive)
  (ein2:notebooklist-open (ein2:$notebooklist-url-or-port ein2:%notebooklist%)
                         (ein2:$notebooklist-path ein2:%notebooklist%) t))

(defun ein2:notebooklist-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ein2:notebook-after-rename-hook'."
  (ein2:notebooklist-open (ein2:$notebook-url-or-port ein2:%notebook%) t))

(add-hook 'ein2:notebook-after-rename-hook 'ein2:notebooklist-refresh-related)

(defun ein2:notebooklist-open-notebook (nblist name path &optional
                                              callback cbargs)
  (ein2:notebook-open (ein2:$notebooklist-url-or-port nblist)
                     (ein2:$notebooklist-api-version nblist)
                     name path
                     callback cbargs))

;;;###autoload
(defun ein2:notebooklist-new-notebook (&optional url-or-port path callback cbargs)
  "Ask server to create a new notebook and open it in a new buffer."
  (interactive (list (ein2:notebooklist-ask-url-or-port)))
  (let ((path (or path (ein2:$notebooklist-path (or ein2:%notebooklist%
                                                   (ein2:notebooklist-list-get url-or-port)))))
        (version (ein2:$notebooklist-api-version (or ein2:%notebooklist%
                                                    (ein2:notebooklist-list-get url-or-port)))))
    (ein2:log 'info "Creating a new notebook at %s..." path)
    (unless url-or-port
      (setq url-or-port (ein2:$notebooklist-url-or-port ein2:%notebooklist%)))
    (assert url-or-port nil
            (concat "URL-OR-PORT is not given and the current buffer "
                    "is not the notebook list buffer."))
    (let ((url (ein2:notebooklist-new-url url-or-port
                                         version
                                         path)))
      (ein2:query-singleton-ajax
       (list 'notebooklist-new-notebook url-or-port path)
       url
       :type "POST"
       :parser #'ein2:json-read
       ;; (lambda ()
       ;;   (ein2:html-get-data-in-body-tag "data-notebook-id"))
       :error (apply-partially #'ein2:notebooklist-new-notebook-error
                               url-or-port path callback cbargs)
       :success (apply-partially #'ein2:notebooklist-new-notebook-callback
                                 url-or-port path callback cbargs)))))

(defun* ein2:notebooklist-new-notebook-callback (url-or-port
                                                path
                                                callback
                                                cbargs
                                                &key
                                                data
                                                &allow-other-keys
                                                &aux
                                                (no-popup t))
  (ein2:log 'info "Creating a new notebook... Done.")
  (if data
      (let ((name (plist-get data :name))
            (path (plist-get data :path)))
        (ein2:notebook-open url-or-port (ein2:query-ipython-version url-or-port) name path callback cbargs))
    (ein2:log 'info (concat "Oops. EIN failed to open new notebook. "
                           "Please find it in the notebook list."))
    (setq no-popup nil))
  ;; reload or open notebook list
  (ein2:notebooklist-open url-or-port path no-popup))

(defun* ein2:notebooklist-new-notebook-error
    (url-or-port callback cbargs
                 &key response &allow-other-keys
                 &aux
                 (no-popup t)
                 (error (request-response-error-thrown response))
                 (dest (request-response-url response)))
  (ein2:log 'verbose
    "NOTEBOOKLIST-NEW-NOTEBOOK-ERROR url-or-port: %S; error: %S; dest: %S"
    url-or-port error dest)
  (ein2:log 'error
    "Failed to open new notebook (error: %S). \
You may find the new one in the notebook list." error)
  (setq no-popup nil)
  (ein2:notebooklist-open url-or-port no-popup))

;;;###autoload
(defun ein2:notebooklist-new-notebook-with-name (name &optional url-or-port path)
  "Open new notebook and rename the notebook."
  (interactive (let* ((url-or-port (or (ein2:get-url-or-port)
                                       (ein2:default-url-or-port)))
                      (name (read-from-minibuffer
                             (format "Notebook name (at %s): " url-or-port))))
                 (list name url-or-port)))
  (let ((path (or path (ein2:$notebooklist-path ein2:%notebooklist%))))
    (ein2:notebooklist-new-notebook
     url-or-port
     path
     (lambda (notebook created name)
       (assert created)
       (with-current-buffer (ein2:notebook-buffer notebook)
         (ein2:notebook-rename-command name)
         ;; As `ein2:notebook-open' does not call `pop-to-buffer' when
         ;; callback is specified, `pop-to-buffer' must be called here:
         (pop-to-buffer (current-buffer))))
     (list name))))

(defun ein2:notebooklist-delete-notebook-ask (name path)
  (when (y-or-n-p (format "Delete notebook %s/%s?" path name))
    (ein2:notebooklist-delete-notebook name path)))

(defun ein2:notebooklist-delete-notebook (name path)
  (ein2:log 'info "Deleting notebook %s/%s..." path name)
  (ein2:query-singleton-ajax
   (list 'notebooklist-delete-notebook
         (ein2:$notebooklist-url-or-port ein2:%notebooklist%) name path)
   (ein2:notebook-url-from-url-and-id
    (ein2:$notebooklist-url-or-port ein2:%notebooklist%)
    (ein2:$notebooklist-api-version ein2:%notebooklist%)
    path
    name)
   :type "DELETE"
   :success (apply-partially (lambda (buffer name &rest ignore)
                               (ein2:log 'info
                                 "Deleting notebook %s... Done." name)
                               (with-current-buffer buffer
                                 (ein2:notebooklist-reload)))
                             (current-buffer) name)))

;; Because MinRK wants me to suffer...
(defun ein2:get-actual-path (path)
  (ein2:aif (cl-position ?/ path :from-end t)
      (substring path 0 it)
    ""))

(defun ein2:notebooklist-render ()
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ein2:notebooklist-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create notebook list
  (widget-insert "IPython Notebook list\n\n")
  (widget-insert " | ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein2:notebooklist-open
                                   (ein2:$notebooklist-url-or-port ein2:%notebooklist%)
                                   ""))
   "Home")
  (if (not (string= "" (ein2:$notebooklist-path ein2:%notebooklist%)))
      (widget-insert " | " (ein2:$notebooklist-path ein2:%notebooklist%)))
  (widget-insert " |\n")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein2:notebooklist-new-notebook))
   "New Notebook")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein2:notebooklist-reload))
   "Reload List")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore)
             (browse-url
              (ein2:url (ein2:$notebooklist-url-or-port ein2:%notebooklist%))))
   "Open In Browser")
  (widget-insert "\n")
  (let ((api-version (ein2:$notebooklist-api-version ein2:%notebooklist%)))
    (loop for note in (cond ((= 2 api-version)
                             (ein2:$notebooklist-data ein2:%notebooklist%))
                            ((= 3 api-version)
                             (plist-get (ein2:$notebooklist-data ein2:%notebooklist%) :content)))
          for urlport = (ein2:$notebooklist-url-or-port ein2:%notebooklist%)
          for name = (plist-get note :name)
          for path = (cond ((= 2 api-version)
                            (plist-get note :path))
                           ((= 3 api-version)
                            (ein2:get-actual-path (plist-get note :path))))
          for type = (plist-get note :type)
          for notebook-id = (plist-get note :notebook_id)
          if (string= type "directory")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((urlport urlport)
                                           (path name))
                               (lambda (&rest ignore)
                                 (ein2:notebooklist-open urlport
                                                        (ein2:url (ein2:$notebooklist-path ein2:%notebooklist%)
                                                                 path))))
                     "Dir")
                    (widget-insert " : " name)
                    (widget-insert "\n"))
          if (string= type "notebook")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((name name)
                                           (path path)
                                           (notebook-id notebook-id))
                               (lambda (&rest ignore)
                                 (ein2:notebooklist-open-notebook
                                  ein2:%notebooklist% name path)))
                     "Open")
                    (widget-insert " ")
                    (widget-create
                     'link
                     :notify (lexical-let ((name name)
                                           (path path))
                               (lambda (&rest ignore)
                                 (ein2:notebooklist-delete-notebook-ask
                                  name
                                  path)))
                     "Delete")
                    (widget-insert " : " name)
                    (widget-insert "\n"))))
  (ein2:notebooklist-mode)
  (widget-setup))

;;;###autoload
(defun ein2:notebooklist-list-notebooks ()
  "Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\"."
  (apply #'append
         (loop for nblist in (ein2:notebooklist-list)
               for url-or-port = (ein2:$notebooklist-url-or-port nblist)
               collect
               (loop for note in (ein2:$notebooklist-data nblist)
                     collect (format "%s/%s"
                                     url-or-port
                                     (plist-get note :name))))))

;;FIXME: Code below assumes notebook is in root directory - need to do a better
;;       job listing notebooks in subdirectories and parsing out the path.
;;;###autoload
(defun ein2:notebooklist-open-notebook-global (nbpath &optional callback cbargs)
  "Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein2:notebook-open'."
  (interactive
   (list (completing-read
          "Open notebook [URL-OR-PORT/NAME]: "
          (ein2:notebooklist-list-notebooks))))
  (let* ((path (split-string nbpath "/"))
         (url-or-port (car path))
         (name (cadr path)))
    (when (and (stringp url-or-port)
               (string-match "^[0-9]+$" url-or-port))
      (setq url-or-port (string-to-number url-or-port)))
    ;; (let ((notebook-id
    ;;        (loop for nblist in (ein2:notebooklist-list)
    ;;              when (equal (ein2:$notebooklist-url-or-port nblist) url-or-port)
    ;;              if (loop for note in (ein2:$notebooklist-data nblist)
    ;;                       when (equal (plist-get note :name) name)
    ;;                       return (plist-get note :name))
    ;;              return it)))
    ;;   (if notebook-id
    (ein2:notebook-open url-or-port (ein2:query-ipython-version url-or-port) name "/" callback cbargs)
    (ein2:log 'info "Notebook '%s' not found" nbpath)))

;;;###autoload
(defun ein2:notebooklist-load (&optional url-or-port)
  "Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein2:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein2:notebooklist-load)

You should setup `ein2:url-or-port' or `ein2:default-url-or-port'
in order to make this code work.

See also:
`ein2:connect-to-default-notebook', `ein2:connect-default-notebook'."
  (ein2:notebooklist-open url-or-port t))


(defun ein2:notebooklist-find-server-by-notebook-name (name)
  "Find a notebook named NAME and return a list (URL-OR-PORT NOTEBOOK-ID)."
  (loop named outer
        for nblist in (ein2:notebooklist-list)
        for url-or-port = (ein2:$notebooklist-url-or-port nblist)
        do (loop for note in (ein2:$notebooklist-data nblist)
                 when (equal (plist-get note :name) name)
                 do (return-from outer
                      (list url-or-port (plist-get note :notebook_id))))))

(defun ein2:notebooklist-open-notebook-by-file-name
  (&optional filename noerror buffer-callback)
  "Find the notebook named as same as the current file in the servers.
Open the notebook if found.  Note that this command will *not*
upload the current file to the server.

.. When FILENAME is unspecified the variable `buffer-file-name'
   is used instead.  Set NOERROR to non-`nil' to suppress errors.
   BUFFER-CALLBACK is called after opening notebook with the
   current buffer as the only one argument."
  (interactive (progn (assert buffer-file-name nil "Not visiting a file.")
                      nil))
  (unless filename (setq filename buffer-file-name))
  (assert filename nil "No file found.")
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory (or filename))))
         (found (ein2:notebooklist-find-server-by-notebook-name name))
         (callback (lambda (-ignore-1- -ignore-2- buffer buffer-callback)
                     (ein2:notebook-pop-to-current-buffer) ; default
                     (when (buffer-live-p buffer)
                       (funcall buffer-callback buffer))))
         (cbargs (list (current-buffer) (or buffer-callback #'ignore))))
    (unless noerror
      (assert found nil "No server has notebook named: %s" name))
    (destructuring-bind (url-or-port notebook-id) found
      (ein2:notebook-open url-or-port (ein2:query-ipython-version url-or-port) notebook-id callback cbargs))))

(defvar ein2:notebooklist-find-file-buffer-callback #'ignore)

(defun ein2:notebooklist-find-file-callback ()
  "A callback function for `find-file-hook' to open notebook.

FIMXE: document how to use `ein2:notebooklist-find-file-callback'
       when I am convinced with the API."
  (ein2:and-let* ((filename buffer-file-name)
                 ((string-match-p "\\.ipynb$" filename)))
    (ein2:notebooklist-open-notebook-by-file-name
     filename t ein2:notebooklist-find-file-buffer-callback)))


;;; Login

(defun ein2:notebooklist-login (url-or-port password)
  "Login to IPython notebook server."
  (interactive (list (ein2:notebooklist-ask-url-or-port)
                     (read-passwd "Password: ")))
  (ein2:log 'debug "NOTEBOOKLIST-LOGIN: %s" url-or-port)
  (ein2:query-singleton-ajax
   (list 'notebooklist-login url-or-port)
   (ein2:url url-or-port "login")
   :type "POST"
   :data (concat "password=" (url-hexify-string password))
   :parser #'ein2:notebooklist-login--parser
   :error (apply-partially #'ein2:notebooklist-login--error url-or-port)
   :success (apply-partially #'ein2:notebooklist-login--success url-or-port)))

(defun ein2:notebooklist-login--parser ()
  (goto-char (point-min))
  (list :bad-page (re-search-forward "<input type=.?password" nil t)))

(defun ein2:notebooklist-login--success-1 (url-or-port)
  (ein2:log 'info "Login to %s complete. \
Now you can open notebook list by `ein2:notebooklist-open'." url-or-port))

(defun ein2:notebooklist-login--error-1 (url-or-port)
  (ein2:log 'info "Failed to login to %s" url-or-port))

(defun* ein2:notebooklist-login--success (url-or-port &key
                                                     data
                                                     &allow-other-keys)
  (if (plist-get data :bad-page)
      (ein2:notebooklist-login--error-1 url-or-port)
    (ein2:notebooklist-login--success-1 url-or-port)))

(defun* ein2:notebooklist-login--error
    (url-or-port &key
                 data
                 symbol-status
                 response
                 &allow-other-keys
                 &aux
                 (response-status (request-response-status-code response)))
  (if (or
       ;; workaround for url-retrieve backend
       (and (eq symbol-status 'timeout)
            (equal response-status 302)
            (request-response-header response "set-cookie"))
       ;; workaround for curl backend
       (and (equal response-status 405)
            (ein2:aand (car (request-response-history response))
                      (request-response-header it "set-cookie"))))
      (ein2:notebooklist-login--success-1 url-or-port)
    (ein2:notebooklist-login--error-1 url-or-port)))


;;; Generic getter

(defun ein2:get-url-or-port--notebooklist ()
  (when (ein2:$notebooklist-p ein2:%notebooklist%)
    (ein2:$notebooklist-url-or-port ein2:%notebooklist%)))


;;; Notebook list mode

(define-derived-mode ein2:notebooklist-mode fundamental-mode "ein2:notebooklist"
  "IPython notebook list mode.")

(defun ein2:notebooklist-prev-item () (interactive) (move-beginning-of-line 0))
(defun ein2:notebooklist-next-item () (interactive) (move-beginning-of-line 2))

(setq ein2:notebooklist-mode-map (copy-keymap widget-keymap))

(let ((map ein2:notebooklist-mode-map))
  (define-key map "\C-c\C-r" 'ein2:notebooklist-reload)
  (define-key map "g" 'ein2:notebooklist-reload)
  (define-key map "p" 'ein2:notebooklist-prev-item)
  (define-key map "n" 'ein2:notebooklist-next-item)
  (define-key map "q" 'bury-buffer)
  (easy-menu-define ein2:notebooklist-menu map "EIN Notebook List Mode Menu"
    `("EIN Notebook List"
      ,@(ein2:generate-menu
         '(("Reload" ein2:notebooklist-reload)
           ("New Notebook" ein2:notebooklist-new-notebook)
           ("New Notebook (with name)"
            ein2:notebooklist-new-notebook-with-name)
           ("New Junk Notebook" ein2:junk-new))))))

(provide 'ein-notebooklist)

;;; ein-notebooklist.el ends here
