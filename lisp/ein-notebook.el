;;; ein-notebook.el --- Notebook module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-notebook.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-notebook.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-notebook.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * Coding rule about current buffer.
;; A lot of notebook and cell functions touches to current buffer and
;; it is not ideal to wrap all these functions by `with-current-buffer'.
;; Therefore, when the function takes `notebook' to the first argument
;; ("method" function), it is always assumed that the current buffer
;; is the notebook buffer.  **However**, functions called as callback
;; (via `url-retrieve', for example) must protect themselves by
;; calling from unknown buffer.

;;; Code:


(eval-when-compile (require 'cl))
(require 'ewoc)
(eval-when-compile (require 'auto-complete nil t))

(require 'ein-core)
(require 'ein-log)
(require 'ein-node)
(require 'ein-kernel)
(require 'ein-kernelinfo)
(require 'ein-cell)
(require 'ein-worksheet)
(require 'ein-scratchsheet)
(require 'ein-notification)
(require 'ein-completer)
(require 'ein-pager)
(require 'ein-events)
(require 'ein-notification)
(require 'ein-kill-ring)
(require 'ein-query)
(require 'ein-pytools)


;;; Configuration

(make-obsolete-variable 'ein2:notebook-discard-output-on-save nil "0.2.0")

(defcustom ein2:notebook-discard-output-on-save 'no
  "Configure if the output part of the cell should be saved or not.

.. warning:: This configuration is obsolete now.
   Use nbconvert (https://github.com/ipython/nbconvert) to
   strip output.

`no' : symbol
    Save output. This is the default.
`yes' : symbol
    Always discard output.
a function
    This function takes two arguments, notebook and cell.  Return
    `t' to discard output and return `nil' to save.  For example,
    if you don't want to save image output but other kind of
    output, use `ein2:notebook-cell-has-image-output-p'.
"
  :type '(choice (const :tag "No" 'no)
                 (const :tag "Yes" 'yes)
                 )
  :group 'ein)

(defun ein2:notebook-cell-has-image-output-p (-ignore- cell)
  (ein2:cell-has-image-ouput-p cell))

(defun ein2:notebook-discard-output-p (notebook cell)
  "Return non-`nil' if the output must be discarded, otherwise save."
  (case ein2:notebook-discard-output-on-save
    (no nil)
    (yes t)
    (t (funcall ein2:notebook-discard-output-on-save notebook cell))))

;; As opening/saving notebook treats possibly huge data, define these
;; timeouts separately:

(defcustom ein2:notebook-querty-timeout-open (* 60 1000) ; 1 min
  "Query timeout for opening notebook.
If you cannot open large notebook because of timeout error, try
to increase this value.  Setting this value to `nil' means to use
global setting.  For global setting and more information, see
`ein2:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein2:notebook-querty-timeout-save (* 60 1000) ; 1 min
  "Query timeout for saving notebook.
Similar to `ein2:notebook-querty-timeout-open', but for saving
notebook.  For global setting and more information, see
`ein2:query-timeout'."
  :type '(choice (integer :tag "Timeout [ms]" 5000)
                 (const :tag "Use global setting" nil))
  :group 'ein)

(defcustom ein2:helm-kernel-history-search-key nil
  "Bind `helm-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein2:helm-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein2:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein2:anything-kernel-history-search-key nil
  "Bind `anything-ein-kernel-history' to this key in notebook mode.

Example::

    (setq ein2:anything-kernel-history-search-key \"\\M-r\")

This key will be installed in the `ein2:notebook-mode-map'."
  :type 'boolean
  :group 'ein)

(defcustom ein2:notebook-set-buffer-file-name nil
  "[EXPERIMENTAL] Set `buffer-file-name' of notebook buffer."
  :type 'boolean
  :group 'ein)

(defvar ein2:notebook-after-rename-hook nil
  "Hooks to run after notebook is renamed successfully.
Current buffer for these functions is set to the notebook buffer.")


;;; Class and variable

(defvar ein2:base-kernel-url "/api/")
(defvar ein2:create-session-url "/api/sessions")
;; Currently there is no way to know this setting.  Maybe I should ask IPython
;; developers for an API to get this from notebook server.  
;;
;; 10April2014 (JMM) - The most recent documentation for the RESTful interface
;; is at:
;; https://github.com/ipython/ipython/wiki/IPEP-16%3A-Notebook-multi-directory-dashboard-and-URL-mapping


(defvar ein2:notebook-pager-buffer-name-template "*ein2:pager %s/%s*")
(defvar ein2:notebook-buffer-name-template "*ein2: %s/%s*")

(defvar ein2:notebook-save-retry-max 1
  "Maximum retries for notebook saving.")

(defstruct ein2:$notebook
  "Hold notebook variables.

`ein2:$notebook-url-or-port'
  URL or port of IPython server.

`ein2:$notebook-notebook-id' : string
  uuid string (as of ipython 2.0 this is the same is notebook-name).

`ein2:$notebook-notebook-path' : string
  Path to notebook.

`ein2:$notebook-kernel' : `ein2:$kernel'
  `ein2:$kernel' instance.

`ein2:$notebook-kernelinfo' : `ein2:kernelinfo'
  `ein2:kernelinfo' instance.

`ein2:$notebook-pager'
  Variable for `ein2:pager-*' functions. See ein-pager.el.

`ein2:$notebook-dirty' : boolean
  Set to `t' if notebook has unsaved changes.  Otherwise `nil'.

`ein2:$notebook-metadata' : plist
  Notebook meta data (e.g., notebook name).

`ein2:$notebook-name' : string
  Notebook name.

`ein2:$notebook-nbformat' : integer
  Notebook file format version.

`ein2:$notebook-nbformat-minor' : integer
  Notebook file format version.

`ein2:$notebook-events' : `ein2:$events'
  Event handler instance.

`ein2:$notebook-worksheets' : list of `ein2:worksheet'
  List of worksheets.

`ein2:$notebook-scratchsheets' : list of `ein2:worksheet'
  List of scratch worksheets.

`ein2:$notebook-api-version' : integer
   Major version of the IPython notebook server we are talking to.
"
  url-or-port
  notebook-id ;; In IPython-2.0 this is "[:path]/[:name].ipynb"
  notebook-path
  kernel
  kernelinfo
  pager
  dirty
  metadata
  notebook-name
  nbformat
  nbformat-minor
  events
  worksheets
  scratchsheets
  api-version
  )

(ein2:deflocal ein2:%notebook% nil
  "Buffer local variable to store an instance of `ein2:$notebook'.")
(define-obsolete-variable-alias 'ein2:notebook 'ein2:%notebook% "0.1.2")



;;; Constructor

(defun ein2:notebook-new (url-or-port api-version notebook-name notebook-path &rest args)
  (let ((notebook (apply #'make-ein2:$notebook
                         :url-or-port url-or-port
                         :api-version api-version
                         :notebook-id notebook-name
                         :notebook-name notebook-name
                         :notebook-path notebook-path
                         args)))
    notebook))


;;; Destructor

(defun ein2:notebook-del (notebook)
  "Destructor for `ein2:$notebook'."
  (ein2:log-ignore-errors
    (ein2:kernel-del (ein2:$notebook-kernel notebook))))

(defun ein2:notebook-close-worksheet (notebook ws)
  "Close worksheet WS in NOTEBOOK.  If WS is the last worksheet,
call notebook destructor `ein2:notebook-del'."
  (symbol-macrolet ((worksheets (ein2:$notebook-worksheets notebook))
                    (scratchsheets (ein2:$notebook-scratchsheets notebook)))
    (cond
     ((ein2:worksheet-p ws) (ein2:worksheet-save-cells ws t))
     (t (setq scratchsheets (delq ws scratchsheets))))
    (unless (or (ein2:filter (lambda (x)
                              (and (not (eq x ws))
                                   (ein2:worksheet-has-buffer-p x)))
                            worksheets)
                scratchsheets)
      (ein2:notebook-del notebook))))


;;; Notebook utility functions

(defun ein2:notebook-buffer (notebook)
  "Return the buffer that is associated with NOTEBOOK."
  ;; FIXME: Find a better way to define notebook buffer!
  ;;        For example, the last accessed buffer.
  (let ((first-buffer
         (lambda (ws-list)
           (loop for ws in ws-list if (ein2:worksheet-buffer ws) return it))))
    (or (funcall first-buffer (ein2:$notebook-worksheets    notebook))
        (funcall first-buffer (ein2:$notebook-scratchsheets notebook)))))

(defun ein2:notebook-buffer-list (notebook)
  "Return the buffers associated with NOTEBOOK's kernel.
The buffer local variable `default-directory' of these buffers
will be updated with kernel's cwd."
  (ein2:filter #'identity
              (mapcar #'ein2:worksheet-buffer
                      (append (ein2:$notebook-worksheets notebook)
                              (ein2:$notebook-scratchsheets notebook)))))

(defun ein2:notebook--get-nb-or-error ()
  (or ein2:%notebook% (error "Not in notebook buffer.")))

;;;###autoload
(defalias 'ein2:notebook-name 'ein2:$notebook-notebook-name)

(defun ein2:notebook-name-getter (notebook)
  (cons #'ein2:notebook-name notebook))


;;; Open notebook

(defun ein2:notebook-url (notebook)
  (ein2:notebook-url-from-url-and-id (ein2:$notebook-url-or-port notebook)
                                    (ein2:$notebook-api-version notebook)
                                    (ein2:$notebook-notebook-path notebook)
                                    (ein2:$notebook-notebook-id notebook)))

(defun ein2:notebook-url-from-url-and-id (url-or-port api-version path notebook-id)
  (cond ((= 2 api-version)
         (ein2:url url-or-port "api/notebooks" path notebook-id))
        ((= 3 api-version)
         (ein2:url url-or-port "api/contents" path notebook-id))))

(defun ein2:notebook-pop-to-current-buffer (&rest -ignore-)
  "Default callback for `ein2:notebook-open'."
  (pop-to-buffer (current-buffer)))

;;; TODO - I think notebook-path is unnecessary (JMM).

(defun ein2:notebook-open (url-or-port api-version notebook-id path &optional callback cbargs)
  "Open notebook of NOTEBOOK-ID in the server URL-OR-PORT.
Opened notebook instance is returned.  Note that notebook might not be
ready at the time when this function is executed.

After the notebook is opened, CALLBACK is called as::

  \(apply CALLBACK notebook CREATED CBARGS)

where the second argument CREATED indicates whether the notebook
is newly created or not.  When CALLBACK is specified, buffer is
**not** brought up by `pop-to-buffer'.  It is caller's
responsibility to do so.  The current buffer is set to the
notebook buffer when CALLBACK is called."
  (unless callback (setq callback #'ein2:notebook-pop-to-current-buffer))
  (let ((buffer (ein2:notebook-get-opened-buffer url-or-port notebook-id)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (ein2:log 'info "Notebook %s is already opened."
                   (ein2:$notebook-notebook-name ein2:%notebook%))
          (when callback
            (apply callback ein2:%notebook% nil cbargs))
          ein2:%notebook%)
      (ein2:log 'info "Opening notebook %s..." notebook-id)
      (ein2:notebook-request-open url-or-port api-version notebook-id path callback cbargs))))

(defun ein2:notebook-request-open (url-or-port api-version notebook-id path
                                              &optional callback cbargs)
  "Request notebook of NOTEBOOK-ID to the server at URL-OR-PORT.
Return `ein2:$notebook' instance.  Notebook may not be ready at
the time of execution.

CALLBACK is called as \(apply CALLBACK notebook t CBARGS).  The second
argument `t' indicates that the notebook is newly opened.
See `ein2:notebook-open' for more information."
  (unless path (setq path ""))
  (let ((url (ein2:notebook-url-from-url-and-id url-or-port api-version path notebook-id))
        (notebook (ein2:notebook-new url-or-port api-version notebook-id path)))
    (ein2:log 'debug "Opening notebook at %s" url)
    (ein2:query-singleton-ajax
     (list 'notebook-open url-or-port api-version notebook-id path)
     url
     :timeout ein2:notebook-querty-timeout-open
     :parser #'ein2:json-read
     :success (apply-partially
               #'ein2:notebook-request-open-callback-with-callback
               notebook callback cbargs))
    notebook))


(defun ein2:notebook-request-open-callback-with-callback (notebook
                                                         callback
                                                         cbargs
                                                         &rest args)
  (apply #'ein2:notebook-request-open-callback notebook args)
  (when callback
    (with-current-buffer (ein2:notebook-buffer notebook)
      (apply callback notebook t cbargs))))

(defun* ein2:notebook-request-open-callback (notebook &key data
                                                     &allow-other-keys)
  (let ((notebook-id (ein2:$notebook-notebook-id notebook)))
    (ein2:notebook-bind-events notebook (ein2:events-new))
    (ein2:notebook-start-kernel notebook)
    (ein2:notebook-from-json notebook data) ; notebook buffer is created here
    (setf (ein2:$notebook-kernelinfo notebook)
          (ein2:kernelinfo-new (ein2:$notebook-kernel notebook)
                              (cons #'ein2:notebook-buffer-list notebook)))
    (ein2:notebook-put-opened-notebook notebook)
    (ein2:notebook--check-nbformat data)
    (ein2:log 'info "Notebook %s is ready"
             (ein2:$notebook-notebook-name notebook))))

(defun ein2:notebook--different-number (n1 n2)
  (and (numberp n1) (numberp n2) (not (= n1 n2))))

(defun ein2:notebook--check-nbformat (data)
  "Warn user when nbformat is changed on server side.
See https://github.com/ipython/ipython/pull/1934 for the purpose
of minor mode."
  ;; See `Notebook.prototype.load_notebook_success'
  ;; at IPython/frontend/html/notebook/static/js/notebook.js
  (destructuring-bind (&key nbformat orig_nbformat
                            nbformat_minor orig_nbformat_minor
                            &allow-other-keys)
      data
    (cond
     ((ein2:notebook--different-number nbformat orig_nbformat)
      (ein2:display-warning
       (format "Notebook major version updated (v%d -> v%d).
  To not update version, do not save this notebook."
               orig_nbformat nbformat)))
     ((ein2:notebook--different-number nbformat_minor orig_nbformat_minor)
      (ein2:display-warning
       (format "This notebook is version v%s.%s, but IPython
  server you are using only fully support up to v%s.%s.
  Some features may not be available."
               orig_nbformat orig_nbformat_minor
               nbformat nbformat_minor))))))


;;; Initialization.

(defun ein2:notebook-bind-events (notebook events)
  "Bind events related to PAGER to the event handler EVENTS."
  (setf (ein2:$notebook-events notebook) events)
  (ein2:worksheet-class-bind-events events)
  ;; Bind events for sub components:
  (setf (ein2:$notebook-pager notebook)
        (ein2:pager-new
         (format ein2:notebook-pager-buffer-name-template
                 (ein2:$notebook-url-or-port notebook)
                 (ein2:$notebook-notebook-name notebook))
         (ein2:$notebook-events notebook))))

(define-obsolete-function-alias
  'ein2:notebook-show-in-shared-output
  'ein2:shared-output-show-code-cell-at-point "0.1.2")


;;; Kernel related things

;;; This no longer works in iPython-2.0. Protocol is to create a session for a
;;; notebook, which will automatically create and associate a kernel with the notebook.
(defun ein2:notebook-start-kernel (notebook)
  (let* ((base-url (concat ein2:base-kernel-url "kernels"))
         (kernel (ein2:kernel-new (ein2:$notebook-url-or-port notebook)
                                 base-url
                                 (ein2:$notebook-events notebook)
                                 (ein2:$notebook-api-version notebook))))
    (setf (ein2:$notebook-kernel notebook) kernel)
    (ein2:pytools-setup-hooks kernel)
    (ein2:kernel-start kernel
                      (ein2:$notebook-notebook-id notebook))))

(defun ein2:notebook-restart-kernel (notebook)
  (ein2:kernel-restart (ein2:$notebook-kernel notebook)))

(defun ein2:notebook-restart-kernel-command ()
  "Send request to the server to restart kernel."
  (interactive)
  (if ein2:%notebook%
      (when (y-or-n-p "Really restart kernel? ")
        (ein2:notebook-restart-kernel ein2:%notebook%))
    (ein2:log 'error "Not in notebook buffer!")))

(define-obsolete-function-alias
  'ein2:notebook-request-tool-tip-or-help-command
  'ein2:pytools-request-tooltip-or-help "0.1.2")

(defun ein2:notebook-complete-dot ()
  "Insert dot and request completion."
  (interactive)
  (if (and ein2:%notebook% (ein2:codecell-p (ein2:get-cell-at-point)))
      (ein2:completer-dot-complete)
    (insert ".")))

(defun ein2:notebook-kernel-interrupt-command ()
  "Interrupt the kernel.
This is equivalent to do ``C-c`` in the console program."
  (interactive)
  (ein2:kernel-interrupt (ein2:$notebook-kernel ein2:%notebook%)))

(defun ein2:notebook-kernel-kill-command ()
  (interactive)
  (when (y-or-n-p "Really kill kernel?")
    (ein2:kernel-kill (ein2:$notebook-kernel ein2:%notebook%))))

;; autoexec

(defun ein2:notebook-execute-autoexec-cells (notebook)
  "Execute cells of which auto-execution flag is on."
  (interactive (list (or ein2:%notebook% (error "Not in notebook buffer!"))))
  (mapc #'ein2:worksheet-execute-autoexec-cells
        (ein2:$notebook-worksheets notebook)))

(define-obsolete-function-alias
  'ein2:notebook-eval-string
  'ein2:shared-output-eval-string "0.1.2")


;;; Persistence and loading

(defun ein2:notebook-set-notebook-name (notebook name)
  "Check NAME and change the name of NOTEBOOK to it."
  (if (ein2:notebook-test-notebook-name name)
      (setf (ein2:$notebook-notebook-name notebook) name
            (ein2:$notebook-notebook-id notebook) name)
    (ein2:log 'error "%S is not a good notebook name." name)
    (error "%S is not a good notebook name." name)))

(defun ein2:notebook-test-notebook-name (name)
  (and (stringp name)
       (> (length name) 0)
       (not (string-match "[\\/\\\\:]" name))))

(defun* ein2:notebook--worksheet-new (notebook
                                     &optional (func #'ein2:worksheet-new))
  (funcall func
           (ein2:$notebook-nbformat notebook)
           (ein2:notebook-name-getter notebook)
           (cons (lambda (notebook cell)
                   (ein2:notebook-discard-output-p notebook cell))
                 notebook)
           (ein2:$notebook-kernel notebook)
           (ein2:$notebook-events notebook)))

(defun ein2:notebook--worksheet-render (notebook ws)
  (ein2:worksheet-render ws)
  (with-current-buffer (ein2:worksheet-buffer ws)
    (ein2:notebook-mode)
    ;; Now that major-mode is set, set buffer local variables:
    (ein2:notebook--notification-setup notebook)
    (ein2:notebook-setup-kill-buffer-hook)
    (ein2:notebook-set-buffer-file-name-maybe notebook)
    (setq ein2:%notebook% notebook)))

(defun ein2:notebook--notification-setup (notebook)
  (ein2:notification-setup
   (current-buffer)
   (ein2:$notebook-events notebook)
   :get-list
   (lambda () (ein2:$notebook-worksheets ein2:%notebook%))
   :get-current
   (lambda () ein2:%worksheet%)
   :get-name
   #'ein2:worksheet-name
   :get-buffer
   (lambda (ws)
     (ein2:notebook-worksheet--render-maybe ein2:%notebook% ws "clicked")
     (ein2:worksheet-buffer ws))
   :delete
   (lambda (ws)
     (ein2:notebook-worksheet-delete ein2:%notebook% ws t))
   :insert-prev
   (lambda (ws) (ein2:notebook-worksheet-insert-prev ein2:%notebook% ws))
   :insert-next
   (lambda (ws) (ein2:notebook-worksheet-insert-next ein2:%notebook% ws))
   :move-prev
   (lambda (ws) (ein2:notebook-worksheet-move-prev ein2:%notebook% ws))
   :move-next
   (lambda (ws) (ein2:notebook-worksheet-move-next ein2:%notebook% ws))
   ))

(defun ein2:notebook-set-buffer-file-name-maybe (notebook)
  "Set `buffer-file-name' of the current buffer to ipynb file
of NOTEBOOK."
  (when ein2:notebook-set-buffer-file-name
    (ein2:notebook-fetch-data
     notebook
     (lambda (data notebook buffer)
       (with-current-buffer buffer
         (destructuring-bind (&key project &allow-other-keys)
             data
           (setq buffer-file-name
                 (expand-file-name
                  (format "%s.ipynb"
                          (ein2:$notebook-notebook-name notebook))
                  project)))))
     (list notebook (current-buffer)))))

(defun ein2:notebook-from-json (notebook data)
  (let ((data (plist-get data :content)))
    (destructuring-bind (&key metadata nbformat nbformat_minor
                              &allow-other-keys)
        data
      (setf (ein2:$notebook-metadata notebook) metadata)
      (setf (ein2:$notebook-nbformat notebook) nbformat)
      (setf (ein2:$notebook-nbformat-minor notebook) nbformat_minor)
      ;;(setf (ein2:$notebook-notebook-name notebook) (plist-get metadata :name))
      )
    (setf (ein2:$notebook-worksheets notebook)
          (cl-case (ein2:$notebook-nbformat notebook)
            (3 (ein2:read-nbformat3-worksheets notebook data))
            (4 (ein2:read-nbformat4-worksheets notebook data))
            (t (ein2:log 'error "Do not currently support nbformat version %s" (ein2:$notebook-nbformat notebook)))))
    (ein2:notebook--worksheet-render notebook
                                    (nth 0 (ein2:$notebook-worksheets notebook)))
    notebook))

(defun ein2:read-nbformat3-worksheets (notebook data)
  (mapcar (lambda (ws-data)
                    (ein2:worksheet-from-json
                     (ein2:notebook--worksheet-new notebook)
                     ws-data))
          (or (plist-get data :worksheets)
              (list nil))))

;; nbformat4 gets rid of the concenpt of worksheets. That means, for the moment,
;; ein will no longer support worksheets. There may be a path forward for
;; reimplementing this feature, however.  The nbformat 4 json definition says
;; that cells are allowed to have tags. Clever use of this feature may lead to
;; good things.

(defun ein2:read-nbformat4-worksheets (notebook data)
  "Convert a notebook in nbformat4 to a list of worksheet-like
  objects suitable for processing in ein2:notebook-from-json."
  (ein2:log 'info "Reading nbformat4 notebook.")
  (let* ((cells (plist-get data :cells))
         (ws-cells (mapcar (lambda (data) (ein2:cell-from-json data)) cells))
         (worksheet (ein2:notebook--worksheet-new notebook)))
    (oset worksheet :saved-cells ws-cells)
    (list worksheet)))

(defun ein2:notebook-to-json (notebook)
  "Return json-ready alist."
  (case (ein2:$notebook-nbformat notebook)
    (3 (ein2:write-nbformat3-worksheets notebook))
    (4 (ein2:write-nbformat4-worksheets notebook))
    (t (ein2:log 'error "EIN does not support saving notebook format %s" (ein2:$notebook-nbformat notebook)))))

(defun ein2:write-nbformat3-worksheets (notebook)
  (let ((worksheets (mapcar #'ein2:worksheet-to-json
                            (ein2:$notebook-worksheets notebook))))
    `((worksheets . ,(apply #'vector worksheets))
      (metadata . ,(ein2:$notebook-metadata notebook))
      )))

(defun ein2:write-nbformat4-worksheets (notebook)
  (ein2:log 'info "Writing notebook %s as nbformat 4." (ein2:$notebook-notebook-name notebook))
  (let ((all-cells (first (mapcar #'ein2:worksheet-to-nb4-json
                                  (ein2:$notebook-worksheets notebook)))))
    `((metadata . ,(ein2:$notebook-metadata notebook))
      (cells . ,(apply #'vector all-cells)))))

(defun ein2:notebook-save-notebook (notebook retry &optional callback cbarg)
  (let ((content-data (ein2:notebook-to-json notebook)))
    (plist-put (cdr (assq 'metadata content-data))
               :name (ein2:$notebook-notebook-name notebook))
    (push `(nbformat . ,(ein2:$notebook-nbformat notebook)) content-data)
    (ein2:aif (ein2:$notebook-nbformat-minor notebook)
        ;; Do not set nbformat when it is not given from server.
        (push `(nbformat_minor . ,it) content-data))
    (ein2:events-trigger (ein2:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (let ((data `((content . ,content-data))))
      (push `(path . ,(ein2:$notebook-notebook-path notebook)) data)
      (push `(name . ,(ein2:$notebook-notebook-name notebook)) data)
      (push `(type . "notebook") data)
      (ein2:query-singleton-ajax
       (list 'notebook-save
             (ein2:$notebook-url-or-port notebook)
             (ein2:$notebook-notebook-path notebook)
             (ein2:$notebook-notebook-name notebook))
       (ein2:notebook-url notebook)
       :timeout ein2:notebook-querty-timeout-save
       :type "PUT"
       :headers '(("Content-Type" . "application/json"))
       :data (json-encode data)
       :error (apply-partially #'ein2:notebook-save-notebook-error notebook)
       :success (apply-partially #'ein2:notebook-save-notebook-workaround
                                 notebook retry callback cbarg)
       :status-code
       `((200 . ,(apply-partially
                  (lambda (notebook callback cbarg &rest rest)
                    (apply #'ein2:notebook-save-notebook-success
                           notebook rest)
                    (when callback
                      (apply callback cbarg rest)))
                  notebook callback cbarg)))))))

(defun ein2:notebook-save-notebook-command ()
  "Save the notebook."
  (interactive)
  (ein2:notebook-save-notebook ein2:%notebook% 0))

(defun* ein2:notebook-save-notebook-workaround
    (notebook retry callback cbarg
              &key
              status
              response
              &allow-other-keys
              &aux
              (response-status (request-response-status-code response)))
  ;; IPython server returns 204 only when the notebook URL is
  ;; accessed via PUT or DELETE.  As it seems Emacs failed to
  ;; choose PUT method every two times, let's check the response
  ;; here and fail when 204 is not returned.
  ;; UPDATE 12Sep2014 (JMM) - IPython server now returns 200 on successful save.
  (unless (eq response-status 200)
    (with-current-buffer (ein2:notebook-buffer notebook)
      (if (< retry ein2:notebook-save-retry-max)
          (progn
            (ein2:log 'info "Retry saving... Next count: %s" (1+ retry))
            (ein2:notebook-save-notebook notebook (1+ retry)
                                        callback cbarg))
        (ein2:notebook-save-notebook-error notebook :status status)
        (ein2:log 'info
          "Status code (=%s) is not 200 and retry exceeds limit (=%s)."
          response-status ein2:notebook-save-retry-max)))))

(defun ein2:notebook-save-notebook-success (notebook &rest ignore)
  (ein2:log 'info "Notebook is saved.")
  (setf (ein2:$notebook-dirty notebook) nil)
  (mapc (lambda (ws)
          (ein2:worksheet-save-cells ws) ; [#]_
          (ein2:worksheet-set-modified-p ws nil))
        (ein2:$notebook-worksheets notebook))
  (ein2:events-trigger (ein2:$notebook-events notebook)
                      'notebook_saved.Notebook))
;; .. [#] Consider the following case.
;;    (1) Open worksheet WS0 and other worksheets.
;;    (2) Edit worksheet WS0 then save the notebook.
;;    (3) Edit worksheet WS0.
;;    (4) Kill WS0 buffer by discarding the edit.
;;    (5) Save the notebook.
;;    This should save the latest WS0.  To do so, WS0 at the point (2)
;;    must be cached in the worksheet slot `:saved-cells'.

(defun* ein2:notebook-save-notebook-error (notebook &key symbol-status
                                                   &allow-other-keys)
  (if (eq symbol-status 'user-cancel)
      (ein2:log 'info "Cancel saving notebook.")
    (ein2:log 'info "Failed to save notebook!")
    (ein2:events-trigger (ein2:$notebook-events notebook)
                        'notebook_save_failed.Notebook)))

(defun ein2:notebook-rename-command (name)
  "Rename current notebook and save it immediately.

NAME is any non-empty string that does not contain '/' or '\\'."
  (interactive
   (list (read-string "Rename notebook: "
                      (let ((name (ein2:$notebook-notebook-name ein2:%notebook%)))
                        (unless (string-match "Untitled[0-9]+" name)
                          name)))))
  (unless (and (string-match ".ipynb" name) (= (match-end 0) (length name)))
    (setq name (format "%s.ipynb" name)))
  (let ((old-name (ein2:$notebook-notebook-name ein2:%notebook%)))
    (ein2:log 'info "Renaming notebook at URL %s" (ein2:notebook-url ein2:%notebook%))
    (ein2:log 'info "JSON data looks like %s"
             (json-encode
              `((:name . ,name)
                (:path . ,(ein2:$notebook-notebook-path ein2:%notebook%)))))
    ;(ein2:notebook-set-notebook-name ein2:%notebook% name)
    (ein2:query-singleton-ajax
     (list 'notebook-rename
           (ein2:$notebook-url-or-port ein2:%notebook%)
           (ein2:$notebook-notebook-path ein2:%notebook%)
           (ein2:$notebook-notebook-name ein2:%notebook%))
     (ein2:notebook-url ein2:%notebook%)
     :timeout ein2:notebook-querty-timeout-save
     :type "PATCH"
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode
            `((name . ,name)
              (path . ,(ein2:$notebook-notebook-path ein2:%notebook%))))
     :error (apply-partially #'ein2:notebook-rename-error old-name name ein2:%notebook%)
     :success (apply-partially #'ein2:notebook-rename-success
                               ein2:%notebook% name)
     :status-code
     `((200 . ,(apply-partially
                #'ein2:notebook-rename-success ein2:%notebook% name))
       (409 . ,(apply-partially
                #'ein2:notebook-rename-success ein2:%notebook% name))))))

(defun* ein2:notebook-rename-error (old new notebook &key symbol-status response
                                       error-thrown
                                       &allow-other-keys)
  (if (= (request-response-status-code response) 409)
      (progn
        (ein2:log 'warn "IPython returned a 409 status code, but has still renamed the notebook. This may be an IPython bug.")
        (ein2:notebook-rename-success notebook new response))
    (ein2:log 'error
      "Error (%s :: %s) while renaming notebook %s to %s."
      symbol-status error-thrown old new)))

(defun* ein2:notebook-rename-success (notebook new &rest args &allow-other-keys)
  (ein2:notebook-set-notebook-name notebook new)
  (mapc #'ein2:worksheet-set-buffer-name
        (ein2:$notebook-worksheets notebook))
  (ein2:log 'info "Notebook renamed to %s." new))

(defun ein2:notebook-close (notebook)
  "Close NOTEBOOK and kill its buffer."
  (interactive (prog1 (list (ein2:notebook--get-nb-or-error))
                 (or (ein2:notebook-ask-before-kill-buffer)
                     (error "Quit"))))
  (let ((ein2:notebook-kill-buffer-ask nil))
    ;; Let `ein2:notebook-kill-buffer-callback' do its job.
    (mapc #'kill-buffer (ein2:notebook-buffer-list notebook))))

(defun ein2:notebook-kill-kernel-then-close-command ()
  "Kill kernel and then kill notebook buffer.
To close notebook without killing kernel, just close the buffer
as usual."
  (interactive)
  (when (ein2:notebook-ask-before-kill-buffer)
    (let ((kernel (ein2:$notebook-kernel ein2:%notebook%)))
      ;; If kernel is live, kill it before closing.
      (if (ein2:kernel-live-p kernel)
          (ein2:kernel-kill kernel #'ein2:notebook-close (list ein2:%notebook%))
        (ein2:notebook-close ein2:%notebook%)))))


;;; Worksheet

(defmacro ein2:notebook--worksheet-render-new (notebook type)
  "Create new worksheet of TYPE in NOTEBOOK."
  (let ((func (intern (format "ein2:%s-new" type)))
        (slot (list (intern (format "ein2:$notebook-%ss" type)) notebook)))
    `(let ((ws (ein2:notebook--worksheet-new ,notebook #',func)))
       (setf ,slot (append ,slot (list ws)))
       (ein2:notebook--worksheet-render ,notebook ws)
       ws)))

(defun ein2:notebook-worksheet-render-new (notebook)
  "Create new worksheet in NOTEBOOK."
  (ein2:notebook--worksheet-render-new notebook worksheet))

(defun ein2:notebook-worksheet-open-next-or-new (notebook ws &optional show)
  "Open next worksheet.  Create new if none.

Try to open the worksheet to the worksheet WS using the function
`ein2:notebook-worksheet-open-next', open a new worksheet if not
found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein2:notebook-worksheet-open-next notebook ws)))
    (unless next
      (ein2:log 'info "Creating new worksheet...")
      (setq next (ein2:notebook-worksheet-render-new notebook))
      (ein2:log 'info "Creating new worksheet... Done."))
    (when show
      (funcall show (ein2:worksheet-buffer next)))))

(defun ein2:notebook-worksheet-open-next-or-first (notebook ws &optional show)
  "Open next or first worksheet.

Try to open the worksheet to the worksheet WS using the function
`ein2:notebook-worksheet-open-next', open the first worksheet if
not found.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (ein2:notebook-worksheet-open-next notebook ws)))
    (unless next
      (setq next (car (ein2:$notebook-worksheets notebook))))
    (when show
      (funcall show (ein2:worksheet-buffer next)))))

(defun ein2:notebook-worksheet-open-prev-or-last (notebook ws &optional show)
  "Open previous or last worksheet.
See also `ein2:notebook-worksheet-open-next-or-first' and
`ein2:notebook-worksheet-open-prev'."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (ein2:notebook-worksheet-open-prev notebook ws)))
    (unless prev
      (setq prev (car (last (ein2:$notebook-worksheets notebook)))))
    (when show
      (funcall show (ein2:worksheet-buffer prev)))))

(defun* ein2:notebook-worksheet--render-maybe
    (notebook ws &optional (adj "next"))
  "Render worksheet WS of NOTEBOOK if it does not have buffer.
ADJ is a adjective to describe worksheet to be rendered."
  (if (ein2:worksheet-has-buffer-p ws)
      (ein2:log 'verbose "The worksheet already has a buffer.")
    (ein2:log 'info "Rendering %s worksheet..." adj)
    (ein2:notebook--worksheet-render notebook ws)
    (ein2:log 'info "Rendering %s worksheet... Done." adj)))

(defun* ein2:notebook-worksheet--open-new
    (notebook new &optional (adj "next") show)
  "Open (possibly new) worksheet NEW of NOTEBOOK with SHOW function.
ADJ is a adjective to describe worksheet to be opened.
SHOW is a function to be called with worksheet buffer if given."
  (when new
    (ein2:notebook-worksheet--render-maybe notebook new adj))
  (when show
    (assert (ein2:worksheet-p new) nil "No %s worksheet." adj)
    (funcall show (ein2:worksheet-buffer new))))

(defun ein2:notebook-worksheet-open-next (notebook ws &optional show)
  "Open next worksheet.

Search the worksheet after the worksheet WS, render it if it is
not yet, then return the worksheet.  If there is no such
worksheet, return nil.  Open the first worksheet if the worksheet
WS is an instance of `ein2:scratchsheet'.

SHOW is a function to be called with the worksheet buffer if
given."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((next (if (ein2:scratchsheet-p ws)
                  (car (ein2:$notebook-worksheets notebook))
                (loop with worksheets = (ein2:$notebook-worksheets notebook)
                      for current in worksheets
                      for next in (cdr worksheets)
                      when (eq current ws) return next))))
    (ein2:notebook-worksheet--open-new notebook next "next" show)
    next))

(defun ein2:notebook-worksheet-open-prev (notebook ws &optional show)
  "Open previous worksheet.
See also `ein2:notebook-worksheet-open-next'."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     #'switch-to-buffer))
  (let ((prev (if (ein2:scratchsheet-p ws)
                  (car (last (ein2:$notebook-worksheets notebook)))
                (loop for (prev current) on (ein2:$notebook-worksheets notebook)
                      when (eq current ws) return prev))))
    (ein2:notebook-worksheet--open-new notebook prev "previous" show)
    prev))

(defun ein2:notebook-worksheet-open-ith (notebook i &optional show)
  "Open I-th (zero-origin) worksheet."
  (let ((ws (nth i (ein2:$notebook-worksheets notebook))))
    (unless ws (error "No %s-th worksheet" (1+ i)))
    (ein2:notebook-worksheet--open-new notebook ws (format "%s-th" i) show)))

(defmacro ein2:notebook-worksheet--defun-open-nth (n)
  "Define a command to open N-th (one-origin) worksheet."
  (assert (and (integerp n) (> n 0)) t)
  (let ((func (intern (format "ein2:notebook-worksheet-open-%sth" n))))
    `(defun ,func (notebook &optional show)
       ,(format "Open %d-th worksheet." n)
       (interactive (list (ein2:notebook--get-nb-or-error)
                          #'switch-to-buffer))
       (ein2:notebook-worksheet-open-ith notebook ,(1- n) show))))

(defmacro ein2:notebook-worksheet--defun-all-open-nth (min max)
  `(progn
     ,@(loop for n from min to max
             collect `(ein2:notebook-worksheet--defun-open-nth ,n))))

(ein2:notebook-worksheet--defun-all-open-nth 1 8)

(defun ein2:notebook-worksheet-open-last (notebook &optional show)
  "Open the last worksheet."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     #'switch-to-buffer))
  (let ((last (car (last (ein2:$notebook-worksheets notebook)))))
    (ein2:notebook-worksheet--open-new notebook last "last" show)
    last))

(defun ein2:notebook-worksheet-insert-new (notebook ws &optional render show
                                                   inserter)
  (let ((new (ein2:notebook--worksheet-new notebook)))
    (setf (ein2:$notebook-worksheets notebook)
          (funcall inserter (ein2:$notebook-worksheets notebook) ws new))
    (when (or render show)
      (ein2:notebook--worksheet-render notebook new))
    (when show
      (funcall show (ein2:worksheet-buffer new)))
    new))

(defun* ein2:notebook-worksheet-insert-next
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet after this worksheet and open it.
See also `ein2:notebook-worksheet-insert-prev'.

.. The worksheet WS is searched in the worksheets slot of
   NOTEBOOK and a newly created worksheet is inserted after WS.
   Worksheet buffer is created when RENDER or SHOW is non-`nil'.
   SHOW is a function which take a buffer."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)))
  (ein2:notebook-worksheet-insert-new notebook ws render show
                                     #'ein2:list-insert-after))

(defun* ein2:notebook-worksheet-insert-prev
    (notebook ws &optional (render t) (show #'switch-to-buffer))
  "Insert a new worksheet before this worksheet and open it.
See also `ein2:notebook-worksheet-insert-next'."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)))
  (ein2:notebook-worksheet-insert-new notebook ws render show
                                     #'ein2:list-insert-before))

(defun ein2:notebook-worksheet-delete (notebook ws &optional confirm)
  "Delete the current worksheet.
When used as a lisp function, delete worksheet WS from NOTEBOOk."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)
                     t))
  (when confirm
    (unless (y-or-n-p
             "Really remove this worksheet? There is no undo.")
      (error "Quit deleting the current worksheet.")))
  (setf (ein2:$notebook-worksheets notebook)
        (delq ws (ein2:$notebook-worksheets notebook)))
  (setf (ein2:$notebook-dirty notebook) t)
  (let ((ein2:notebook-kill-buffer-ask nil))
    (kill-buffer (ein2:worksheet-buffer ws))))

(defun ein2:notebook-worksheet-move-prev (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)))
  (assert (ein2:worksheet-p ws) nil "Not worksheet.")
  (setf (ein2:$notebook-worksheets notebook)
        (ein2:list-move-left (ein2:$notebook-worksheets notebook) ws)))

(defun ein2:notebook-worksheet-move-next (notebook ws)
  "Switch the current worksheet with the previous one."
  (interactive (list (ein2:notebook--get-nb-or-error)
                     (ein2:worksheet--get-ws-or-error)))
  (assert (ein2:worksheet-p ws) nil "Not worksheet.")
  (setf (ein2:$notebook-worksheets notebook)
        (ein2:list-move-right (ein2:$notebook-worksheets notebook) ws)))

(defun* ein2:notebook-worksheet-index
    (&optional (notebook ein2:%notebook%)
               (ws ein2:%worksheet%))
  "Return an index of the worksheet WS in NOTEBOOK."
  (loop for i from 0
        for ith-ws in (ein2:$notebook-worksheets notebook)
        when (eq ith-ws ws)
        return i))


;;; Scratch sheet

(defun ein2:notebook-scratchsheet-render-new (notebook)
  "Create new scratchsheet in NOTEBOOK."
  (ein2:notebook--worksheet-render-new notebook scratchsheet))

(defun ein2:notebook-scratchsheet-open (notebook &optional new popup)
  "Open \"scratch sheet\".
Open a new one when prefix argument is given.
Scratch sheet is almost identical to worksheet.  However, EIN
will not save the buffer.  Use this buffer like of normal IPython
console.  Note that you can always copy cells into the normal
worksheet to save result."
  (interactive (list (ein2:get-notebook-or-error)
                     current-prefix-arg
                     t))
  (let ((ss (or (unless new
                  (car (ein2:$notebook-scratchsheets notebook)))
                (ein2:notebook-scratchsheet-render-new notebook))))
    (when popup
      (pop-to-buffer (ein2:worksheet-buffer ss)))
    ss))


;;; Opened notebooks

(defvar ein2:notebook--opened-map (make-hash-table :test 'equal)
  "A map: (URL-OR-PORT NOTEBOOK-ID) => notebook instance.")

(defun ein2:notebook-get-opened-notebook (url-or-port notebook-id)
  (gethash (list url-or-port notebook-id) ein2:notebook--opened-map))

(defun ein2:notebook-get-opened-buffer (url-or-port notebook-id)
  (ein2:aand (ein2:notebook-get-opened-notebook url-or-port notebook-id)
            (ein2:notebook-buffer it)))

(defun ein2:notebook-put-opened-notebook (notebook)
  (puthash (list (ein2:$notebook-url-or-port notebook)
                 (ein2:$notebook-notebook-id notebook))
           notebook
           ein2:notebook--opened-map))

(defun ein2:notebook-opened-notebooks (&optional predicate)
  "Return list of opened notebook instances.
If PREDICATE is given, notebooks are filtered by PREDICATE.
PREDICATE is called with each notebook and notebook is included
in the returned list only when PREDICATE returns non-nil value."
  (let (notebooks)
    (maphash (lambda (k n) (if (ein2:notebook-live-p n)
                               (push n notebooks)
                             (remhash k ein2:notebook--opened-map)))
             ein2:notebook--opened-map)
    (if predicate
        (ein2:filter predicate notebooks)
      notebooks)))

(defun ein2:notebook-opened-buffers (&optional predicate)
  "Return list of opened notebook buffers."
  (mapcar #'ein2:notebook-buffer (ein2:notebook-opened-notebooks predicate)))

(defun ein2:notebook-opened-buffer-names (&optional predicate)
  "Return list of opened notebook buffer names."
  (mapcar #'buffer-name (ein2:notebook-opened-buffers predicate)))


;;; Generic getter

(defun ein2:get-url-or-port--notebook ()
  (when ein2:%notebook% (ein2:$notebook-url-or-port ein2:%notebook%)))

(defun ein2:get-notebook--notebook ()
  ein2:%notebook%)

(defun ein2:get-kernel--notebook ()
  (when (ein2:$notebook-p ein2:%notebook%)
    (ein2:$notebook-kernel ein2:%notebook%)))


;;; Predicate

(defun ein2:notebook-buffer-p ()
  "Return non-`nil' if current buffer is notebook buffer."
  ein2:%notebook%)

(defun ein2:notebook-live-p (notebook)
  "Return non-`nil' if NOTEBOOK has live buffer."
  (buffer-live-p (ein2:notebook-buffer notebook)))

(defun ein2:notebook-modified-p (&optional notebook)
  "Return non-nil if NOTEBOOK is modified.
If NOTEBOOK is not given or nil then consider the notebook
associated with current buffer (if any)."
  (unless notebook (setq notebook ein2:%notebook%))
  (and (ein2:$notebook-p notebook)
       (ein2:notebook-live-p notebook)
       (or (ein2:$notebook-dirty notebook)
           (loop for ws in (ein2:$notebook-worksheets notebook)
                 when (ein2:worksheet-modified-p ws)
                 return t))))


;;; Notebook mode

(defcustom ein2:notebook-modes
  '(ein2:notebook-multilang-mode)
  "Notebook modes to use \(in order of preference).

When the notebook is opened, mode in this value is checked one by one
and the first usable mode is used.

Available modes:

* `ein2:notebook-multilang-mode'
* `ein2:notebook-mumamo-mode'
* `ein2:notebook-python-mode'
* `ein2:notebook-plain-mode'

Examples:

Use MuMaMo if it is installed.  Otherwise, use plain mode.
This is the old default setting::

  (setq ein2:notebook-modes '(ein2:notebook-mumamo-mode ein2:notebook-plain-mode))

Avoid using MuMaMo even when it is installed::

  (setq ein2:notebook-modes '(ein2:notebook-plain-mode))

Use simple `python-mode' based notebook mode when MuMaMo is not installed::

  (setq ein2:notebook-modes '(ein2:notebook-mumamo-mode ein2:notebook-python-mode))
"
  :type '(repeat (choice (const :tag "Multi-lang" ein2:notebook-multilang-mode)
                         (const :tag "MuMaMo" ein2:notebook-mumamo-mode)
                         (const :tag "Only Python" ein2:notebook-python-mode)
                         (const :tag "Plain" ein2:notebook-plain-mode)))
  :group 'ein)

(defcustom ein2:notebook-mode-hook nil
  "Hook for `ein2:notebook-mode'.
This hook is run regardless the actual major mode used."
  :type 'hook
  :group 'ein)

(defun ein2:notebook-choose-mode ()
  "Return usable (defined) notebook mode."
  ;; So try to load extra modules here.
  (when (require 'mumamo nil t)
    (require 'ein-mumamo))
  ;; Return first matched mode
  (loop for mode in ein2:notebook-modes
        if (functionp mode)
        return mode))

(defvar ein2:notebook-mode-map (make-sparse-keymap))

(let ((map ein2:notebook-mode-map))
  (define-key map "\C-c\C-c" 'ein2:worksheet-execute-cell)
  (define-key map (kbd "M-RET") 'ein2:worksheet-execute-cell-and-goto-next)
  (define-key map (kbd "<M-S-return>")
    'ein2:worksheet-execute-cell-and-insert-below)
  (define-key map (kbd "C-c C-'") 'ein2:worksheet-turn-on-autoexec)
  (define-key map "\C-c\C-e" 'ein2:worksheet-toggle-output)
  (define-key map "\C-c\C-v" 'ein2:worksheet-set-output-visibility-all)
  (define-key map "\C-c\C-l" 'ein2:worksheet-clear-output)
  (define-key map (kbd "C-c C-S-l") 'ein2:worksheet-clear-all-output)
  (define-key map (kbd "C-c C-;") 'ein2:shared-output-show-code-cell-at-point)
  (define-key map "\C-c\C-k" 'ein2:worksheet-kill-cell)
  (define-key map "\C-c\M-w" 'ein2:worksheet-copy-cell)
  (define-key map "\C-c\C-w" 'ein2:worksheet-copy-cell)
  (define-key map "\C-c\C-y" 'ein2:worksheet-yank-cell)
  (define-key map "\C-c\C-a" 'ein2:worksheet-insert-cell-above)
  (define-key map "\C-c\C-b" 'ein2:worksheet-insert-cell-below)
  (define-key map "\C-c\C-t" 'ein2:worksheet-toggle-cell-type)
  (define-key map "\C-c\C-u" 'ein2:worksheet-change-cell-type)
  (define-key map "\C-c\C-s" 'ein2:worksheet-split-cell-at-point)
  (define-key map "\C-c\C-m" 'ein2:worksheet-merge-cell)
  (define-key map "\C-c\C-n" 'ein2:worksheet-goto-next-input)
  (define-key map "\C-c\C-p" 'ein2:worksheet-goto-prev-input)
  (define-key map (kbd "C-<up>") 'ein2:worksheet-goto-prev-input)
  (define-key map (kbd "C-<down>") 'ein2:worksheet-goto-next-input)
  (define-key map (kbd "C-c <up>") 'ein2:worksheet-move-cell-up)
  (define-key map (kbd "C-c <down>") 'ein2:worksheet-move-cell-down)
  (define-key map (kbd "M-<up>") 'ein2:worksheet-move-cell-up)
  (define-key map (kbd "M-<down>") 'ein2:worksheet-move-cell-down)
  (define-key map "\C-c\C-f" 'ein2:pytools-request-tooltip-or-help)
  (define-key map "\C-c\C-i" 'ein2:completer-complete)
  (define-key map "\C-c\C-x" 'ein2:tb-show)
  (define-key map "\C-c\C-r" 'ein2:notebook-restart-kernel-command)
  (define-key map "\C-c\C-z" 'ein2:notebook-kernel-interrupt-command)
  (define-key map "\C-c\C-q" 'ein2:notebook-kill-kernel-then-close-command)
  (define-key map (kbd "C-c C-#") 'ein2:notebook-close)
  (define-key map (kbd "C-:") 'ein2:shared-output-eval-string)
  (define-key map "\C-c\C-o" 'ein2:console-open)
  (define-key map "\C-x\C-s" 'ein2:notebook-save-notebook-command)
  (define-key map "\C-x\C-w" 'ein2:notebook-rename-command)
  (define-key map "\M-."          'ein2:pytools-jump-to-source-command)
  (define-key map (kbd "C-c C-.") 'ein2:pytools-jump-to-source-command)
  (define-key map "\M-,"          'ein2:pytools-jump-back-command)
  (define-key map (kbd "C-c C-,") 'ein2:pytools-jump-back-command)
  (define-key map "\M-p"          'ein2:worksheet-previous-input-history)
  (define-key map "\M-n"          'ein2:worksheet-next-input-history)
  (define-key map (kbd "C-c C-/") 'ein2:notebook-scratchsheet-open)
  ;; Worksheets
  (define-key map (kbd "C-c !")     'ein2:worksheet-rename-sheet)
  (define-key map (kbd "C-c {")     'ein2:notebook-worksheet-open-prev-or-last)
  (define-key map (kbd "C-c }")     'ein2:notebook-worksheet-open-next-or-first)
  (define-key map (kbd "C-c M-{")   'ein2:notebook-worksheet-move-prev)
  (define-key map (kbd "C-c M-}")   'ein2:notebook-worksheet-move-next)
  (define-key map (kbd "C-c +")     'ein2:notebook-worksheet-insert-next)
  (define-key map (kbd "C-c M-+")   'ein2:notebook-worksheet-insert-prev)
  (define-key map (kbd "C-c -")     'ein2:notebook-worksheet-delete)
  (loop for n from 1 to 8
        do (define-key map (format "\C-c%d" n)
             (intern (format "ein2:notebook-worksheet-open-%sth" n))))
  (define-key map "\C-c9" 'ein2:notebook-worksheet-open-last)
  ;; Menu
  (easy-menu-define ein2:notebook-menu map "EIN Notebook Mode Menu"
    `("EIN Notebook"
      ("File"
       ,@(ein2:generate-menu
          '(("Save notebook" ein2:notebook-save-notebook-command)
            ("Rename notebook" ein2:notebook-rename-command)
            ("Close notebook without saving"
             ein2:notebook-close)
            ("Kill kernel then close notebook"
             ein2:notebook-kill-kernel-then-close-command))))
      ("Edit"
       ,@(ein2:generate-menu
          '(("Kill cell" ein2:worksheet-kill-cell)
            ("Copy cell" ein2:worksheet-copy-cell)
            ("Yank cell" ein2:worksheet-yank-cell)
            ("Insert cell above" ein2:worksheet-insert-cell-above)
            ("Insert cell below" ein2:worksheet-insert-cell-below)
            ("Toggle cell type" ein2:worksheet-toggle-cell-type)
            ("Change cell type" ein2:worksheet-change-cell-type)
            ("Split cell at point" ein2:worksheet-split-cell-at-point)
            ("Merge cell" ein2:worksheet-merge-cell)
            ("Go to next cell" ein2:worksheet-goto-next-input)
            ("Go to previous cell" ein2:worksheet-goto-prev-input)
            ("Move cell up" ein2:worksheet-move-cell-up)
            ("Move cell down" ein2:worksheet-move-cell-down)
            ("Dedent text in CELL" ein2:worksheet-dedent-cell-text)
            )))
      ("Cell/Code"
       ,@(ein2:generate-menu
          '(("Execute cell" ein2:worksheet-execute-cell
             :active (ein2:worksheet-at-codecell-p))
            ("Execute cell and go to next"
             ein2:worksheet-execute-cell-and-goto-next
             :active (ein2:worksheet-at-codecell-p))
            ("Execute cell and insert below"
             ein2:worksheet-execute-cell-and-insert-below
             :active (ein2:worksheet-at-codecell-p))
            ("Execute all"
             ein2:worksheet-execute-all-cell)
            ("Turn on auto execution flag" ein2:worksheet-turn-on-autoexec
             :active (ein2:worksheet-at-codecell-p))
            ("Evaluate code in minibuffer" ein2:shared-output-eval-string)
            ("Toggle instant cell execution mode" ein2:iexec-mode)
            ))
       "---"
       ,@(ein2:generate-menu
          '(("Toggle output visibility" ein2:worksheet-toggle-output
             :active (ein2:worksheet-at-codecell-p))
            ("Show all output"
             ein2:worksheet-set-output-visibility-all)
            ("Discard output" ein2:worksheet-clear-output
             :active (ein2:worksheet-at-codecell-p))
            ("Discard all output" ein2:worksheet-clear-all-output)
            ("Show full output" ein2:shared-output-show-code-cell-at-point
             :active (ein2:worksheet-at-codecell-p))
            ("Traceback viewer" ein2:tb-show)
            ))
       "---"
       ,@(ein2:generate-menu
          '(("Show object help"
             ein2:pytools-request-tooltip-or-help)
            ("Complete code" ein2:completer-complete
             :active (ein2:worksheet-at-codecell-p))
            ("Jump to definition" ein2:pytools-jump-to-source-command)
            ("Go back to the previous jump point"
             ein2:pytools-jump-back-command)
            ("Previous input history"
             ein2:worksheet-previous-input-history)
            ("Next input history"
             ein2:worksheet-next-input-history))))
      ("Kernel"
       ,@(ein2:generate-menu
          '(("Restart kernel" ein2:notebook-restart-kernel-command)
            ("Interrupt kernel" ein2:notebook-kernel-interrupt-command))))
      ("Worksheets [Experimental]"
       ,@(ein2:generate-menu
          '(("Rename worksheet" ein2:worksheet-rename-sheet)
            ("Insert next worksheet"
             ein2:notebook-worksheet-insert-next)
            ("Insert previous worksheet"
             ein2:notebook-worksheet-insert-prev)
            ("Delete worksheet" ein2:notebook-worksheet-delete)
            ("Move worksheet left"  ein2:notebook-worksheet-move-prev)
            ("Move worksheet right" ein2:notebook-worksheet-move-next)
            ))
       "---"
       ,@(ein2:generate-menu
          '(("Open previous worksheet"
             ein2:notebook-worksheet-open-prev)
            ("Open previous or last worksheet"
             ein2:notebook-worksheet-open-prev-or-last)
            ("Open next worksheet"
             ein2:notebook-worksheet-open-next)
            ("Open next or first worksheet"
             ein2:notebook-worksheet-open-next-or-first)
            ("Open next or new worksheet"
             ein2:notebook-worksheet-open-next-or-new)
            ))
       "---"
       ,@(ein2:generate-menu
          (append
           (loop for n from 1 to 8
                 collect
                 (list
                  (format "Open %d-th worksheet" n)
                  (intern (format "ein2:notebook-worksheet-open-%sth" n))))
           '(("Open last worksheet" ein2:notebook-worksheet-open-last)))))
      ("Junk notebook"
       ,@(ein2:generate-menu
          '(("Junk this notebook" ein2:junk-rename)
            ("Open new junk" ein2:junk-new))))
      ;; Misc:
      ,@(ein2:generate-menu
         '(("Open regular IPython console" ein2:console-open)
           ("Open scratch sheet" ein2:notebook-scratchsheet-open)
           ("Toggle pseudo console mode" ein2:pseudo-console-mode)
           ))
      ))
  map)

(defun ein2:notebook-mode ()
  (funcall (ein2:notebook-choose-mode))
  (ein2:complete-on-dot-install
   ein2:notebook-mode-map 'ein2:notebook-complete-dot)
  (ein2:aif ein2:helm-kernel-history-search-key
      (define-key ein2:notebook-mode-map it 'helm-ein-kernel-history))
  (ein2:aif ein2:anything-kernel-history-search-key
      (define-key ein2:notebook-mode-map it 'anything-ein-kernel-history))
  (ein2:notebook-minor-mode +1)
  (run-hooks 'ein2:notebook-mode-hook))

(add-hook 'ein2:notebook-mode-hook 'ein2:worksheet-imenu-setup)

(define-minor-mode ein2:notebook-minor-mode
  "Minor mode to install `ein2:notebook-mode-map' for `ein2:notebook-mode'."
  :keymap ein2:notebook-mode-map
  :group 'ein)

;; To avoid MuMaMo to discard `ein2:notebook-minor-mode', make it
;; permanent local.
(put 'ein2:notebook-minor-mode 'permanent-local t)

(define-derived-mode ein2:notebook-plain-mode fundamental-mode "ein2:notebook"
  "IPython notebook mode without fancy coloring."
  (font-lock-mode))

(define-derived-mode ein2:notebook-python-mode python-mode "ein2:python"
  "Use `python-mode' for whole notebook buffer.")

(defun ein2:notebook-open-in-browser (&optional print)
  "Open current notebook in web browser.
When the prefix argument (``C-u``) is given, print page is opened.
Note that print page is not supported in IPython 0.12.1."
  (interactive "P")
  (let ((url (apply #'ein2:url
                    (ein2:$notebook-url-or-port ein2:%notebook%)
                    (ein2:$notebook-notebook-id ein2:%notebook%)
                    (if print (list "print")))))
    (message "Opening %s in browser" url)
    (browse-url url)))

(defun ein2:notebook-fetch-data (notebook callback &optional cbargs)
  "Fetch data in body tag of NOTEBOOK html page.
CALLBACK is called with a plist with data in the body tag as
the first argument and CBARGS as the rest of arguments."
  (let ((url-or-port (ein2:$notebook-url-or-port notebook))
        (notebook-id (ein2:$notebook-notebook-id notebook)))
    (ein2:query-singleton-ajax
     (list 'notebook-fetch-data url-or-port notebook-id)
     (ein2:url url-or-port notebook-id)
     :parser
     (lambda ()
       (list
        :project
        (ein2:html-get-data-in-body-tag "data-project")
        :base-project-url
        (ein2:html-get-data-in-body-tag "data-base-project-url")
        :base-kernel-url
        (ein2:html-get-data-in-body-tag "data-base-kernel-url")
        :read-only
        (ein2:html-get-data-in-body-tag "data-read-only")
        :notebook-id
        (ein2:html-get-data-in-body-tag "data-notebook-id")))
     :success
     (apply-partially (function*
                       (lambda (callback cbargs &key data &allow-other-keys)
                         (apply callback data cbargs)))
                      callback cbargs))))


;;; Buffer and kill hooks

(defcustom ein2:notebook-kill-buffer-ask t
  "Whether EIN should ask before killing unsaved notebook buffer."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil))
  :group 'ein)

;; -- `kill-buffer-query-functions'
(defun ein2:notebook-ask-before-kill-buffer ()
  "Return `nil' to prevent killing the notebook buffer.
Called via `kill-buffer-query-functions'."
  (not (or (and ein2:notebook-kill-buffer-ask
                (ein2:worksheet-p ein2:%worksheet%) ; it's not `ein2:scratchsheet'
                (ein2:notebook-modified-p)
                (not (y-or-n-p
                      "You have unsaved changes. Discard changes?")))
           (when (ein2:worksheet-p ein2:%worksheet%)
             ;; To make `ein2:worksheet-save-cells' no-op.
             (ein2:worksheet-dont-save-cells ein2:%worksheet%)
             nil))))

(add-hook 'kill-buffer-query-functions 'ein2:notebook-ask-before-kill-buffer)

;; -- `kill-emacs-query-functions'
(defun ein2:notebook-ask-before-kill-emacs ()
  "Return `nil' to prevent killing Emacs when unsaved notebook exists.
Called via `kill-emacs-query-functions'."
  (condition-case err
      (let ((unsaved (ein2:filter #'ein2:notebook-modified-p
                                 (ein2:notebook-opened-notebooks))))
        (if (null unsaved)
            t
          (let ((answer
                 (y-or-n-p
                  (format "You have %s unsaved notebook(s). Discard changes?"
                          (length unsaved)))))
            ;; kill all unsaved buffers forcefully
            (when answer
              (mapc #'ein2:notebook-close unsaved))
            answer)))
    ((debug error)
     (ein2:log 'error "Got error: %S" err)
     (y-or-n-p "Error while examine notebooks.  Kill Emacs anyway? "))))

(add-hook 'kill-emacs-query-functions 'ein2:notebook-ask-before-kill-emacs)

;; -- `kill-buffer-hook'
(defun ein2:notebook-kill-buffer-callback ()
  "Call notebook destructor.  This function is called via `kill-buffer-hook'."
  (when (ein2:$notebook-p ein2:%notebook%)
    (ein2:notebook-close-worksheet ein2:%notebook% ein2:%worksheet%)))

(defun ein2:notebook-setup-kill-buffer-hook ()
  "Add \"notebook destructor\" to `kill-buffer-hook'."
  (add-hook 'kill-buffer-hook 'ein2:notebook-kill-buffer-callback))

;; Useful command to close notebooks.
(defun ein2:notebook-kill-all-buffers ()
  "Close all opened notebooks."
  (interactive)
  (let* ((notebooks (ein2:notebook-opened-notebooks))
         (unsaved (ein2:filter #'ein2:notebook-modified-p notebooks)))
    (if notebooks
        (if (y-or-n-p
             (format (concat "You have %s opened notebook(s). "
                             (when unsaved
                               (format "%s are UNSAVED. " (length unsaved)))
                             "Really kill all of them?")
                     (length notebooks)))
            (progn (ein2:log 'info "Killing all notebook buffers...")
                   (mapc #'ein2:notebook-close notebooks)
                   (ein2:log 'info "Killing all notebook buffers... Done!"))
          (ein2:log 'info "Canceled to kill all notebooks."))
      (ein2:log 'info "No opened notebooks."))))

(provide 'ein-notebook)

;;; ein-notebook.el ends here
