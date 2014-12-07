;;; ein-kernel.el --- Communicate with IPython notebook server

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-kernel.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-kernel.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-kernel.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'ansi-color)

(require 'ein-core)
(require 'ein-log)
;; FIXME: use websocket.el directly once v1.0 is released.
(require 'ein-websocket)
(require 'ein-events)
(require 'ein-query)


;; FIXME: Rewrite `ein2:$kernel' using `defclass'.  It should ease
;;        testing since I can mock I/O using method overriding.
(defstruct ein2:$kernel
  "Hold kernel variables.

`ein2:$kernel-url-or-port'
  URL or port of IPython server.
"
  url-or-port
  events
  api-version
  session-id
  kernel-id
  shell-channel
  iopub-channel
  base-url                              ; /api/kernels/
  kernel-url                            ; /api/kernels/<KERNEL-ID>
  ws-url                                ; ws://<URL>[:<PORT>]
  running
  username
  msg-callbacks
  ;; FIXME: Use event instead of hook.
  after-start-hook
  after-execute-hook)

;; "Public" getters.  Use them outside of this package.

(defun ein2:$kernel-session-url (kernel)
  (concat "/api/sessions/" (ein2:$kernel-session-id kernel)))

;;;###autoload
(defalias 'ein2:kernel-url-or-port 'ein2:$kernel-url-or-port)

;;;###autoload
(defalias 'ein2:kernel-id 'ein2:$kernel-kernel-id)


;;; Initialization and connection.

(defun ein2:kernel-new (url-or-port base-url events &optional api-version)
  (make-ein2:$kernel
   :url-or-port url-or-port
   :events events
   :api-version (or api-version 2)
   :session-id (ein2:utils-uuid)
   :kernel-id nil
   :shell-channel nil
   :iopub-channel nil
   :base-url base-url
   :running nil
   :username "username"
   :msg-callbacks (make-hash-table :test 'equal)))


(defun ein2:kernel-del (kernel)
  "Destructor for `ein2:$kernel'."
  (ein2:kernel-stop-channels kernel))


(defun ein2:kernel--get-msg (kernel msg-type content)
  (list
   :header (list
            :msg_id (ein2:utils-uuid)
            :username (ein2:$kernel-username kernel)
            :session (ein2:$kernel-session-id kernel)
            :msg_type msg-type)
   :metadata (make-hash-table)
   :content content
   :parent_header (make-hash-table)))


(defun ein2:kernel-start (kernel notebook-id &optional path)
  "Start kernel of the notebook whose id is NOTEBOOK-ID."
  (unless (ein2:$kernel-running kernel)
    (if (not path)
        (setq path ""))
    (ein2:query-singleton-ajax
     (list 'kernel-start (ein2:$kernel-kernel-id kernel))
     ;;(concat
     (ein2:url (ein2:$kernel-url-or-port kernel)
              ;;(ein2:$kernel-base-url kernel))
              ;;"?" (format "notebook=%s" notebook-id)
              "api/sessions")
     :type "POST"
     :data (json-encode `(("notebook" .
                           (("name" . ,notebook-id)
                            ("path" . ,path)))))
     :parser #'ein2:json-read
     :success (apply-partially #'ein2:kernel--kernel-started kernel))))


(defun ein2:kernel-restart (kernel)
  (ein2:events-trigger (ein2:$kernel-events kernel)
                      'status_restarting.Kernel)
  (ein2:log 'info "Restarting kernel")
  (when (ein2:$kernel-running kernel)
    (ein2:kernel-stop-channels kernel)
    (ein2:query-singleton-ajax
     (list 'kernel-restart (ein2:$kernel-kernel-id kernel))
     (ein2:url (ein2:$kernel-url-or-port kernel)
              (ein2:$kernel-kernel-url kernel)
              "restart")
     :type "POST"
     :parser #'ein2:json-read
     :success (apply-partially #'ein2:kernel--kernel-started kernel))))


(defun* ein2:kernel--kernel-started (kernel &key data &allow-other-keys)
  (let ((session-id (plist-get data :id)))
    (if (plist-get data :kernel)
        (setq data (plist-get data :kernel)))
    (destructuring-bind (&key id &allow-other-keys) data
      (unless id
        (error "Failed to start kernel.  No `kernel_id' or `ws_url'.  Got %S."
               data))
      (ein2:log 'info "Kernel started: %s" id)
      (setf (ein2:$kernel-running kernel) t)
      (setf (ein2:$kernel-kernel-id kernel) id)
      (setf (ein2:$kernel-session-id kernel) session-id)
      (setf (ein2:$kernel-ws-url kernel) (ein2:kernel--ws-url kernel id))
      (setf (ein2:$kernel-kernel-url kernel)
            (concat (ein2:$kernel-base-url kernel) "/" id)))
    (ein2:kernel-start-channels kernel)
    (let ((shell-channel (ein2:$kernel-shell-channel kernel))
          (iopub-channel (ein2:$kernel-iopub-channel kernel)))
      ;; FIXME: get rid of lexical-let
      (lexical-let ((kernel kernel))
        (setf (ein2:$websocket-onmessage shell-channel)
              (lambda (packet)
                (ein2:kernel--handle-shell-reply kernel packet)))
        (setf (ein2:$websocket-onmessage iopub-channel)
              (lambda (packet)
                (ein2:kernel--handle-iopub-reply kernel packet)))))))


(defun ein2:kernel--ws-url (kernel ws_url)
  "Use `ein2:$kernel-url-or-port' if WS_URL is an empty string.
See: https://github.com/ipython/ipython/pull/3307"
  (if (string-match-p "^wss?://" ws_url)
      ws_url
    (let ((ein2:url-localhost-template "ws://127.0.0.1:%s"))
      (ein2:url (ein2:$kernel-url-or-port kernel)))))


(defun ein2:kernel--websocket-closed (kernel ws-url early)
  (if early
      (ein2:display-warning
       "Websocket connection to %s could not be established.
  You will NOT be able to run code.  Your websocket.el may not be
  compatible with the websocket version in the server, or if the
  url does not look right, there could be an error in the
  server's configuration." ws-url)
    (ein2:display-warning "Websocket connection closed unexpectedly.
  The kernel will no longer be responsive.")))


(defun ein2:kernel-send-cookie (channel host)
  ;; cookie can be an empty string for IPython server with no password,
  ;; but something must be sent to start channel.
  (let ((cookie (ein2:query-get-cookie host "/")))
    (ein2:websocket-send channel cookie)))


(defun ein2:kernel--ws-closed-callback (websocket kernel arg)
  ;; NOTE: The argument ARG should not be "unpacked" using `&rest'.
  ;; It must be given as a list to hold state `already-called-onclose'
  ;; so it can be modified in this function.
  (destructuring-bind (&key already-called-onclose ws-url early)
      arg
    (unless already-called-onclose
      (plist-put arg :already-called-onclose t)
      (unless (ein2:$websocket-closed-by-client websocket)
        ;; Use "event-was-clean" when it is implemented in websocket.el.
        (ein2:kernel--websocket-closed kernel ws-url early)))))


(defun ein2:kernel-start-channels (kernel)
  (ein2:kernel-stop-channels kernel)
  (let* ((api-version (ein2:$kernel-api-version kernel))
         (ws-url (concat (ein2:$kernel-ws-url kernel)
                         (ein2:$kernel-kernel-url kernel)))
         (shell-session-url (concat ws-url "/shell?session_id="
                                   (ein2:$kernel-session-id kernel)))
         (iopub-session-url (concat ws-url "/iopub?session_id="
                                    (ein2:$kernel-session-id kernel)))
         (onclose-arg (list :ws-url ws-url
                            :already-called-onclose nil
                            :early t)))
    (ein2:log 'info "Starting session WS: %S" shell-session-url)
    (ein2:log 'info "Starting iopub WS: %S" iopub-session-url)
    (setf (ein2:$kernel-shell-channel kernel)
          (cond ((= api-version 3)
                 (ein2:websocket shell-session-url))
                (t
                 (ein2:websocket (concat ws-url "/shell")))))
    (setf (ein2:$kernel-iopub-channel kernel)
          (cond ((= api-version 3)
                 (ein2:websocket iopub-session-url))
                (t
                 (ein2:websocket (concat ws-url "/iopub")))))
    
    (loop for c in (list (ein2:$kernel-shell-channel kernel)
                         (ein2:$kernel-iopub-channel kernel))
          do (setf (ein2:$websocket-onclose-args c) (list kernel onclose-arg))
          do (setf (ein2:$websocket-onopen c)
                   (lexical-let ((channel c)
                                 (kernel kernel)
                                 (api-version api-version)
                                 (host (let (url-or-port
                                             (ein2:$kernel-url-or-port kernel))
                                         (if (stringp url-or-port)
                                             url-or-port
                                           ein2:url-localhost))))
                     (lambda ()
                       (cond ((= api-version 2)
                              (ein2:kernel-send-cookie channel host))
                             ((= api-version 3)
                              (ein2:kernel-connect-request kernel (list :kernel_connect_reply (cons 'ein2:kernel-on-connect kernel))))
                             )
                       ;; run `ein2:$kernel-after-start-hook' if both
                       ;; channels are ready.
                       (when (ein2:kernel-live-p kernel)
                         (ein2:kernel-run-after-start-hook kernel)))))
          do (setf (ein2:$websocket-onclose c)
                   #'ein2:kernel--ws-closed-callback))
    
    ;; switch from early-close to late-close message after 1s
    (run-at-time
     1 nil
     (lambda (onclose-arg)
       (plist-put onclose-arg :early nil)
       (ein2:log 'debug "(via run-at-time) onclose-arg changed to: %S"
                onclose-arg))
     onclose-arg)))

;; NOTE: `onclose-arg' can be accessed as:
;; (nth 1 (ein2:$websocket-onclose-args (ein2:$kernel-shell-channel (ein2:$notebook-kernel ein2:notebook))))

(defun ein2:kernel-on-connect (kernel content -metadata-not-used-)
  (ein2:log 'info "Kernel connect_request_reply received."))

(defun ein2:kernel-run-after-start-hook (kernel)
  (ein2:log 'debug "EIN2:KERNEL-RUN-AFTER-START-HOOK")
  (mapc #'ein2:funcall-packed
        (ein2:$kernel-after-start-hook kernel)))


(defun ein2:kernel-stop-channels (kernel)
  (when (ein2:$kernel-shell-channel kernel)
    (setf (ein2:$websocket-onclose (ein2:$kernel-shell-channel kernel)) nil)
    (ein2:websocket-close (ein2:$kernel-shell-channel kernel))
    (setf (ein2:$kernel-shell-channel kernel) nil))
  (when (ein2:$kernel-iopub-channel kernel)
    (setf (ein2:$websocket-onclose (ein2:$kernel-iopub-channel kernel)) nil)
    (ein2:websocket-close (ein2:$kernel-iopub-channel kernel))
    (setf (ein2:$kernel-iopub-channel kernel) nil)))


(defun ein2:kernel-live-p (kernel)
  (and
   (ein2:$kernel-p kernel)
   (ein2:aand (ein2:$kernel-shell-channel kernel) (ein2:websocket-open-p it))
   (ein2:aand (ein2:$kernel-iopub-channel kernel) (ein2:websocket-open-p it))))


(defmacro ein2:kernel-if-ready (kernel &rest body)
  "Execute BODY if KERNEL is ready.  Warn user otherwise."
  (declare (indent 1))
  `(if (ein2:kernel-live-p ,kernel)
       (progn ,@body)
     (ein2:log 'warn "Kernel is not ready yet! (or closed already.)")))


;;; Main public methods

;; NOTE: The argument CALLBACKS for the following functions is almost
;;       same as the JS implementation in IPython.  However, as Emacs
;;       lisp does not support closure, value is "packed" using
;;       `cons': `car' is the actual callback function and `cdr' is
;;       its first argument.  It's like using `cons' instead of
;;       `$.proxy'.

(defun ein2:kernel-object-info-request (kernel objname callbacks)
  "Send object info request of OBJNAME to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:object_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `object_into_reply' message.

`object_into_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#object-information
"
  (assert (ein2:kernel-live-p kernel) nil "object_info_reply: Kernel is not active.")
  (when objname
    (let* ((content (list :oname (format "%s" objname)))
           (msg (ein2:kernel--get-msg kernel "object_info_request" content))
           (msg-id (plist-get (plist-get msg :header) :msg_id)))
      (ein2:websocket-send
       (ein2:$kernel-shell-channel kernel)
       (json-encode msg))
      (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
      msg-id)))


(defun* ein2:kernel-execute (kernel code &optional callbacks
                                   &key
                                   (silent t)
                                   (user-variables [])
                                   (user-expressions (make-hash-table))
                                   (allow-stdin json-false))
  "Execute CODE on KERNEL.

When calling this method pass a CALLBACKS structure of the form:

  (:execute_reply  EXECUTE-REPLY-CALLBACK
   :output         OUTPUT-CALLBACK
   :clear_output   CLEAR-OUTPUT-CALLBACK
   :set_next_input SET-NEXT-INPUT)

Objects end with -CALLBACK above must pack a FUNCTION and its
first ARGUMENT in a `cons'::

  (FUNCTION . ARGUMENT)

Call signature
--------------
::

  (`funcall' EXECUTE-REPLY-CALLBACK ARGUMENT          CONTENT METADATA)
  (`funcall' OUTPUT-CALLBACK        ARGUMENT MSG-TYPE CONTENT METADATA)
  (`funcall' CLEAR-OUTPUT-CALLBACK  ARGUMENT          CONTENT METADATA)
  (`funcall' SET-NEXT-INPUT         ARGUMENT TEXT)

* Both CONTENT and METADATA objects are plist.
* The MSG-TYPE argument for OUTPUT-CALLBACK is a string
  (one of `stream', `display_data', `pyout' and `pyerr').
* The CONTENT object for CLEAR-OUTPUT-CALLBACK has
  `stdout', `stderr' and `other' fields that are booleans.
* The SET-NEXT-INPUT callback will be passed the `set_next_input' payload,
  which is a string.
  See `ein2:kernel--handle-shell-reply' for how the callbacks are called.

Links
-----
* For general description of CONTENT and METADATA:
  http://ipython.org/ipython-doc/dev/development/messaging.html#general-message-format
* `execute_reply' message is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#execute
* Output type messages is documented here:
  http://ipython.org/ipython-doc/dev/development/messaging.html#messages-on-the-pub-sub-socket

Sample implementations
----------------------
* `ein2:cell--handle-execute-reply'
* `ein2:cell--handle-output'
* `ein2:cell--handle-clear-output'
* `ein2:cell--handle-set-next-input'
"
  ;; FIXME: Consider changing callback to use `&key'.
  ;;        Otherwise, adding new arguments to callback requires
  ;;        backward incompatible changes (hence a big diff), unlike
  ;;        Javascript.  Downside of this is that there is no short way
  ;;        to write anonymous callback because there is no `lambda*'.
  ;;        You can use `function*', but that's bit long...

  ;; FIXME: Consider allowing a list of fixed argument so that the
  ;;        call signature becomes something like:
  ;;           (funcall FUNCTION [ARG ...] CONTENT METADATA)

  (assert (ein2:kernel-live-p kernel) nil "execute_reply: Kernel is not active.")
  (let* ((content (list
                   :code code
                   :silent (or silent json-false)
                   :user_variables user-variables
                   :user_expressions user-expressions
                   :allow_stdin allow-stdin))
         (msg (ein2:kernel--get-msg kernel "execute_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein2:websocket-send
     (ein2:$kernel-shell-channel kernel)
     (json-encode msg))
    (unless (plist-get callbacks :execute_reply)
      (ein2:log 'debug "code: %s" code))
    (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    (unless silent
      (mapc #'ein2:funcall-packed
            (ein2:$kernel-after-execute-hook kernel)))
    msg-id))


(defun ein2:kernel-complete (kernel line cursor-pos callbacks)
  "Complete code at CURSOR-POS in a string LINE on KERNEL.

CURSOR-POS is the position in the string LINE, not in the buffer.

When calling this method pass a CALLBACKS structure of the form:

    (:complete_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `complete_reply' message.

`complete_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#complete
"
  (assert (ein2:kernel-live-p kernel) nil "complete_reply: Kernel is not active.")
  (let* ((content (list
                   :text ""
                   :line line
                   :cursor_pos cursor-pos))
         (msg (ein2:kernel--get-msg kernel "complete_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein2:websocket-send
     (ein2:$kernel-shell-channel kernel)
     (json-encode msg))
    (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun* ein2:kernel-history-request (kernel callbacks
                                           &key
                                           (output nil)
                                           (raw t)
                                           (hist-access-type "tail")
                                           session
                                           start
                                           stop
                                           (n 10)
                                           pattern
                                           unique)
  "Request execution history to KERNEL.

When calling this method pass a CALLBACKS structure of the form:

    (:history_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `history_reply' message.

`history_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#history

Relevant Python code:

* :py:method:`IPython.zmq.ipkernel.Kernel.history_request`
* :py:class:`IPython.core.history.HistoryAccessor`
"
  (assert (ein2:kernel-live-p kernel) nil "history_reply: Kernel is not active.")
  (let* ((content (list
                   :output (ein2:json-any-to-bool output)
                   :raw (ein2:json-any-to-bool raw)
                   :hist_access_type hist-access-type
                   :session session
                   :start start
                   :stop stop
                   :n n
                   :pattern pattern
                   :unique unique))
         (msg (ein2:kernel--get-msg kernel "history_request" content))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein2:websocket-send
     (ein2:$kernel-shell-channel kernel)
     (json-encode msg))
    (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))

(defun ein2:kernel-connect-request (kernel callbacks)
  "Request basic information for a KERNEL.

When calling this method pass a CALLBACKS structure of the form::

  (:connect_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `kernel_info_reply' message.

`connect_request' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#connect

Example::

  (ein2:kernel-connect-request
   (ein2:get-kernel)
   '(:kernel_connect_reply (message . \"CONTENT: %S\\nMETADATA: %S\")))
"
  (assert (ein2:kernel-live-p kernel) nil "connect_reply: Kernel is not active.")
  (let* ((msg (ein2:kernel--get-msg kernel "connect_request" (make-hash-table)))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein2:websocket-send
     (ein2:$kernel-shell-channel kernel)
     (json-encode msg))
    (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))

(defun ein2:kernel-kernel-info-request (kernel callbacks)
  "Request core information of KERNEL.

When calling this method pass a CALLBACKS structure of the form::

  (:kernel_info_reply (FUNCTION . ARGUMENT))

Call signature::

  (`funcall' FUNCTION ARGUMENT CONTENT METADATA)

CONTENT and METADATA are given by `kernel_info_reply' message.

`kernel_info_reply' message is documented here:
http://ipython.org/ipython-doc/dev/development/messaging.html#kernel-info

Example::

  (ein2:kernel-kernel-info-request
   (ein2:get-kernel)
   '(:kernel_info_reply (message . \"CONTENT: %S\\nMETADATA: %S\")))
"
  (assert (ein2:kernel-live-p kernel) nil "kernel_info_reply: Kernel is not active.")
  (let* ((msg (ein2:kernel--get-msg kernel "kernel_info_request" nil))
         (msg-id (plist-get (plist-get msg :header) :msg_id)))
    (ein2:websocket-send
     (ein2:$kernel-shell-channel kernel)
     (json-encode msg))
    (ein2:kernel-set-callbacks-for-msg kernel msg-id callbacks)
    msg-id))


(defun ein2:kernel-interrupt (kernel)
  (when (ein2:$kernel-running kernel)
    (ein2:log 'info "Interrupting kernel")
    (ein2:query-singleton-ajax
     (list 'kernel-interrupt (ein2:$kernel-kernel-id kernel))
     (ein2:url (ein2:$kernel-url-or-port kernel)
              (ein2:$kernel-kernel-url kernel)
              "interrupt")
     :type "POST"
     :success (lambda (&rest ignore)
                (ein2:log 'info "Sent interruption command.")))))


(defun ein2:kernel-kill (kernel &optional callback cbargs)
  (when (ein2:$kernel-running kernel)
    (ein2:query-singleton-ajax
     (list 'kernel-kill (ein2:$kernel-kernel-id kernel))
     (ein2:url (ein2:$kernel-url-or-port kernel)
              (ein2:$kernel-kernel-url kernel))
     :type "DELETE"
     :success (apply-partially
               (lambda (kernel callback cbargs &rest ignore)
                 (ein2:log 'info "Notebook kernel is killed")
                 (setf (ein2:$kernel-running kernel) nil)
                 (when callback (apply callback cbargs)))
               kernel callback cbargs))))


;; Reply handlers.

(defun ein2:kernel-get-callbacks-for-msg (kernel msg-id)
  (gethash msg-id (ein2:$kernel-msg-callbacks kernel)))

(defun ein2:kernel-set-callbacks-for-msg (kernel msg-id callbacks)
  (puthash msg-id callbacks (ein2:$kernel-msg-callbacks kernel)))

(defun ein2:kernel--handle-shell-reply (kernel packet)
  (ein2:log 'debug "KERNEL--HANDLE-SHELL-REPLY")
  (destructuring-bind
      (&key header content metadata parent_header &allow-other-keys)
      (ein2:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (msg-id (plist-get parent_header :msg_id))
           (callbacks (ein2:kernel-get-callbacks-for-msg kernel msg-id))
           (cb (plist-get callbacks (intern (format ":%s" msg-type)))))
      (ein2:log 'debug "KERNEL--HANDLE-SHELL-REPLY: msg_type = %s" msg-type)
      (if cb
          (ein2:funcall-packed cb content metadata)
        (ein2:log 'debug "no callback for: msg_type=%s msg_id=%s"
                 msg-type msg-id))
      (ein2:aif (plist-get content :payload)
          (ein2:kernel--handle-payload kernel callbacks it))
      (let ((events (ein2:$kernel-events kernel)))
        (ein2:case-equal msg-type
          (("execute_reply")
           (ein2:aif (plist-get content :execution_count)
               ;; It can be `nil' for silent execution
               (ein2:events-trigger events 'execution_count.Kernel it)))))))
  (ein2:log 'debug "KERNEL--HANDLE-SHELL-REPLY: finished"))

(defun ein2:kernel--handle-payload (kernel callbacks payload)
  (loop with events = (ein2:$kernel-events kernel)
        for p in payload
        for text = (plist-get p :text)
        for source = (plist-get p :source)
        if (member source '("IPython.kernel.zmq.page.page"
                            "IPython.zmq.page.page"
                            "page"))
        do (when (not (equal (ein2:trim text) ""))
             (ein2:events-trigger
              events 'open_with_text.Pager (list :text text)))
        else if
        (member
         source
         '("IPython.kernel.zmq.zmqshell.ZMQInteractiveShell.set_next_input"
           "IPython.zmq.zmqshell.ZMQInteractiveShell.set_next_input"))
        do (let ((cb (plist-get callbacks :set_next_input)))
             (when cb (ein2:funcall-packed cb text)))))

(defun ein2:kernel--handle-iopub-reply (kernel packet)
  (ein2:log 'debug "KERNEL--HANDLE-IOPUB-REPLY")
  (destructuring-bind
      (&key content metadata parent_header header &allow-other-keys)
      (ein2:json-read-from-string packet)
    (let* ((msg-type (plist-get header :msg_type))
           (callbacks (ein2:kernel-get-callbacks-for-msg
                       kernel (plist-get parent_header :msg_id)))
           (events (ein2:$kernel-events kernel)))
      (ein2:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: msg_type = %s" msg-type)
      (if (and (not (equal msg-type "status")) (null callbacks))
          (ein2:log 'verbose "Got message not from this notebook.")
        (ein2:case-equal msg-type
          (("stream" "display_data" "pyout" "pyerr" "execute_result")
           (ein2:aif (plist-get callbacks :output)
               (ein2:funcall-packed it msg-type content metadata)))
          (("status")
           (ein2:case-equal (plist-get content :execution_state)
             (("busy")
              (ein2:events-trigger events 'status_busy.Kernel))
             (("idle")
              (ein2:events-trigger events 'status_idle.Kernel))
             (("dead")
              (ein2:kernel-stop-channels kernel)
              (ein2:events-trigger events 'status_dead.Kernel))))
          (("clear_output")
           (ein2:aif (plist-get callbacks :clear_output)
               (ein2:funcall-packed it content metadata)))))))
  (ein2:log 'debug "KERNEL--HANDLE-IOPUB-REPLY: finished"))


;;; Utility functions

(defun ein2:kernel-filename-to-python (kernel filename)
  "See: `ein2:filename-to-python'."
  (ein2:filename-to-python (ein2:$kernel-url-or-port kernel) filename))

(defun ein2:kernel-filename-from-python (kernel filename)
  "See: `ein2:filename-from-python'."
  (ein2:filename-from-python (ein2:$kernel-url-or-port kernel) filename))

(defun ein2:kernel-construct-defstring (content)
  "Construct call signature from CONTENT of ``:object_info_reply``.
Used in `ein2:pytools-finish-tooltip', etc."
  (or (plist-get content :call_def)
      (plist-get content :init_definition)
      (plist-get content :definition)))

(defun ein2:kernel-construct-help-string (content)
  "Construct help string from CONTENT of ``:object_info_reply``.
Used in `ein2:pytools-finish-tooltip', etc."
  (ein2:log 'debug "KERNEL-CONSTRUCT-HELP-STRING")
  (let* ((defstring (ein2:aand
                     (ein2:kernel-construct-defstring content)
                     (ansi-color-apply it)
                     (ein2:string-fill-paragraph it)))
         (docstring (ein2:aand
                     (or (plist-get content :call_docstring)
                         (plist-get content :init_docstring)
                         (plist-get content :docstring)
                         ;; "<empty docstring>"
                         )
                     (ansi-color-apply it)))
         (help (ein2:aand
                (ein2:filter 'identity (list defstring docstring))
                (ein2:join-str "\n" it))))
    (ein2:log 'debug "KERNEL-CONSTRUCT-HELP-STRING: help=%s" help)
    help))

(defun ein2:kernel-request-stream (kernel code func &optional args)
  "Run lisp callback FUNC with the output stream returned by Python CODE.

The first argument to the lisp function FUNC is the stream output
as a string and the rest of the argument is the optional ARGS."
  (ein2:kernel-execute
   kernel
   code
   (list :output (cons (lambda (packed msg-type content -metadata-not-used-)
                         (let ((func (car packed))
                               (args (cdr packed)))
                           (when (equal msg-type "stream")
                             (ein2:aif (plist-get content :data)
                                 (apply func it args)))))
                       (cons func args)))))

(defun* ein2:kernel-history-request-synchronously
    (kernel &rest args &key (timeout 0.5) (tick-time 0.05) &allow-other-keys)
  "Send the history request and wait TIMEOUT seconds.
Return a list (CONTENT METADATA).
This function checks the request reply every TICK-TIME seconds.
See `ein2:kernel-history-request' for other usable options."
  ;; As `result' and `finished' are set in callback, make sure they
  ;; won't be trapped in other let-bindings.
  (lexical-let (result finished)
    (apply
     #'ein2:kernel-history-request
     kernel
     (list :history_reply
           (cons (lambda (-ignore- content metadata)
                   (setq result (list content metadata))
                   (setq finished t))
                 nil))
     args)
    (loop repeat (floor (/ timeout tick-time))
          do (sit-for tick-time)
          when finished
          return t
          finally (error "Timeout"))
    result))

(defun ein2:kernel-history-search-synchronously (kernel pattern &rest args)
  "Search execution history in KERNEL using PATTERN.
Return matched history as a list of strings.
See `ein2:kernel-history-request-synchronously' and
`ein2:kernel-history-request' for usable options."
  (let ((reply
         (apply #'ein2:kernel-history-request-synchronously
                kernel
                :hist-access-type "search"
                :pattern pattern
                args)))
    (mapcar #'caddr (plist-get (car reply) :history))))

(provide 'ein-kernel)

;;; ein-kernel.el ends here
