;;; ein-websocket.el --- Wrapper of websocket.el

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-websocket.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-websocket.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-websocket.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'websocket)

(require 'ein-core)


(defstruct ein2:$websocket
  "A wrapper object of `websocket'.

`ein2:$websocket-ws'               : an instance returned by `websocket-open'

`ein2:$websocket-onmessage'        : function called with (PACKET &rest ARGS)'
`ein2:$websocket-onclose'          : function called with (WEBSOCKET &rest ARGS)'
`ein2:$websocket-onopen'           : function called with (&rest ARGS)'

`ein2:$websocket-onmessage-args'   : optional arguments for onmessage callback'
`ein2:$websocket-onclose-args'     : optional arguments for onclose callback'
`ein2:$websocket-onopen-args'      : optional arguments for onopen callback'

`ein2:$websocket-closed-by-client' : t/nil'
"
  ws
  onmessage
  onmessage-args
  onclose
  onclose-args
  onopen
  onopen-args
  closed-by-client)


;; Issues opening websockets in IPython 2.0, think it is related to
;; http://stackoverflow.com/questions/22202182/error-on-websocket-when-try-to-use-ipython-notebook-in-emacs
(defun ein2:websocket (url &optional onmessage onclose onopen
                          onmessage-args onclose-args onopen-args)
  (let ((websocket (make-ein2:$websocket
                    :onmessage onmessage
                    :onclose onclose
                    :onopen onopen
                    :onmessage-args onmessage-args
                    :onclose-args onclose-args
                    :onopen-args onopen-args))
        (ws (websocket-open
             url
             :on-open
             (lambda (ws)
               (let ((websocket (websocket-client-data ws)))
                 (ein2:aif (ein2:$websocket-onopen websocket)
                     (apply it (ein2:$websocket-onopen-args websocket)))))
             :on-message
             (lambda (ws frame)
               (let ((websocket (websocket-client-data ws))
                     (packet (websocket-frame-payload frame)))
                 (ein2:aif (ein2:$websocket-onmessage websocket)
                     (when packet
                       (apply it packet
                              (ein2:$websocket-onmessage-args websocket))))))
             :on-close
             (lambda (ws)
               (let ((websocket (websocket-client-data ws)))
                 (ein2:aif (ein2:$websocket-onclose websocket)
                     (apply it websocket
                            (ein2:$websocket-onclose-args websocket))))))))
    (setf (websocket-client-data ws) websocket)
    (setf (ein2:$websocket-ws websocket) ws)
    websocket))


(defun ein2:websocket-open-p (websocket)
  (eql (websocket-ready-state (ein2:$websocket-ws websocket)) 'open))


(defun ein2:websocket-send (websocket text)
  ;;  (ein2:log 'info "WS: Sent message %s" text)
  (websocket-send-text (ein2:$websocket-ws websocket) text))


(defun ein2:websocket-close (websocket)
  (setf (ein2:$websocket-closed-by-client websocket) t)
  (websocket-close (ein2:$websocket-ws websocket)))


(provide 'ein-websocket)

;;; ein-websocket.el ends here
