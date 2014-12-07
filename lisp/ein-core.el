;;; ein-core.el --- EIN core

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-core.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-core.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-core.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

;; Optional dependency on tramp:
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")


(require 'ein2)  ; get autoloaded functions into namespace
(require 'ein-utils)


(defgroup ein nil
  "IPython notebook client in Emacs"
  :group 'applications
  :prefix "ein2:")

(defvar ein2:version "0.3"
  "Version number for Emacs IPython Notebook (EIN).")


;;; Configuration

(defcustom ein2:url-or-port '(8888)
  "List of default url-or-port values.
This will be used for completion. So put your IPython servers.
You can connect to servers not in this list \(but you will need
to type every time)."
  :type '(repeat (choice (integer :tag "Port number" 8888)
                         (string :tag "URL" "http://127.0.0.1:8888")))
  :group 'ein)

(defcustom ein2:default-url-or-port nil
  "Default URL or port.  This should be your main IPython
Notebook server."
  :type '(choice (integer :tag "Port number" 8888)
                 (string :tag "URL" "http://127.0.0.1:8888")
                 (const :tag "First value of `ein2:url-or-port'" nil))
  :group 'ein)

(defcustom ein2:filename-translations nil
  "Convert file paths between Emacs and Python process.

This value can take these form:

alist
    Its key specifies URL-OR-PORT and value must be a list of two
    functions: (TO-PYTHON FROM-PYTHON).  Key (URL-OR-PORT) can be
    string (URL), integer (port), or `default' (symbol).  The
    value of `default' is used when other key does not much.
function
    Called with an argument URL-OR-PORT (integer or string).
    This function must return a list of two functions:
    (TO-PYTHON FROM-PYTHON).

Here, the functions TO-PYTHON and FROM-PYTHON are defined as:

TO-PYTHON
    A function which converts a file name (returned by
    `buffer-file-name') to the one Python understands.
FROM-PYTHON
    A function which converts a file path returned by
    Python process to the one Emacs understands.

Use `ein2:tramp-create-filename-translator' to easily generate the
pair of TO-PYTHON and FROM-PYTHON."
  ;; I've got the idea from `slime-filename-translations'.
  :type '(choice
          (alist :tag "Translations mapping"
                 :key-type (choice :tag "URL or PORT"
                                   (string :tag "URL" "http://127.0.0.1:8888")
                                   (integer :tag "PORT" 8888)
                                   (const default))
                 :value-type (list (function :tag "TO-PYTHON")
                                   (function :tag "FROM-PYTHON")))
          (function :tag "Translations getter"))
  :group 'ein)



;;; Constants

(defvar ein2:source-dir (file-name-directory load-file-name)
  "Directory in which ``ein*.el`` locate.")


;;; Configuration getter

(defun ein2:default-url-or-port ()
  (or ein2:default-url-or-port (car ein2:url-or-port) 8888))

(defun ein2:version ()
  "Return a string containing `ein2:version' and git revision if
the source is in git repository."
  (ein2:aif (when (ein2:git-root-p
                  (concat (file-name-as-directory ein2:source-dir) ".."))
             (let ((default-directory ein2:source-dir))
               (ein2:git-revision-dirty)))
      (concat ein2:version "." it)
    ein2:version))

(defun ein2:query-ipython-version (&optional url-or-port)
  (let ((resp (request (ein2:url (or url-or-port
                                    (ein2:default-url-or-port))
                                "api")
                       :parser #'ein2:json-read
                       :timeout 0.5
                       :sync t)))
    (if (eql 404 (request-response-status-code resp))
        (progn
          (ein2:log 'warn "Version api not implemented, assuming we are working with IPython 2.x")
          2)
      (string-to-number (first (split-string (plist-get (request-response-data resp) :version) "[\\.]"))))))


;;; File name translation (tramp support)

;; Probably it's better to define `ein2:filename-translations-get' as
;; an EIEIO method so that I don't have to re-define functions such as
;; `ein2:kernel-filename-to-python' and `ein2:kernel-filename-from-python'.

(defun ein2:filename-translations-get (url-or-port)
  (ein2:choose-setting 'ein2:filename-translations url-or-port))

(defun ein2:filename-to-python (url-or-port filename)
  (ein2:aif (car (ein2:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein2:filename-from-python (url-or-port filename)
  (ein2:aif (cadr (ein2:filename-translations-get url-or-port))
      (funcall it filename)
    filename))

(defun ein2:make-tramp-file-name (username remote-host python-filename)
  "Old (with multi-hops) tramp compatibility function.
Adapted from `slime-make-tramp-file-name'."
  (if (boundp 'tramp-multi-methods)
      (tramp-make-tramp-file-name nil nil
                                  username
                                  remote-host
                                  python-filename)
    (tramp-make-tramp-file-name nil
                                username
                                remote-host
                                python-filename)))

(defun ein2:tramp-create-filename-translator (remote-host &optional username)
  "Generate a pair of TO-PYTHON and FROM-PYTHON for
`ein2:filename-translations'.

Usage::

    (setq ein2:filename-translations
          `((8888
             . ,(ein2:tramp-create-filename-translator \"MY-HOSTNAME\"))))
    ;; Equivalently:
    (setq ein2:filename-translations
          (lambda (url-or-port)
            (when (equal url-or-port 8888)
              (ein2:tramp-create-filename-translator \"MY-HOSTNAME\"))))

This setting assumes that the IPython server which can be
connected using the port 8888 in localhost is actually running in
the host named MY-HOSTNAME.

Adapted from `slime-create-filename-translator'."
  (require 'tramp)
  (lexical-let ((remote-host remote-host)
                (username (or username (user-login-name))))
    (list (lambda (emacs-filename)
            (tramp-file-name-localname
             (tramp-dissect-file-name emacs-filename)))
          (lambda (python-filename)
             (ein2:make-tramp-file-name username remote-host python-filename)))))



;;; Generic getter

(defun ein2:generic-getter (func-list)
  "Internal function for generic getter functions (`ein2:get-*').

FUNC-LIST is a list of function which takes no argument and
return what is desired or nil.  Each function in FUNC-LIST is
called one by one and the first non-nil result will be used.  The
function is not called when it is not bound.  So, it is safe to
give functions defined in lazy-loaded sub-modules.

This is something similar to dispatching in generic function such
as `defgeneric' in EIEIO, but it takes no argument.  Actual
implementation is chosen based on context (buffer, point, etc.).
This helps writing generic commands which requires same object
but can operate in different contexts."
  (loop for func in func-list
        if (and (functionp func) (funcall func))
        return it))

(defun ein2:get-url-or-port ()
  (ein2:generic-getter '(ein2:get-url-or-port--notebooklist
                        ein2:get-url-or-port--notebook
                        ein2:get-url-or-port--worksheet
                        ein2:get-url-or-port--shared-output
                        ein2:get-url-or-port--connect)))

(defun ein2:get-notebook ()
  (ein2:generic-getter '(ein2:get-notebook--notebook
                        ;; ein2:get-notebook--shared-output
                        ein2:get-notebook--connect)))

(defun ein2:get-notebook-or-error ()
  (or (ein2:get-notebook)
      (error "No notebook related to the current buffer.")))

(defun ein2:get-kernel ()
  (ein2:generic-getter '(ein2:get-kernel--notebook
                        ein2:get-kernel--worksheet
                        ein2:get-kernel--shared-output
                        ein2:get-kernel--connect)))

(defun ein2:get-kernel-or-error ()
  (or (ein2:get-kernel)
      (error "No kernel related to the current buffer.")))

(defun ein2:get-cell-at-point ()
  (ein2:generic-getter '(ein2:get-cell-at-point--worksheet
                        ein2:get-cell-at-point--shared-output)))

(defun ein2:get-traceback-data ()
  (ein2:generic-getter '(ein2:get-traceback-data--worksheet
                        ein2:get-traceback-data--shared-output
                        ein2:get-traceback-data--connect)))



;;; Emacs utilities

(defun ein2:byte-compile-ein ()
  "Byte compile EIN files."
  (interactive)
  (let* ((files (directory-files ein2:source-dir 'full "^ein-.*\\.el$"))
         (errors (ein2:filter
                  'identity
                  (mapcar (lambda (f) (unless (byte-compile-file f) f))
                          files))))
    (ein2:aif errors
        (error "Got %s errors while compiling these files: %s"
               (length errors)
               (ein2:join-str " " (mapcar #'file-name-nondirectory it))))
    (message "Compiled %s files" (length files))))


(provide 'ein-core)

;;; ein-core.el ends here
