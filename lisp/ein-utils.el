;;; ein-utils.el --- Utility module

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-utils.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-utils.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-utils.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'cc-mode)
(require 'json)


;;; Macros and core functions/variables

(defmacro ein2:aif (test-form then-form &rest else-forms)
  "Anaphoric IF.  Adapted from `e2wm:aif'."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ein2:aif 'lisp-indent-function 2)

(defmacro ein2:aand (test &rest rest)
  "Anaphoric AND.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(ein2:aand ,@rest)) 'it))))

(defmacro ein2:and-let* (bindings &rest form)
  "Gauche's `and-let*'."
  (declare (debug ((&rest &or symbolp (form) (gate symbolp &optional form))
                   body))
           ;; See: (info "(elisp) Specification List")
           (indent 1))
  (if (null bindings)
      `(progn ,@form)
    (let* ((head (car bindings))
           (tail (cdr bindings))
           (rest (macroexpand-all `(ein2:and-let* ,tail ,@form))))
      (cond
       ((symbolp head) `(if ,head ,rest))
       ((= (length head) 1) `(if ,(car head) ,rest))
       (t `(let (,head) (if ,(car head) ,rest)))))))

(defmacro ein2:deflocal (name &optional initvalue docstring)
  "Define permanent buffer local variable named NAME.
INITVALUE and DOCSTRING are passed to `defvar'."
  (declare (indent defun)
           (doc-string 3))
  `(progn
     (defvar ,name ,initvalue ,docstring)
     (make-variable-buffer-local ',name)
     (put ',name 'permanent-local t)))

(defmacro ein2:with-read-only-buffer (buffer &rest body)
  (declare (indent 1))
  `(with-current-buffer ,buffer
     (setq buffer-read-only t)
     (save-excursion
       (let ((inhibit-read-only t))
         ,@body))))

(defmacro ein2:with-live-buffer (buffer &rest body)
  "Execute BODY in BUFFER if BUFFER is alive."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer)
     (with-current-buffer ,buffer
       ,@body)))

(defmacro ein2:with-possibly-killed-buffer (buffer &rest body)
  "Execute BODY in BUFFER if BUFFER is live.
Execute BODY if BUFFER is not live anyway."
  (declare (indent 1) (debug t))
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer
         ,@body)
     ,@body))

(defvar ein2:dotty-syntax-table
  (let ((table (make-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Adapted from `python-dotty-syntax-table'.")

(defun ein2:object-at-point ()
  "Return dotty.words.at.point.
When region is active, text in region is returned after trimmed
white spaces, newlines and dots.
When object is not found at the point, return the object just
before previous opening parenthesis."
  ;; For auto popup tooltip (or something like eldoc), probably it is
  ;; better to return function (any word before "(").  I should write
  ;; another function or add option to this function when the auto
  ;; popup tooltip is implemented.
  (if (region-active-p)
      (ein2:trim (buffer-substring (region-beginning) (region-end))
                "\\s-\\|\n\\|\\.")
    (save-excursion
      (with-syntax-table ein2:dotty-syntax-table
        (ein2:aif (thing-at-point 'word)
            it
          (unless (looking-at "(")
            (search-backward "(" (point-at-bol) t))
          (thing-at-point 'word))))))

(defun ein2:object-at-point-or-error ()
  (or (ein2:object-at-point) (error "No object found at the point")))


;;; URL utils

(defvar ein2:url-localhost "127.0.0.1")
(defvar ein2:url-localhost-template "http://127.0.0.1:%s")

(defun ein2:url (url-or-port &rest paths)
  (loop with url = (if (integerp url-or-port)
                       (format ein2:url-localhost-template url-or-port)
                     url-or-port)
        for p in paths
        do (setq url (concat (ein2:trim-right url "/")
                             "/"
                             (ein2:trim-left p "/")))
        finally return url))

(defun ein2:url-no-cache (url)
  "Imitate `cache=false' of `jQuery.ajax'.
See: http://api.jquery.com/jQuery.ajax/"
  (concat url (format-time-string "?_=%s")))


;;; HTML utils

(defun ein2:html-get-data-in-body-tag (key)
  "Very ad-hoc parser to get data in body tag."
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (search-forward "<body")
      (search-forward-regexp (format "%s=\\([^[:space:]\n]+\\)" key))
      (match-string 1))))


;;; JSON utils

(defmacro ein2:with-json-setting (&rest body)
  `(let ((json-object-type 'plist)
         (json-array-type 'list))
     ,@body))

(defun ein2:json-read ()
  "Read json from `url-retrieve'-ed buffer.

* `json-object-type' is `plist'. This is mainly for readability.
* `json-array-type' is `list'.  Notebook data is edited locally thus
  data type must be edit-friendly.  `vector' type is not."
  (goto-char (point-max))
  (backward-sexp)
  (ein2:with-json-setting
   (json-read)))

(defun ein2:json-read-from-string (string)
  (ein2:with-json-setting
   (json-read-from-string string)))

(defun ein2:json-any-to-bool (obj)
  (if (and obj (not (eq obj json-false))) t json-false))

(defun ein2:json-encode-char (char)
  "Fixed `json-encode-char'."
  (setq char (json-encode-char0 char 'ucs))
  (let ((control-char (car (rassoc char json-special-chars))))
    (cond
     ;; Special JSON character (\n, \r, etc.).
     (control-char
      (format "\\%c" control-char))
     ;; ASCIIish printable character.
     ((and (> char 31) (< char 127))    ; s/161/127/
      (format "%c" char))
     ;; Fallback: UCS code point in \uNNNN form.
     (t
      (format "\\u%04x" char)))))

(defadvice json-encode-char (around ein2:json-encode-char (char) activate)
  "Replace `json-encode-char' with `ein2:json-encode-char'."
  (setq ad-return-value (ein2:json-encode-char char)))


;;; EWOC

(defun ein2:ewoc-create (pretty-printer &optional header footer nosep)
  "Do nothing wrapper of `ewoc-create' to provide better error message."
  (condition-case nil
      (ewoc-create pretty-printer header footer nosep)
    ((debug wrong-number-of-arguments)
     (ein2:display-warning "Incompatible EOWC version.
  The version of ewoc.el you are using is too old for EIN.
  Please install the newer version.
  See also: https://github.com/tkf/emacs-ipython-notebook/issues/49")
     (error "Incompatible EOWC version."))))


;;; Text property

(defun ein2:propertize-read-only (string &rest properties)
  (apply #'propertize string 'read-only t 'front-sticky t properties))

(defun ein2:insert-read-only (string &rest properties)
  (insert (apply #'ein2:propertize-read-only string properties)))


;;; String manipulation

(defun ein2:trim (string &optional regexp)
  (ein2:trim-left (ein2:trim-right string regexp) regexp))

(defun ein2:trim-left (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein2:trim-regexp string (format "^\\(%s\\)+" regexp)))

(defun ein2:trim-right (string &optional regexp)
  (unless regexp (setq regexp "\\s-\\|\n"))
  (ein2:trim-regexp string (format "\\(%s\\)+$" regexp)))

(defun ein2:trim-regexp (string regexp)
  (if (string-match regexp string)
      (replace-match "" t t string)
    string))

(defun ein2:trim-indent (string)
  "Strip uniform amount of indentation from lines in STRING."
  (let* ((lines (split-string string "\n"))
         (indent
          (let ((lens
                 (loop for line in lines
                       for stripped = (ein2:trim-left line)
                       unless (equal stripped "")
                       collect (- (length line) (length stripped)))))
            (if lens (apply #'ein2:min lens) 0)))
         (trimmed
          (loop for line in lines
                if (> (length line) indent)
                collect (ein2:trim-right (substring line indent))
                else
                collect line)))
    (ein2:join-str "\n" trimmed)))

(defun ein2:join-str (sep strings)
  (mapconcat 'identity strings sep))

(defun ein2:join-path (paths)
  (mapconcat 'file-name-as-directory paths ""))

(defun ein2:string-fill-paragraph (string &optional justify)
  (with-temp-buffer
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (fill-paragraph justify)
    (buffer-string)))

(defmacro ein2:case-equal (str &rest clauses)
  "Similar to `case' but comparison is done by `equal'.
Adapted from twittering-mode.el's `case-string'."
  (declare (indent 1))
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))


;;; Text manipulation on buffer

(defun ein2:find-leftmot-column (beg end)
  "Return the leftmost column in region BEG to END."
  (save-excursion
    (let (mincol)
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (= (point) (point-at-eol))
          (setq mincol (if mincol
                           (min mincol (current-column))
                         (current-column))))
        (unless (= (forward-line 1) 0)
          (return-from ein2:find-leftmot-column mincol)))
      mincol)))


;;; Misc

(defun ein2:plist-iter (plist)
  "Return list of (key . value) in PLIST."
  ;; FIXME: this is not needed.  See: `ein2:plist-exclude'.
  (loop for p in plist
        for i from 0
        for key-p = (= (% i 2) 0)
        with key = nil
        if key-p do (setq key p)
        else collect `(,key . ,p)))

(defun ein2:plist-exclude (plist keys)
  "Exclude entries specified by KEYS in PLIST.

Example::

    (ein2:plist-exclude '(:a 1 :b 2 :c 3 :d 4) '(:b :c))"
  (loop for (k v) on plist by 'cddr
        unless (memq k keys)
        nconc (list k v)))

(defun ein2:hash-keys (table)
  (let (keys)
    (maphash (lambda (k v) (push k keys)) table)
    keys))

(defun ein2:hash-vals (table)
  (let (vals)
    (maphash (lambda (k v) (push v vals)) table)
    vals))

(defun ein2:filter (predicate sequence)
  (loop for item in sequence
        when (funcall predicate item)
        collect item))

(defun ein2:clip-list (list first last)
  "Return elements in region of the LIST specified by FIRST and LAST element.

Example::

    (ein2:clip-list '(1 2 3 4 5 6) 2 4)  ;=> (2 3 4)"
  (loop for elem in list
        with clipped
        with in-region-p = nil
        when (eq elem first)
        do (setq in-region-p t)
        when in-region-p
        do (push elem clipped)
        when (eq elem last)
        return (reverse clipped)))

(defun* ein2:list-insert-after (list pivot new &key (test #'eq))
  "Insert NEW after PIVOT in LIST destructively.
Note: do not rely on that `ein2:list-insert-after' change LIST in place.
Elements are compared using the function TEST (default: `eq')."
  (loop for rest on list
        when (funcall test (car rest) pivot)
        return (progn (push new (cdr rest)) list)
        finally do (error "PIVOT %S is not in LIST %S" pivot list)))

(defun* ein2:list-insert-before (list pivot new &key (test #'eq))
  "Insert NEW before PIVOT in LIST destructively.
Note: do not rely on that `ein2:list-insert-before' change LIST in place.
Elements are compared using the function TEST (default: `eq')."
  (if (and list (funcall test (car list) pivot))
      (cons new list)
    (loop for rest on list
          when (funcall test (cadr rest) pivot)
          return (progn (push new (cdr rest)) list)
          finally do (error "PIVOT %S is not in LIST %S" pivot list))))

(defun* ein2:list-move-left (list elem &key (test #'eq))
  "Move ELEM in LIST left.  TEST is used to compare elements"
  (macrolet ((== (a b) `(funcall test ,a ,b)))
    (cond
     ((== (car list) elem)
      (append (cdr list) (list (car list))))
     (t
      (loop for rest on list
            when (== (cadr rest) elem)
            return (let ((prev (car rest)))
                     (setf (car rest) elem)
                     (setf (cadr rest) prev)
                     list)
            finally do (error "ELEM %S is not in LIST %S" elem list))))))

(defun* ein2:list-move-right (list elem &key (test #'eq))
  "Move ELEM in LIST right.  TEST is used to compare elements"
  (loop with first = t
        for rest on list
        when (funcall test (car rest) elem)
        return (if (cdr rest)
                   (let ((next (cadr rest)))
                     (setf (car rest) next)
                     (setf (cadr rest) elem)
                     list)
                 (if first
                     list
                   (setcdr rest-1 nil)
                   (cons elem list)))
        finally do (error "ELEM %S is not in LIST %S" elem list)
        for rest-1 = rest
        do (setq first nil)))

(defun ein2:get-value (obj)
  "Get value from obj if it is a variable or function."
  (cond
   ((not (symbolp obj)) obj)
   ((boundp obj) (eval obj))
   ((fboundp obj) (funcall obj))))

(defun ein2:choose-setting (symbol value &optional single-p)
  "Choose setting in stored in SYMBOL based on VALUE.
The value of SYMBOL can be string, alist or function.
SINGLE-P is a function which takes one argument.  It must
return t when the value of SYMBOL can be used as a setting.
SINGLE-P is `stringp' by default."
  (let ((setting (eval symbol)))
    (cond
     ((funcall (or single-p 'stringp) setting) setting)
     ((functionp setting) (funcall setting value))
     ((listp setting)
      (ein2:get-value (or (assoc-default value setting)
                         (assoc-default 'default setting))))
     (t (error "Unsupported type of `%s': %s" symbol (type-of setting))))))

(defmacro ein2:setf-default (place val)
  "Set VAL to PLACE using `setf' if the value of PLACE is `nil'."
  `(unless ,place
     (setf ,place ,val)))

(defun ein2:funcall-packed (func-arg &rest args)
  "Call \"packed\" function.
FUNC-ARG is a `cons' of the form: (FUNC ARG).
FUNC is called as (apply FUNC ARG ARGS)."
  (apply (car func-arg) (cdr func-arg) args))

(defun ein2:eval-if-bound (symbol)
  (if (boundp symbol) (eval symbol)))

(defun ein2:remove-by-index (list indices)
  "Remove elements from LIST if its index is in INDICES.
NOTE: This function creates new list."
  (loop for l in list
        for i from 0
        when (not (memq i indices))
        collect l))

(defun ein2:min (x &rest xs)
  (loop for y in xs if (< y x) do (setq x y))
  x)

(defun ein2:do-nothing (&rest -ignore-)
  "A function which can take any number of variables and do nothing.")

(defun ein2:ask-choice-char (prompt choices)
  "Show PROMPT and read one of acceptable key specified as CHOICES."
  (let ((char-list (loop for i from 0 below (length choices)
                         collect (elt choices i)))
        (answer 'recenter))
    (while
        (let ((key
               (let ((cursor-in-echo-area t))
                 (read-key (propertize (if (eq answer 'recenter)
                                           prompt
                                         (concat "Please choose answer from"
                                                 (format " %s.  " choices)
                                                 prompt))
                                       'face 'minibuffer-prompt)))))
          (setq answer (lookup-key query-replace-map (vector key) t))
          (cond
           ((memq key char-list) (setq answer key) nil)
           ((eq answer 'recenter) (recenter) t)
           ((memq answer '(exit-prefix quit)) (signal 'quit nil) t)
           (t t)))
      (ding)
      (discard-input))
    answer))


(defun ein2:truncate-lines-on ()
  "Set `truncate-lines' on (set it to `t')."
  (setq truncate-lines t))


;;; Emacs utilities

(defun ein2:display-warning (message &optional level)
  "Simple wrapper around `display-warning'.
LEVEL must be one of :emergency, :error or :warning (default).
This must be used only for notifying user.
Use `ein2:log' for debugging and logging."
  ;; FIXME: Probably set BUFFER-NAME per notebook?
  ;; FIXME: Call `ein2:log' here (but do not display in minibuffer).
  (display-warning 'ein message level))

(defvar ein2:display-warning-once--db
  (make-hash-table :test 'equal))

(defun ein2:display-warning-once (message &optional level)
  "Call `ein2:display-warning' once for same MESSAGE and LEVEL."
  (let ((key (list message level)))
    (unless (gethash key ein2:display-warning-once--db)
      (ein2:display-warning message level)
      (puthash key t ein2:display-warning-once--db))))

(defun ein2:get-docstring (function)
  "Return docstring of FUNCTION."
  ;; Borrowed from `ac-symbol-documentation'.
  (with-temp-buffer
    ;; import help-xref-following
    (require 'help-mode)
    (erase-buffer)
    (let ((standard-output (current-buffer))
          (help-xref-following t)
          (major-mode 'help-mode)) ; avoid error in Emacs 24
      (describe-function-1 function))
    (buffer-string)))

(defun ein2:generate-menu (list-name-callback)
  (mapcar (lambda (name-callback)
            (destructuring-bind (name callback &rest args) name-callback
              `[,name ,callback :help ,(ein2:get-docstring callback) ,@args]))
          list-name-callback))


;;; Git utilities

(defun ein2:call-process (command &optional args)
  "Call COMMAND with ARGS and return its stdout as string or
`nil' if COMMAND fails.  It also checks if COMMAND executable
exists or not."
  (with-temp-buffer
    (erase-buffer)
    (and (executable-find command)
         (= (apply #'call-process command nil t nil args) 0)
         (buffer-string))))

(defun ein2:git-root-p (&optional dir)
  "Return `t' when DIR is root of git repository."
  (file-directory-p (expand-file-name ".git" (or dir default-directory))))

(defun ein2:git-dirty-p ()
  "Return `t' if the current directory is in git repository and it is dirty."
  (not (equal (ein2:call-process
               "git" '("--no-pager" "status" "--porcelain"))
              "")))

(defun ein2:git-revision ()
  "Return abbreviated git revision if the current directory is in
git repository."
  (ein2:call-process "git" '("--no-pager" "log" "-n1" "--format=format:%h")))

(defun ein2:git-revision-dirty ()
  "Return `ein2:git-revision' + \"-dirty\" suffix if the current
directory is in a dirty git repository."
  (ein2:aand (ein2:git-revision)
            (concat it (if (ein2:git-dirty-p) "-dirty" ""))))


;;; utils.js compatible

(defun ein2:utils-uuid ()
  "Return string with random (version 4) UUID.
Adapted from org-mode's `org-id-uuid'."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random t)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))


(provide 'ein-utils)

;;; ein-utils.el ends here
