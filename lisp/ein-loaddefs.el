;;; ein-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ein2:connect-to-default-notebook ein2:connect-buffer-to-notebook
;;;;;;  ein2:connect-to-notebook-buffer ein2:connect-to-notebook ein2:connect-to-notebook-command)
;;;;;;  "ein-connect" "ein-connect.el" (20629 35398 551423 930000))
;;; Generated autoloads from ein-connect.el

(autoload 'ein2:connect-to-notebook-command "ein-connect" "\
Connect to notebook.  When the prefix argument is given,
you can choose any notebook on your server including the ones
not yet opened.  Otherwise, already chose from already opened
notebooks.

\(fn &optional NOT-YET-OPENED)" t nil)

(autoload 'ein2:connect-to-notebook "ein-connect" "\
Connect any buffer to notebook and its kernel.

\(fn NBPATH &optional BUFFER NO-RECONNECTION)" t nil)

(autoload 'ein2:connect-to-notebook-buffer "ein-connect" "\
Connect any buffer to opened notebook and its kernel.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'ein2:connect-buffer-to-notebook "ein-connect" "\
Connect BUFFER to NOTEBOOK.

\(fn NOTEBOOK &optional BUFFER NO-RECONNECTION)" nil nil)

(autoload 'ein2:connect-to-default-notebook "ein-connect" "\
Connect to the default notebook specified by
`ein2:connect-default-notebook'.  Set this to `python-mode-hook'
to automatically connect any python-mode buffer to the
notebook.

\(fn)" nil nil)

;;;***

;;;### (autoloads (ein2:console-open) "ein-console" "ein-console.el"
;;;;;;  (20546 25932 437322 552000))
;;; Generated autoloads from ein-console.el

(autoload 'ein2:console-open "ein-console" "\
Open IPython console.
To use this function, `ein2:console-security-dir' and
`ein2:console-args' must be set properly.
This function requires `Fabian Gallina's python.el`_ for now;
It should be possible to support python-mode.el.  Patches are welcome!

.. _`Fabian Gallina's python.el`: https://github.com/fgallina/python.el

\(fn)" t nil)

;;;***

;;;### (autoloads (ein2:dev-bug-report-template ein2:dev-stop-debug
;;;;;;  ein2:dev-start-debug ein2:dev-insert-mode-map) "ein-dev" "ein-dev.el"
;;;;;;  (20593 3171 499680 955000))
;;; Generated autoloads from ein-dev.el

(autoload 'ein2:dev-insert-mode-map "ein-dev" "\
Insert mode-map into rst document.  For README.rst.

\(fn MAP-STRING)" nil nil)

(autoload 'ein2:dev-start-debug "ein-dev" "\
Enable EIN debugging support.
When the prefix argument is given, debugging support for websocket
callback (`websocket-callback-debug-on-error') is enabled.

\(fn &optional WS-CALLBACK)" t nil)

(autoload 'ein2:dev-stop-debug "ein-dev" "\
Disable debugging support enabled by `ein2:dev-start-debug'.

\(fn)" t nil)

(autoload 'ein2:dev-bug-report-template "ein-dev" "\
Open a buffer with bug report template.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ein-notebook-buffers anything-ein-notebook-buffers
;;;;;;  helm-ein-kernel-history anything-ein-kernel-history) "ein-helm"
;;;;;;  "ein-helm.el" (20642 12065 658663 404000))
;;; Generated autoloads from ein-helm.el

(autoload 'anything-ein-kernel-history "ein-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'helm-ein-kernel-history "ein-helm" "\
Search kernel execution history then insert the selected one.

\(fn)" t nil)

(autoload 'anything-ein-notebook-buffers "ein-helm" "\
Choose opened notebook using anything.el interface.

\(fn)" t nil)

(autoload 'helm-ein-notebook-buffers "ein-helm" "\
Choose opened notebook using helm interface.

\(fn)" t nil)

;;;***

;;;### (autoloads (ein2:iexec-mode) "ein-iexec" "ein-iexec.el" (20546
;;;;;;  25932 437322 552000))
;;; Generated autoloads from ein-iexec.el

(autoload 'ein2:iexec-mode "ein-iexec" "\
Instant cell execution minor mode.
Code cell at point will be automatically executed after any
change in its input area.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ein2:ipynb-mode) "ein-ipynb-mode" "ein-ipynb-mode.el"
;;;;;;  (20600 17207 518562 563000))
;;; Generated autoloads from ein-ipynb-mode.el

(autoload 'ein2:ipynb-mode "ein-ipynb-mode" "\
A simple mode for ipynb file.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '(".*\\.ipynb\\'" . ein2:ipynb-mode))

;;;***

;;;### (autoloads (ein2:jedi-setup ein2:jedi-dot-complete ein2:jedi-complete)
;;;;;;  "ein-jedi" "ein-jedi.el" (20650 42078 657490 10000))
;;; Generated autoloads from ein-jedi.el

(autoload 'ein2:jedi-complete "ein-jedi" "\
Run completion using candidates calculated by EIN and Jedi.

\(fn)" t nil)

(autoload 'ein2:jedi-dot-complete "ein-jedi" "\
Insert \".\" and run `ein2:jedi-complete'.

\(fn)" t nil)

(autoload 'ein2:jedi-setup "ein-jedi" "\
Setup auto-completion using EIN and Jedi.el_ together.

Jedi.el_ is a Python auto-completion library for Emacs.
To use EIN and Jedi together, add the following in your Emacs setup.::

  (add-hook 'ein2:connect-mode-hook 'ein2:jedi-setup)

.. _Jedi.el: https://github.com/tkf/emacs-jedi

\(fn)" nil nil)

;;;***

;;;### (autoloads (ein2:junk-rename ein2:junk-new) "ein-junk" "ein-junk.el"
;;;;;;  (20571 14580 170606 716000))
;;; Generated autoloads from ein-junk.el

(autoload 'ein2:junk-new "ein-junk" "\
Open a notebook to try random thing.
Notebook name is determined based on
`ein2:junk-notebook-name-template'.

When prefix argument is given, it asks URL or port to use.

\(fn NAME URL-OR-PORT)" t nil)

(autoload 'ein2:junk-rename "ein-junk" "\
Rename the current notebook based on `ein2:junk-notebook-name-template'
and save it immediately.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "ein-kernel" "ein-kernel.el" (20596 28965 566444
;;;;;;  588000))
;;; Generated autoloads from ein-kernel.el

(defalias 'ein2:kernel-url-or-port 'ein2:$kernel-url-or-port)

(defalias 'ein2:kernel-id 'ein2:$kernel-kernel-id)

;;;***

;;;### (autoloads (ein2:notebook-multilang-mode) "ein-multilang" "ein-multilang.el"
;;;;;;  (20591 25156 462553 731000))
;;; Generated autoloads from ein-multilang.el

(autoload 'ein2:notebook-multilang-mode "ein-multilang" "\
Notebook mode with multiple language fontification.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ein-notebook" "ein-notebook.el" (20604 29927
;;;;;;  731865 215000))
;;; Generated autoloads from ein-notebook.el

(defalias 'ein2:notebook-name 'ein2:$notebook-notebook-name)

;;;***

;;;### (autoloads (ein2:notebooklist-load ein2:notebooklist-open-notebook-global
;;;;;;  ein2:notebooklist-list-notebooks ein2:notebooklist-new-notebook-with-name
;;;;;;  ein2:notebooklist-new-notebook ein2:notebooklist-reload ein2:notebooklist-open)
;;;;;;  "ein-notebooklist" "ein-notebooklist.el" (20604 21315 671448
;;;;;;  417000))
;;; Generated autoloads from ein-notebooklist.el

(autoload 'ein2:notebooklist-open "ein-notebooklist" "\
Open notebook list buffer.

\(fn &optional URL-OR-PORT NO-POPUP)" t nil)

(autoload 'ein2:notebooklist-reload "ein-notebooklist" "\
Reload current Notebook list.

\(fn)" t nil)

(autoload 'ein2:notebooklist-new-notebook "ein-notebooklist" "\
Ask server to create a new notebook and open it in a new buffer.

\(fn &optional URL-OR-PORT CALLBACK CBARGS)" t nil)

(autoload 'ein2:notebooklist-new-notebook-with-name "ein-notebooklist" "\
Open new notebook and rename the notebook.

\(fn NAME &optional URL-OR-PORT)" t nil)

(autoload 'ein2:notebooklist-list-notebooks "ein-notebooklist" "\
Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/NOTEBOOK-NAME\".

\(fn)" nil nil)

(autoload 'ein2:notebooklist-open-notebook-global "ein-notebooklist" "\
Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein2:notebook-open'.

\(fn NBPATH &optional CALLBACK CBARGS)" t nil)

(autoload 'ein2:notebooklist-load "ein-notebooklist" "\
Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein2:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein2:notebooklist-load)

You should setup `ein2:url-or-port' or `ein2:default-url-or-port'
in order to make this code work.

See also:
`ein2:connect-to-default-notebook', `ein2:connect-default-notebook'.

\(fn &optional URL-OR-PORT)" nil nil)

;;;***

;;;### (autoloads (ein2:org-store-link ein2:org-open) "ein-org" "ein-org.el"
;;;;;;  (20604 29927 731865 215000))
;;; Generated autoloads from ein-org.el

(autoload 'ein2:org-open "ein-org" "\
Open IPython notebook specified by LINK-PATH.
This function is to be used for FOLLOW function of
`org-add-link-type'.

\(fn LINK-PATH)" nil nil)

(autoload 'ein2:org-store-link "ein-org" "\
Call `org-store-link-props' when in notebook buffer.
This function is to be used for `org-store-link-functions'.

Examples::

  ipynb:(:url-or-port 8888 :name \"My_Notebook\")
  ipynb:(:url-or-port \"http://notebook-server\" :name \"My_Notebook\")

Note that spaces will be escaped in org files.

As how IPython development team supports multiple directory in
IPython notebook server is unclear, it is not easy to decide the
format for notebook links.  Current approach is to use
S-expression based (rather verbose) serialization, so that
extending link spec without loosing backward compatibility is
easier.  For the examples of link format in general, see Info
node `(org) External links' and Info node `(org) Search options'

\(fn)" nil nil)

(eval-after-load "org" '(progn (org-add-link-type "ipynb" 'ein2:org-open) (add-hook 'org-store-link-functions 'ein2:org-store-link)))

;;;***

;;;### (autoloads (ein2:pseudo-console-mode) "ein-pseudo-console"
;;;;;;  "ein-pseudo-console.el" (20546 25932 441322 552000))
;;; Generated autoloads from ein-pseudo-console.el

(autoload 'ein2:pseudo-console-mode "ein-pseudo-console" "\
Pseudo console mode.  Hit RET to execute code.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ein2:shared-output-eval-string ein2:shared-output-show-code-cell-at-point
;;;;;;  ein2:shared-output-pop-to-buffer) "ein-shared-output" "ein-shared-output.el"
;;;;;;  (20629 35398 555423 930000))
;;; Generated autoloads from ein-shared-output.el

(autoload 'ein2:shared-output-pop-to-buffer "ein-shared-output" "\
Open shared output buffer.

\(fn)" t nil)

(autoload 'ein2:shared-output-show-code-cell-at-point "ein-shared-output" "\
Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein2:cell-max-num-outputs'.

\(fn)" t nil)

(autoload 'ein2:shared-output-eval-string "ein-shared-output" "\
Evaluate a piece of code.  Prompt will appear asking the code to run.
This is handy when you want to execute something quickly without
making a cell.  If the code outputs something, it will go to the
shared output buffer.  You can open the buffer by the command
`ein2:shared-output-pop-to-buffer'.

.. ARGS is passed to `ein2:kernel-execute'.  Unlike `ein2:kernel-execute',
   `:silent' is `nil' by default.

\(fn CODE &optional POPUP VERBOSE KERNEL &rest ARGS)" t nil)

;;;***

;;;### (autoloads (ein2:tb-show) "ein-traceback" "ein-traceback.el"
;;;;;;  (20546 25932 441322 552000))
;;; Generated autoloads from ein-traceback.el

(autoload 'ein2:tb-show "ein-traceback" "\
Show full traceback in traceback viewer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("debug-ein.el" "ein-ac.el" "ein-cell.el"
;;;;;;  "ein-completer.el" "ein-core.el" "ein-events.el" "ein-kernelinfo.el"
;;;;;;  "ein-kill-ring.el" "ein-log.el" "ein-multilang-fontify.el"
;;;;;;  "ein-mumamo.el" "ein-node.el" "ein-notification.el" "ein-output-area.el"
;;;;;;  "ein-pager.el" "ein-pkg.el" "ein-python.el" "ein-pytools.el"
;;;;;;  "ein-query.el" "ein-scratchsheet.el" "ein-smartrep.el" "ein-subpackages.el"
;;;;;;  "ein-utils.el" "ein-websocket.el" "ein-worksheet.el" "ein.el"
;;;;;;  "zeroein.el") (20650 45637 307359 381000))

;;;***

(provide 'ein-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ein-loaddefs.el ends here
