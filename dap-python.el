;;; dap-python.el --- Debug Adapter Protocol mode for Python      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; Adapter for ptvsd (https://github.com/Microsoft/ptvsd)

;;; Code:

(require 'cl-lib)
(require 'dap-mode)

(defcustom dap-python-default-debug-port 32000
  "The debug port which will be used for ptvsd process.
If the port is taken, DAP will try the next port."
  :group 'dap-python
  :type 'number)

(defcustom dap-python-executable "python"
  "The python executable to use."
  :group 'dap-python
  :risky t
  :type 'file)

(defcustom dap-python-terminal nil
  "The terminal to use when running the debug process.
For example you may set it to `xterm -e' which will pop xterm console when you are debugging."
  :group 'dap-python
  :risky t
  :type 'string)

(defun dap-python--pyenv-executable-find (command)
  "Find executable COMMAND, taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around
https://github.com/pyenv/pyenv-which-ext."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(cl-defstruct dap-python--point
  (line nil :type integer)
  (character nil :type integer))

(cl-defstruct dap-python--location
  (start nil :type dap-python--point)
  (end nil :type dap-python--point))

(cl-defstruct dap-python--symbol
  (name nil :type string)
  (type nil :type string)
  (location nil :type dap-python--location))

(lsp-defun dap-python--parse-lsp-symbol
  ((&DocumentSymbol
    :name :kind
    :selection-range (&Range :start (&Position :line start-line
                                               :character start-character)
                             :end (&Position :line end-line
                                             :character end-character))))
  (make-dap-python--symbol
   :name name
   :type (alist-get kind lsp--symbol-kind)
   :location (make-dap-python--location
              :start (make-dap-python--point :line start-line
                                             :character start-character)
              :end (make-dap-python--point :line end-line
                                           :character end-character))))

(defun dap-python--symbol-before-point (point lsp-symbol)
  (-> lsp-symbol
      dap-python--symbol-location
      dap-python--location-start
      dap-python--point-line
      (< (dap-python--point-line point))))

(defun dap-python--symbols-before-point (point lsp-symbols)
  (-filter (-partial 'dap-python--symbol-before-point point) lsp-symbols))

(defun dap-python--test-p (lsp-symbol)
  (let ((name (dap-python--symbol-name lsp-symbol)))
    (and (string= (dap-python--symbol-type lsp-symbol) "Function")
         (s-starts-with? "test_" name))))

(defun dap-python--test-class-p (test-symbol lsp-symbol)
  (when (string= (dap-python--symbol-type lsp-symbol) "Class")
    (let* ((class-location (dap-python--symbol-location lsp-symbol))
           (class-start-line (-> class-location dap-python--location-start dap-python--point-line))
           (class-end-line (-> class-location dap-python--location-end dap-python--point-line))
           (test-start-line (-> test-symbol dap-python--symbol-location dap-python--location-start dap-python--point-line)))
      (and (> test-start-line class-start-line)
           (< test-start-line class-end-line)))))

(defun dap-python--nearest-test (lsp-symbols)
  (cl-callf reverse lsp-symbols)
  (when-let ((test-symbol (-first 'dap-python--test-p lsp-symbols)))
    (let ((class-symbol
           (-first (-partial 'dap-python--test-class-p test-symbol)
                   lsp-symbols)))
      (if class-symbol
          (concat "::" (dap-python--symbol-name class-symbol)
                  "::" (dap-python--symbol-name test-symbol))
        (concat "::" (dap-python--symbol-name test-symbol))))))

(defun dap-python--cursor-position ()
  (make-dap-python--point :line (line-number-at-pos)
                          :character (current-column)))

(defun dap-python--test-at-point ()
  (->> (lsp--get-document-symbols)
       (mapcar #'dap-python--parse-lsp-symbol)
       (dap-python--symbols-before-point (dap-python--cursor-position))
       dap-python--nearest-test))

(defun dap-python--template (template-name)
  "Return the debug template whose name is TEMPLATE-NAME.
For the name, only the template's `car' is checked, not its
`:name' property."
  (--first (string= template-name it) dap-debug-template-configurations))

(defalias 'dap-python--debug-test-at-point #'dap-python-debug-test-at-point)
(defun dap-python-debug-test-at-point ()
  "Debug the pytest test under the cursor."
  (interactive)
  (dap-debug (dap-python--template "Python :: Run pytest (at point)")))

(defcustom dap-python-debugger 'ptvsd
  "Specify which debugger to use for `dap-python'.
Can be either `ptvsd' or `debugpy.' Note that this setting can be
overridden in individual `dap-python' launch configuration. The
values of this variable or the :debugger field may also be
strings, for the sake of launch.json feature parity."
  :type '(choice (const 'ptvsd) (const 'debugpy))
  :group 'dap-python)

(defun dap-python--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let* ((python-executable (dap-python--pyenv-executable-find dap-python-executable))
         (python-args (plist-get conf :args))
         (program (or (plist-get conf :target-module)
                      (plist-get conf :program)
                      (buffer-file-name)))
         (module (plist-get conf :module))
         (debugger (plist-get conf :debugger)))
    ;; These are `dap-python'-specific and always ignored.
    (cl-remf conf :debugger)
    (cl-remf conf :target-module)

    ;; Ignored by ptsvd and set explicitly for debugpy.
    (cl-remf conf :program)
    (pcase (or debugger dap-python-debugger)
      ((or 'ptvsd "ptvsd")
       (let ((host "localhost")
             (debug-port (dap--find-available-port)))
         ;; support :args ["foo" "bar"]; NOTE: :args can be nil; however, nil is
         ;; a list, so it will be mapconcat'ed, yielding the empty string.
         (when (sequencep python-args)
           (setq python-args (mapconcat #'shell-quote-argument python-args " ")))
         ;; ignored by ptsvd anyway
         (cl-remf conf :module)
         (cl-remf conf :args)
         (plist-put conf :program-to-start
                    (format "%s%s -m ptvsd --wait --host %s --port %s%s %s%s"
                            (or dap-python-terminal "")
                            (shell-quote-argument python-executable)
                            host
                            debug-port
                            (if module (concat " -m " (shell-quote-argument module)) "")
                            (if program (shell-quote-argument program) "")
                            (if (not (string-empty-p python-args)) (concat " " python-args) "")))
         (plist-put conf :debugServer debug-port)
         (plist-put conf :port debug-port)
         (plist-put conf :hostName host)
         (plist-put conf :host host)))
      ((or 'debugpy "debugpy")
       (cond ((stringp python-args)
              (cl-callf split-string-and-unquote python-args))
             ;; If both :module and :program are specified, we'll need to push
             ;; :program to PYTHON-ARGS instead, to behave like ptvsd. This is
             ;; needed for the debug-test-at-point functionality.
             ((and (vectorp python-args) module program)
              (cl-callf cl-coerce python-args 'list)))

       ;; If certain properties are nil, issues will arise, as debugpy expects
       ;; them to unspecified instead. Some templates in this file set such
       ;; properties (e.g. :module) to nil instead of leaving them undefined. To
       ;; support them, sanitize CONF before passing it on.
       (when program
         (if module
             (push program python-args)
           (plist-put conf :program program)))

       (cl-remf conf :args)
       (plist-put conf :args (or python-args []))

       (unless module
         (cl-remf conf :module))

       (unless (plist-get conf :cwd)
         (cl-remf conf :cwd))

       (plist-put conf :dap-server-path
                  (list python-executable "-m" "debugpy.adapter")))
      (_ (error "`dap-python': unknown :debugger type %S" debugger)))
    conf))

(defun dap-python--populate-test-at-point (conf)
  "Populate CONF with the required arguments."
  (if-let ((test (dap-python--test-at-point)))
      (plist-put conf :program (concat (buffer-file-name) test))
    (user-error "`dap-python': no test at point"))
  (plist-put conf :cwd (lsp-workspace-root))

  (dap-python--populate-start-file-args conf))

(dap-register-debug-provider "python" 'dap-python--populate-start-file-args)
(dap-register-debug-template "Python :: Run file (buffer)"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :module nil
                                   :program nil
                                   :request "launch"
                                   :name "Python :: Run file (buffer)"))

(dap-register-debug-template "Python :: Run pytest (buffer)"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (buffer)"))

(dap-register-debug-provider "python-test-at-point" 'dap-python--populate-test-at-point)
(dap-register-debug-template "Python :: Run pytest (at point)"
                             (list :type "python-test-at-point"
                                   :args ""
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (at point)"))

(cl-defmethod dap-handle-event ((_event-type (eql debugpyWaitingForServer)) _session _params))
(cl-defmethod dap-handle-event ((_event-type (eql debugpyAttach)) _session _params))

(provide 'dap-python)
;;; dap-python.el ends here
