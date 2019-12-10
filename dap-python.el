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
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
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

(defun dap-python--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let* ((host "localhost")
         (debug-port (dap--find-available-port host dap-python-default-debug-port))
         (python-executable (dap-python--pyenv-executable-find dap-python-executable))
         (python-args (or (plist-get conf :args) ""))
         (program (or (plist-get conf :target-module)
                      (plist-get conf :program)
                      (buffer-file-name)))
         (module (plist-get conf :module)))

    (dap--put-if-absent conf :program-to-start
                        (format "%s%s -m ptvsd --wait --host %s --port %s %s %s %s"
                                (or dap-python-terminal "")
                                (shell-quote-argument python-executable)
                                host
                                debug-port
                                (if module (concat "-m " (shell-quote-argument module)) "")
                                (shell-quote-argument program)
                                python-args))
    (plist-put conf :program program)
    (plist-put conf :debugServer debug-port)
    (plist-put conf :port debug-port)
    (plist-put conf :wait-for-port t)
    (plist-put conf :hostName host)
    (plist-put conf :host host)
    conf))

(dap-register-debug-provider "python" 'dap-python--populate-start-file-args)
(dap-register-debug-template "Python :: Run Configuration"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :module nil
                                   :program nil
                                   :request "launch"
                                   :name "Python :: Run Configuration"))

(dap-register-debug-template "Python :: pytest"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run Configuration"))

(provide 'dap-python)
;;; dap-python.el ends here
