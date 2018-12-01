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

(defcustom  dap-python-default-debug-port 32000
  "The debug port which will be used for ptvsd process.
If the port is taken, DAP will try the next port."
  :group 'dap-python
  :type 'number)

(defun dap-python--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let* ((host "localhost")
         (debug-port (dap--find-available-port host dap-python-default-debug-port)))
    (compile (format "python -m ptvsd --wait --host %s --port %s %s" host debug-port buffer-file-name))
    (dap--wait-for-port host debug-port)
    (plist-put conf :debugServer debug-port)
    (plist-put conf :host host)
    conf))

(eval-after-load "dap-mode"
  '(progn
     (dap-register-debug-provider "python" 'dap-python--populate-start-file-args)
     (dap-register-debug-template "Python :: Run Configuration"
                                 (list :type "python"
                                       :args ""
                                       :request "launch"
                                       :name "Python :: Run Configuration"))))

(provide 'dap-python)
;;; dap-python.el ends here
