;;; dap-lldb.el --- Debug Adapter Protocol mode for LLDB      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Daniel Martín

;; Author: Daniel Martín <mardani29@yahoo.es>
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
;; Adapter for https://github.com/llvm-mirror/lldb/tree/master/tools/lldb-vscode

;;; Code:

(require 'dap-mode)

(defcustom dap-lldb-debug-program `(,(expand-file-name "~/.vscode/extensions/llvm-org.lldb-vscode-0.1.0/bin/lldb-vscode"))
  "The path to the LLDB debugger."
  :group 'dap-lldb
  :type '(repeat string))

(defun dap-lldb--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (if (equal (plist-get conf :request) "launch")
      (dap--put-if-absent conf :program
                          (read-file-name
                           "Select file to debug."))
    (dap--put-if-absent conf :initCommands
                        (list (concat "process attach --pid "
                                      (dap-get-process-id-executed-in-eshell
                                       (read-file-name
                                        "Select file to debug."))))))

  (-> conf
      (dap--put-if-absent :dap-server-path dap-lldb-debug-program)
      (dap--put-if-absent :type "lldb")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :name "LLDB Debug")))

(eval-after-load "dap-mode"
  '(progn
     (dap-register-debug-provider "lldb" 'dap-lldb--populate-start-file-args)
     (dap-register-debug-template "LLDB Run"
                             (list :type "lldb"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "LLDB::Run"))
     (dap-register-debug-template "LLDB Run In Eshell"
                             (list :type "lldb"
                                   :cwd nil
                                   :request "attach"
                                   :program nil
                                   :name "LLDB::Run"))))

(provide 'dap-lldb)
;;; dap-lldb.el ends here
