;;; dap-gdb-lldb.el --- Debug Adapter Protocol mode for LLDB/GDB      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski yyoncho@gmail.com
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
;; Version: 0.2

;;; Commentary:
;; Adapter for https://github.com/WebFreak001/code-debug

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-gdb-lldb-path (expand-file-name "vscode/webfreak.debug" dap-utils-extension-path)
  "The path to the place at which the webfreak.debug extension.
Link: https://marketplace.visualstudio.com/items?itemName=webfreak.debug ."
  :group 'dap-gdb-lldb
  :type 'string)

(defcustom dap-gdb-lldb-debug-program `("node"
                                        ,(f-join dap-gdb-lldb-path "extension/out/src/gdb.js"))
  "The path to the LLDB debugger."
  :group 'dap-gdb-lldb
  :type '(repeat string))

(dap-utils-vscode-setup-function "dap-gdb-lldb" "webfreak" "debug" dap-gdb-lldb-path)

(defun dap-gdb-lldb--populate-gdb (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-gdb-lldb-debug-program)
      (dap--put-if-absent :type "gdb")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :target (read-file-name "Select file to debug."))
      (dap--put-if-absent :name "GDB Debug")))

(dap-register-debug-provider "gdb" 'dap-gdb-lldb--populate-gdb)
(dap-register-debug-template "GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :target nil
                                   :cwd nil))



(defcustom dap-gdb-lldb-path-lldb `("node" ,(expand-file-name (f-join dap-gdb-lldb-path "extension/out/src/lldb.js")))
  "The path to the LLDB debugger."
  :group 'dap-gdb-lldb
  :type '(repeat string))

(defun dap-gdb-lldb--populate-lldb (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-gdb-lldb-path-lldb)
      (dap--put-if-absent :type "lldb")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :target (read-file-name "Select file to debug."))
      (dap--put-if-absent :name "LLDB Debug")))

(dap-register-debug-provider "lldb" 'dap-gdb-lldb--populate-lldb)
(dap-register-debug-template "LLDB Run Configuration"
                             (list :type "lldb"
                                   :request "launch"
                                   :name "LLDB::Run"
                                   :target nil
                                   :cwd nil))

(provide 'dap-gdb-lldb)
;;; dap-gdb-lldb.el ends here
