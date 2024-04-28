;;; dap-gdb.el --- Debug Adapter Protocol mode for LLDB/GDB      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski
;; Copyright (C) 2024  Danny Milosavljevic

;; Author: Danny Milosavljevic <dannym@scratchpost.org>
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

;;; Commentary:
;; Adapter for GDB version >= 14

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-gdb-debug-program `("gdb" "-i" "dap")
  "The path to the GDB debugger."
  :group 'dap-gdb
  :type '(repeat string))

(defun dap-gdb--populate-gdb (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-gdb-debug-program)
      (dap--put-if-absent :type "gdb")
      (dap--put-if-absent :cwd default-directory)
      ;(dap--put-if-absent :target (expand-file-name (read-file-name "Select file to debug.")))
      (dap--put-if-absent :name "GDB Debug")

      (dap--put-if-absent :valuesFormatting "prettyPrinters")))

(dap-register-debug-provider "gdb" 'dap-gdb--populate-gdb)
(dap-register-debug-template "GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :target nil
                                   :cwd nil))

(defun dap-gdb--populate-gdbserver (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-gdb-debug-program)
      (dap--put-if-absent :type "gdbserver")
      (dap--put-if-absent :name "GDB Server")
      (dap--put-if-absent :request "attach")
      (dap--put-if-absent :gdbpath "gdb")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :target (read-string "target?(host:port) "))
      (dap--put-if-absent :remote :json-true)

      (dap--put-if-absent :valuesFormatting "prettyPrinters")))


(dap-register-debug-provider "gdbserver" 'dap-gdb--populate-gdbserver)
(dap-register-debug-template "GDBServer Connect Configuration"
                             (list :type "gdbserver"
                                   :name "GDBServer::Connect"
                                   :target nil ;;host:port
                                   :cwd nil
                                   :executable nil ;;usually not needed as symbols can be downloaded from gdbserver
                                   :autorun nil
                                   :debugger_args nil
                                   :env nil
                                   :showDevDebugOutput :json-false
                                   :printCalls :json-false))


(provide 'dap-gdb)
;;; dap-gdb.el ends here
