;;; dap-cortex.el --- Debug Adapter Protocol mode for cortex      -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Mario Schlegel

;; Author: Mario Schlegel m.schlegel@posteo.de
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

;; URL: https://github.com/mrsch/dap-mode
;; Version: 0.1

;;; Commentary:
;; Adapter for https://github.com/Marus/cortex-debug

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-cortex-path (expand-file-name "marus25.cortex-debug" dap-utils-extension-path)
  "The path to the place at which the webfreak.debug extension."
  :group 'dap-cortex
  :type 'string)

(defcustom dap-cortex-debug-program `("node" ,(f-join dap-cortex-path "dist/debugadapter.js"))
  "The path to the cortex debugger."
  :group 'dap-cortex
  :type '(repeat string))

(defun dap-cortex--populate-jlink (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-cortex-debug-program)
      (dap--put-if-absent :type "cortex-debug")
      (dap--put-if-absent :name "Debug JLink")
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :gdbPath "arm-none-eabi-gdb")
      (dap--put-if-absent :rttConfig '(:enabled :json-true :address "auto" :decoders [(:label "" :port 0 :type "console")]))
      (dap--put-if-absent :gdbServerConsolePort 55878)
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :extensionPath (concat dap-cortex-path "/"))
      (dap--put-if-absent :preLaunchCommands [])
      (dap--put-if-absent :postLaunchCommands [])
      (dap--put-if-absent :interface "swd")
      (dap--put-if-absent :swoConfig '(:enabled :json-false))
      ))


(dap-register-debug-provider "cortex-debug" 'dap-cortex--populate-jlink)

(provide 'dap-cortex)
;;; dap-cortex.el ends here
