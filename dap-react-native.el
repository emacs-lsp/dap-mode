;;; dap-react-native.el --- Debug Adapter Protocol mode for React Native      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dario Ceccoli

;; Author: Dario Ceccoli <dario.ceccoli@gmail.com>
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
;; Adapter for https://github.com/microsoft/vscode-react-native

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-react-native-debug-path (expand-file-name "vscode/msjsdiag.vscode-react-native"
                                                   dap-utils-extension-path)
  "The path to React Native Tools vscode extension."
  :group 'dap-react-native
  :type 'string)

(defcustom dap-react-native-debug-program `("node"
                                      ,(f-join dap-react-native-debug-path "extension/src/debugger/reactNativeDebugEntryPoint.js"))
  "The path to the React Native debugger."
  :group 'dap-react-native
  :type '(repeat string))

(dap-utils-vscode-setup-function "dap-react-native" "msjsdiag" "vscode-react-native"
                                 dap-react-native-debug-path)

(defun dap-react-native--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-react-native-debug-program)
      (dap--put-if-absent :type "react-native")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :name "React Native Attach")))

(dap-register-debug-provider "react-native" #'dap-react-native--populate-start-file-args)

(dap-register-debug-template "React Native Attach"
                             (list :type "react-native"
                                   :cwd nil
                                   :request "attach"
                                   :reAttach t
                                   :name "React Native Attach"))

(provide 'dap-react-native)
;;; dap-react-native.el ends here
