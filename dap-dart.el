;;; dap-Dart.el --- Debug Adapter Protocol mode for Python      -*- lexical-binding: t; -*-

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
;; Adapter for https://github.com/Dartide/vscode-ruby

;;; Code:

(require 'dap-mode)

(defcustom dap-dart-debug-program `("node" ,(expand-file-name "/home/kyoncho/Downloads/dart-debug/extension/out/src/debug/dart_debug_entry.js"))
  "The path to the Dart debugger."
  :group 'dap-Dart
  :type '(repeat string))

(defun dap-dart--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-dart-debug-program)
      (dap--put-if-absent :type "dart")
      (dap--put-if-absent :cwd "/home/kyoncho/Downloads/dart-debug/extension/")
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :name "Dart Debug")))

(dap-register-debug-provider "dart" 'dap-dart--populate-start-file-args)
(dap-register-debug-template "Dart Run Configuration"
                             (list :type "dart"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Dart::Run"))

(provide 'dap-Dart)
;;; dap-Dart.el ends here
