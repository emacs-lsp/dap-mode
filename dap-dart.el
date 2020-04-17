;;; dap-dart.el --- Debug Adapter Protocol mode for Dart language -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Eric Dallo

;; Author: Eric Dallo  <ercdll1337@gmail.com>
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

;; URL: https://github.com/emacs-lsp/dap-mode
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))
;; Version: 0.2

;;; Commentary:

;; Debug adapter for Dart SDK.

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-dart-extension-version "3.9.1"
  "The extension version."
  :group 'dap-dart
  :type 'string)

(defcustom dap-dart-debugger-path
  (expand-file-name "github/Dart-Code.Dart-Code"
                    dap-utils-extension-path)
  "The path to dart vscode extension."
  :group 'dap-dart
  :type 'string)

(defcustom dap-dart-debugger-program
  `("node" ,(f-join dap-dart-debugger-path "extension/out/src/debug/dart_debug_entry.js"))
  "The path to the dart debugger."
  :group 'dap-dart
  :type '(repeat string))

(defcustom dap-dart-executable "dart"
  "The dart executable from dart SDK dir."
  :group 'dap-dart
  :type 'string)

(defun dap-dart--get-project-root ()
  "Return the project root path."
  (file-truename (locate-dominating-file default-directory "pubspec.yaml")))

(defun dap-dart--setup-extension (&rest _rest)
  "Setup dart debugger extension to run `dap-dart-debugger-program`."
  (message "DAP Dart :: Setting up...")
  (lsp-async-start-process
   (lambda ()
     (lsp-async-start-process
      (lambda () (message "DAP Dart :: Setup done!"))
      (lambda () (message "DAP Dart :: Error setting up dap-dart, check if `npm` is on $PATH"))
      (f-join dap-dart-debugger-path "extension/node_modules/typescript/bin/tsc")
      "--project" (f-join dap-dart-debugger-path "extension")))
   (lambda () (message "DAP Dart :: Error setting up dap-dart, check if `npm` is on $PATH"))
   "npm" "install" "--prefix" (f-join dap-dart-debugger-path "extension")
   "--no-package-lock" "--silent" "--no-save"))

(add-hook 'dap-utils-extension-downloaded-hook #'dap-dart--setup-extension nil t)

(dap-utils-github-extension-setup-function "dap-dart"
                                           "Dart-Code"
                                           "Dart-Code"
                                           dap-dart-extension-version
                                           dap-dart-debugger-path)

(defun dap-dart--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-dart-debugger-program)
      (dap--put-if-absent :type "dart")
      (dap--put-if-absent :cwd (dap-dart--get-project-root))
      (dap--put-if-absent :program (buffer-file-name))
      (dap--put-if-absent :name "Dart Debug")
      (dap--put-if-absent :dartPath dap-dart-executable)
      (dap--put-if-absent :debuggerType 0)
      (dap--put-if-absent :debugExternalLibraries nil)
      (dap--put-if-absent :debugSdkLibraries nil)))

(dap-register-debug-provider "dart" 'dap-dart--populate-start-file-args)
(dap-register-debug-template "Dart :: Run Configuration"
                             (list :type "dart"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Dart::Run"))

(provide 'dap-dart)
;;; dap-dart.el ends here
