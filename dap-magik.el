;;; dap-magik.el --- Debug Adapter Protocol mode for Magik -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Steven Looman

;; Author: Steven Looman <steven.looman@gmail.com>
;; Keywords: dap, magik

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


;; LSP client for the Magik programming language
;; https://github.com/StevenLooman/magik-tools

;;; Code:

(require 'lsp-magik)
(require 'dap-mode)

(defgroup dap-magik nil
  "DAP support for Magik."
  :link '(url-link "https://github.com/StevenLooman/magik-tools")
  :group 'dap-mode
  :tag "Dap Magik"
  :package-version '(dap-mode . "0.9.1"))

(defcustom dap-magik-version "0.6.0"
  "Version of DAP server."
  :type `string
  :group `dap-magik
  :package-version '(dap-mode . "0.9.1"))

(defcustom dap-magik-download-url-da (format "https://github.com/StevenLooman/magik-tools/releases/download/%s/magik-debug-adapter-%s.jar" dap-magik-version dap-magik-version)
  "URL of DAP server to download."
  :type `string
  :group `dap-magik
  :package-version '(dap-mode . "0.9.1"))

(lsp-dependency
 'magik-da
 `(:download :url dap-magik-download-url-da
             :store-path ,(f-join lsp-server-install-dir "magik-lsp" (format "magik-debug-adapter-%s.jar" dap-magik-version))))

(defcustom dap-magik-da-path
  (f-join lsp-server-install-dir (format "magik-lsp/magik-debug-adapter-%s.jar" dap-magik-version))
  "Path of the debug adapter."
  :type 'string
  :group `dap-magik
  :package-version '(lsp-mode . "8.0.1"))

(defcustom dap-magik-attach-host "127.0.0.1"
  "Host to connect to when attaching to a session."
  :type 'string
  :group `dap-magik
  :package-version '(dap-mode . "0.9.1"))

(defcustom dap-magik-attach-port 21821
  "Host to port to when attaching to a session."
  :type 'integer
  :group `dap-magik
  :package-version '(dap-mode . "0.9.1"))



(defun dap-magik--populate-attach-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :da-server-path (list lsp-magik-java-path "-jar" dap-magik-da-path "--debug"))
      (dap--put-if-absent :type "magik")
      (dap--put-if-absent :request "attach")
      (dap--put-if-absent :connect (list :host dap-magik-attach-host
					 :port dap-magik-attach-port))))

(dap-register-debug-template "Magik: Attach"
                             (list :type "magik"
                                   :request "attach"
				   :name "Magik: Attach"))

(dap-register-debug-provider "magik" #'dap-magik--populate-attach-args)

(provide 'dap-magik)
;;; dap-magik.el ends here
