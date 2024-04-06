;;; dap-kotlin.el --- Debug Adapter Protocol mode for Kotlin  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024  emacs-lsp maintainers

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

;; Commentary:
;; Adapter for https://github.com/fwcd/kotlin-debug-adapter

;;; Code:

(require 'lsp-kotlin)
(require 'dap-mode)

(defcustom dap-kotlin-enable-log nil
  "Turn on logging of debug adapter logs to file."
  :type 'boolean
  :group 'dap-kotlin)

(defcustom dap-kotlin-log-path nil
  "Path to the debug adapter log file."
  :type 'string
  :group 'dap-kotlin)

(defun dap-kotlin-populate-launch-args (conf)
  ;; we require mainClass to be filled in in the launch configuration.
  ;; dap-kotlin does currently not support a fallback if not defined
  (-> conf
	  (dap--put-if-absent :request "launch")
	  (dap--put-if-absent :name "Kotlin Launch")
	  (dap--put-if-absent :projectRoot (lsp-workspace-root))
	  (dap--put-if-absent :enableJsonLogging (lsp-json-bool dap-kotlin-enable-log))
	  (dap--put-if-absent :jsonLogFile dap-kotlin-log-path)))

(defun dap-kotlin-populate-attach-args (conf)
  (-> conf
	  (dap--put-if-absent :request "attach")
	  (dap--put-if-absent :name "Kotlin Attach")
	  (dap--put-if-absent :projectRoot (lsp-workspace-root))
	  (dap--put-if-absent :hostName "localhost")
	  (dap--put-if-absent :port 5005)
	  (dap--put-if-absent :timeout 2000)
	  (dap--put-if-absent :enableJsonLogging (lsp-json-bool dap-kotlin-enable-log))
	  (dap--put-if-absent :jsonLogFile dap-kotlin-log-path)))

(defun dap-kotlin-populate-default-args (conf)
  (setq conf (pcase (plist-get conf :request)
			   ("launch" (dap-kotlin-populate-launch-args conf))
			   ("attach" (dap-kotlin-populate-attach-args conf))
			   (_ (error "Unsupported dap-request"))))

  (-> conf
	  (dap--put-if-absent :type "kotlin")
	  (plist-put :dap-server-path (list lsp-kotlin-debug-adapter-path))))

(dap-register-debug-provider "kotlin" #'dap-kotlin-populate-default-args)

(dap-register-debug-template "Kotlin Attach"
							 (list :type "kotlin"
								   :request "attach"
								   :noDebug nil))

(provide 'dap-kotlin)
;;; dap-kotlin.el ends here
