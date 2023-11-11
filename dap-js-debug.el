;;; dap-js-debug.el --- Debug Adapter Protocol mode for vscode-js-debug -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeffrey Phillips

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

;; Author: Jeffrey Phillips <jeffphil@gmail.com>
;; Keywords: languages, debug, javascript
;; Version: 0.1
;; URL: https://github.com/emacs-lsp/dap-mode

;;; Commentary:
;; Adapter for microsoft/vscode-js-debug,
;;    see: https://github.com/microsoft/vscode-js-debug
;; Package-Requires: ((dap-mode "0.8"))
;; Also requires vscode-js-debug v1.77.2+ which can be installed here

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'json)

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-js-debug-path (expand-file-name "vscode/ms-vscode.js-debug"
                                               dap-utils-extension-path)
  "The path to ms-vscode js-debugger extension."
  :group 'dap-js-debug
  :type 'string)

(defcustom dap-js-debug-program `("node"
                                  ,(f-join dap-js-debug-path
                                           "extension/dist/src/dapDebugServer.js"))
  "The path and program for the ms-vscode js-debugger."
  :group 'dap-js-debug
  :type 'string)

(defcustom dap-js-debug-output-telemetry t
  "Output telemetry data from js-debug server if non-nil."
  :group 'dap-js-debug
  :type 'boolean)

(defcustom dap-js-debug-extension-version "latest"
  "The version of the github release found at
https://github.com/microsoft/vscode-js-debug/releases"
  :group 'dap-js-debug
  :type 'string)

(dap-utils-github-extension-setup-function "dap-js-debug" "microsoft" "vscode-js-debug"
                                           dap-js-debug-extension-version
                                           dap-js-debug-path
                                           #'dap-js-debug-extension-build)

(defun dap-js-debug-extension-build ()
  "Callback from setup function in order to install extension node deps and compile."
  (message "Building ms-vscode.js-debug in %s directory." dap-js-debug-path)
  (let ((buf (get-buffer-create "*dap-js-debug extension build*"))
        (default-directory (concat dap-js-debug-path "/extension")))
    (async-shell-command
     "npm install --sav-dev --force; npm run compile -- dapDebugServer" buf buf)))

(cl-defun dap-js-debug-extension-update (&optional (ask-upgrade t))
  "Check for update, and if `ask-upgrade' arg is non-nil will prompt user to upgrade."
  (interactive)
  (let* ((url (format dap-utils-github-extension-releases-info-url "microsoft"
                      "vscode-js-debug" "latest"))
         (cur-version
          (let ((file (f-join dap-js-debug-path "extension/package.json")))
            (when (file-exists-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (cdr (assoc 'version (json-read)))))))
         (latest-version
          (let ((inhibit-message dap-inhibit-io))
            (with-current-buffer
                (if-let ((buf (url-retrieve-synchronously url t t 10)))
                    buf ;returned
                  (progn
                    ;; Probably timeout
                    (message "Problem getting latest version from: %s" url)
                    (generate-new-buffer "*dap-js-debug-temp*")))
              (if (/= (point-max) 1)
                  (progn
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (substring (cdr (assoc 'tag_name (json-read))) 1))
                (progn
                  (kill-buffer)
                  cur-version))))))
    (if (string= cur-version latest-version)
        (when ask-upgrade
          (message "ms-vscode.js-debug extension is up to date at version: %s"
                   latest-version))
      (let ((msg (format "Newer version (%s) of vscode/ms-vscode.js-debug exists than \
currently installed version (%s)." latest-version cur-version)))
        (if ask-upgrade
            (when (y-or-n-p (concat msg " Do you want to upgrade now?"))
              (dap-js-debug-setup t))
          (message "%s Upgrade with `M-x dap-js-debug-extension-update'" msg))))))

;; Check extension version when loading, and give a message about upgrading.
(dap-js-debug-extension-update nil)

(defun dap-js-debug--populate-start-file-args (conf)
  "Load up the start config CONF for the debug adapter from launch.json, and default
   required attributes if missing. See full options:
   `https://github.com/microsoft/vscode-js-debug/blob/main/OPTIONS.md'"
  (dap--put-if-absent conf :type "pwa-chrome")
  (dap--put-if-absent conf :cwd (lsp-workspace-root))
  (dap--put-if-absent conf :request "launch")
  (dap--put-if-absent conf :console "internalConsole")
  (dap--put-if-absent conf :name (concat (plist-get conf :type) "-js-debug"))
  (let ((host "localhost")
        (debug-port (dap--find-available-port)))
    (dap--put-if-absent conf :host "localhost")
    (dap--put-if-absent conf :debugServer debug-port)
    (dap--put-if-absent conf :debugPort debug-port)
    (dap--put-if-absent conf :program-to-start
                        (if (not (file-exists-p dap-js-debug-path))
                            (error "DAP program path: %s does not exist! \
Install it with M-x dap-js-debug-setup." dap-js-debug-path)
                          (format "%s %s %s"
                                  (mapconcat 'identity dap-js-debug-program " ")
                                  (plist-get conf :debugPort)
                                  (plist-get conf :host)))))
  (if (plist-member conf :url)
      (progn
        ;;(plist-put conf :mode "url")
        (dap--put-if-absent conf :url (read-string
                                       "Browse url: "
                                       "http://localhost:3000" t))
        (dap--put-if-absent conf :webRoot (lsp-workspace-root))))
  (if (plist-member conf :file)
      (if (plist-get conf :url)
          (error "Both \"file\" and \"url\" properties are set in launch.json. \
Choose one.")
        (progn
          (plist-put conf :mode "file")
          (dap--put-if-absent conf :file
                              (read-file-name "Select the file to open in the browser:"
                                              nil (buffer-file-name) t)))))
  (if (plist-member conf :program)
      (dap--put-if-absent conf :program (read-file-name
                                         "Select the Node.js program to run: "
                                         nil (buffer-file-name) t)))
  (when (string= "node-terminal" (plist-get conf :type))
    (error "In launch.json \"node-terminal\" debug type is currently not supported."))
  (when (string= "integratedTerminal" (plist-get conf :console))
    (error "In launch.json \"console\":\"integratedTerminal\" not supported at this \
time, use \"console\":\"internalConsole\" instead"))
  (dap--put-if-absent conf
                      :output-filter-function #'dap-js-debug--output-filter-function)
  (unless dap-inhibit-io
    (message "dap-js-debug--populate-start-file-args: %s" conf))
  conf)

;; Note, vscode-js-debug prefers now not using `pwa-' prefix, but still takes.
;; Need to deprecate and replace: dap-chrome.el, dap-edge.el, dap-node.el before can
;; remove `pwa-' here.
(dap-register-debug-provider "pwa-node" #'dap-js-debug--populate-start-file-args)
(dap-register-debug-provider "pwa-chrome" #'dap-js-debug--populate-start-file-args)
(dap-register-debug-provider "pwa-msedge" #'dap-js-debug--populate-start-file-args)
(dap-register-debug-provider "node-terminal" #'dap-js-debug--populate-start-file-args)
;;If writing a vscode extension, probably wouldn't come to emacs.  Don't register this.
;;(dap-register-debug-provider "pwa-extensionHost" #'dap-js-debug--populate-start-file-args)

(dap-register-debug-template "Node.js Launch Program"
                             (list :type "pwa-node"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Node.js Launch Program"))

(dap-register-debug-template "Chrome Launch File"
                             (list :type "pwa-chrome"
                                   :cwd nil
                                   :request "launch"
                                   :file nil
                                   :name "Chrome Launch File"))

(dap-register-debug-template "Chrome Launch URL"
                             (list :type "pwa-chrome"
                                   :cwd nil
                                   :request "launch"
                                   :webRoot nil
                                   :url nil
                                   :name "Chrome Launch URL"))

(add-hook 'dap-session-created-hook #'dap-js-debug--session-created)
(defun dap-js-debug--session-created (debug-session)
  "Set up so that processes won't ask about closing."
  (when-let (proc (dap--debug-session-program-proc debug-session))
    (set-process-query-on-exit-flag proc nil)))

(defun dap-js-debug--output-filter-function (debug-session event)
  "Output event data, including for vscode-js-debug, some useful telemetry data.
   Future can do something more with the telemetry data than just printing."
  (-let [(&hash "seq" "event" event-type "body") event]
    (if (hash-table-p body)
        (progn
          (if (and (bound-and-true-p dap-js-debug-output-telemetry)
                   (string= (gethash "category" body) "telemetry"))
              (dap--print-to-output-buffer
               debug-session (concat (dap--json-encode body) "\n"))
            (dap--print-to-output-buffer
             debug-session (concat (dap--output-buffer-format body) "\n")))))))

(add-hook 'dap-terminated-hook #'dap-js-debug--term-parent)
(defun dap-js-debug--term-parent (debug-session)
  "Kill off parent process when child is disconnected."
  (if (eq debug-session (if (boundp 'parent-debug-session) parent-debug-session nil))
      (progn
        (when-let (proc (dap--debug-session-program-proc debug-session))
          (when (process-live-p proc)
            (makunbound 'parent-debug-session)
            (set-process-query-on-exit-flag proc nil)
            (with-current-buffer (process-buffer proc)
              ;; Switching mode, prevents triggering to open err file after killing proc
              (shell-script-mode)
              (kill-buffer))
            (dap-delete-session debug-session)))))
  (kill-buffer (dap--debug-session-output-buffer debug-session)))

(add-hook 'dap-executed-hook #'dap-js-debug--reverse-request-handler)
(defun dap-js-debug--reverse-request-handler (debug-session command)
  "Callback hook to get messages from dap-mode reverse requests."
  ;;This is set with `add-hook' above.
  (unless dap-inhibit-io
    (message "dap-js-debug--reverse-request-handler -> command: %s" command))
  (pcase command
    ((guard (string= command "startDebugging"))
     ;; Assume current session now parent requesting start debugging in child session
     (setq parent-debug-session debug-session)
     (-let [(&hash "seq" "command" "arguments"
                   (&hash "request" "configuration"
                          (&hash? "type" "__pendingTargetId")))
            (dap--debug-session-metadata debug-session)]
       (-let (((&plist  :mode :url :file :webroot :program :outputCapture
                        :skipFiles :timeout :host :name :debugPort)
               (dap--debug-session-launch-args debug-session))
              (conf `(:request ,request)))
         ;; DAP Spec says not to include client variables to start child, including type
         ;;(plist-put conf :type type)
         (plist-put conf :name (concat type "-" command))
         (plist-put conf :__pendingTargetId __pendingTargetId)
         (plist-put conf :outputCapture outputCapture)
         (plist-put conf :skipFiles skipFiles)
         (plist-put conf :timeout timeout)
         (plist-put conf :host host)
         (plist-put conf :debugServer debugPort)
         (plist-put conf :debugPort debugPort)
         (if (or (string= "pwa-node" type) (string= "node" type))
             (plist-put conf :program program)
           (progn
             (if (string= mode "file")
                 (plist-put conf :file file)
               (progn
                 (plist-put conf :url url)
                 (plist-put conf :webroot webroot)))))
         (unless dap-inhibit-io
           (message "dap-js-debug startDebugging conf: %s" conf))
         (dap-start-debugging-noexpand conf)
         ;; Remove child session if stored in list of recent/last configurations to
         ;; allow `dap-debug-last' to work by getting parent not child.
         (when-let ((last-conf (cdr (cl-first dap--debug-configuration)))
                    (_ptid-equal (string= __pendingTargetId
                                          (plist-get last-conf :__pendingTargetId))))
           (pop dap--debug-configuration))
         ;; success
         (dap--send-message (dap--make-success-response seq command)
                            (dap--resp-handler) debug-session))))
    ;; This is really just confirmation response, but good place to ensure session
    ;; selected
    ("launch" (dap--switch-to-session debug-session))
    (_
     (unless dap-inhibit-io
       (message "command: %s wasn't handled by dap-js-debug." command)))))

(provide 'dap-js-debug)

;;; dap-js-debug.el ends here
