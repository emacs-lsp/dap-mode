;;; dap-netcore.el --- Debug Adapter Protocol mode for .NET Core -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
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
;; Adapter for https://github.com/Samsung/netcoredbg .
;;; Code:

(require 'dap-mode)
(require 'f)
(require 'dom)

(defcustom dap-netcore-install-dir (f-join user-emacs-directory ".cache" "lsp" "netcoredbg")
  "Install directory for netcoredbg."
  :group 'dap-netcore
  :risky t
  :type 'directory)

(defcustom dap-netcore-download-url nil
  "Netcoredbg download url.
See asset links here https://github.com/Samsung/netcoredbg/releases/ and select
the correct one for your OS.  Will be set automatically in Emacs 27.1 or newer
with libxml2 support."
  :group 'dap-netcore
  :risky t
  :type 'string)

(defun dap-netcore-update-debugger ()
  "Update netcoredbg."
  (interactive)
  (let ((backup (concat dap-netcore-install-dir ".old")))
    (when (f-exists-p dap-netcore-install-dir)
      (f-move dap-netcore-install-dir backup))
    (condition-case err
        (dap-netcore--debugger-install)
      (error (f-move backup dap-netcore-install-dir)
             (signal (car err) (cdr err)))
      (:success (when (f-exists-p backup)
                  (f-delete backup t))))))

(defun dap-netcore--debugger-install ()
  "Download the latest version of netcoredbg and extract it
to `dap-netcore-install-dir'."
  (let* ((temp-file (make-temp-file "netcoredbg" nil
                                    (if (eq system-type 'windows-nt)
                                        ".zip"
                                      ".tar.gz")))
         (install-dir-full (expand-file-name dap-netcore-install-dir))
         (unzip-script (pcase system-type
                         (`windows-nt (format "powershell -noprofile -noninteractive -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file install-dir-full))
                         ((or `gnu/linux `darwin) (format "mkdir -p %s && tar xvzf %s -C %s" dap-netcore-install-dir temp-file dap-netcore-install-dir))
                         (_ (user-error (format "Unable to extract server - file %s cannot be extracted, please extract it manually" temp-file))))))
    (if (and (not dap-netcore-download-url)
             (fboundp 'libxml-available-p)
             (fboundp 'dom-search)
             (fboundp 'dom-attr))
        (url-retrieve "https://github.com/Samsung/netcoredbg/releases"
                      (lambda (_)
                        (setq dap-netcore-download-url
                              (concat
                               "https://github.com"
                               (dom-attr
                                (dom-search
                                 (if (libxml-available-p)
                                     (libxml-parse-html-region (point-min) (point-max))
                                   (xml-parse-region (point-min) (point-max)))
                                 (lambda (node)
                                   (string-match-p (pcase system-type
                                                     (`gnu/linux (if (string-match-p system-configuration ".*arm")
                                                                     ".*linux-arm64\\.tar\\.gz"
                                                                   ".*linux-amd64\\.tar\\.gz"))
                                                     (`darwin ".*osx.*\\.tar\\.gz")
                                                     (`windows-nt ".*win64.*\\.zip"))
                                                   (or (dom-attr node 'href) ""))))
                                'href)))
                        (lsp-download-install
                         (lambda (&rest _)
                           (shell-command unzip-script))
                         (lambda (error &rest _)
                           (user-error "Error during netcoredbg downloading: %s" error))
                         :url dap-netcore-download-url
                         :store-path temp-file)))
      (if dap-netcore-download-url
          (lsp-download-install
           (lambda (&rest _)
             (shell-command unzip-script))
           (lambda (error &rest _)
             (user-error "Error during netcoredbg downloading: %s" error))
           :url dap-netcore-download-url
           :store-path temp-file)
        (user-error "`dap-netcore-download-url' is not set. You can customize it")))))

(defun dap-netcore--debugger-cmd ()
  "The location of the netcoredbg executable."
  (let ((file-ext (pcase system-type
                    (`windows-nt ".exe")
                    (_ ""))))
    (or
     (executable-find "netcoredbg")
     (expand-file-name (concat "netcoredbg" file-ext) (f-join dap-netcore-install-dir "netcoredbg")))))

(defun dap-netcore--debugger-locate-or-install ()
  "Return the location of netcoredbg."
  (let ((dbg (dap-netcore--debugger-cmd)))
    (unless (file-exists-p dbg)
      (if (yes-or-no-p "Netcoredbg is not installed. Do you want to install it?")
          (dap-netcore--debugger-install)
        (error "Cannot start debugger configuration without netcoredbg")))
    dbg))

(defun dap-netcore--locate-dominating-file-wildcard (file name)
  "Starting at FILE, look up directory hierarchy for directory containing NAME.
FILE can be a file or a directory.  If it's a file, its directory will
serve as the starting point for searching the hierarchy of directories.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking.  The predicate will be called with every file/directory
the function needs to examine, starting with FILE."
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `name' in /home or in /.
  (setq file (abbreviate-file-name (expand-file-name file)))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (and (file-directory-p file)
                         (file-expand-wildcards (f-join file name)))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (if root (file-name-as-directory root))))

(defun dap-netcore--populate-args (conf)
  "Populate CONF with arguments to launch or attach netcoredbg."
  (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate-or-install) "--interpreter=vscode"))
  (pcase (plist-get conf :mode)
    ("launch"
     (dap--put-if-absent
      conf
      :program
      (let ((project-dir (f-full
                          (or
                           (dap-netcore--locate-dominating-file-wildcard
                            default-directory "*.*proj")
                           (lsp-workspace-root)))))
        (save-mark-and-excursion
          (find-file (concat (f-slash project-dir) "*.*proj") t)
          (let ((res (if (libxml-available-p)
                         (libxml-parse-xml-region (point-min) (point-max))
                       (xml-parse-region (point-min) (point-max)))))
            (kill-buffer)
            (f-join project-dir "bin" "Debug"
                    (dom-text (dom-by-tag res 'TargetFramework))
                    (dom-text (dom-by-tag res 'RuntimeIdentifier))
                    (concat (car (-take-last 1 (f-split project-dir))) ".dll")))))))
    ("attach"
     (dap--put-if-absent conf :processId (string-to-number (read-string "Enter PID: " "2345"))))))

(dap-register-debug-provider
 "coreclr"
 'dap-netcore--populate-args)

(dap-register-debug-template ".Net Core Attach (Console)"
                             (list :type "coreclr"
                                   :request "attach"
                                   :mode "attach"
                                   :name "NetCoreDbg::Attach"))

(dap-register-debug-template ".Net Core Launch (Console)"
                             (list :type "coreclr"
                                   :request "launch"
                                   :mode "launch"
                                   :name "NetCoreDbg::Launch"
                                   :dap-compilation "dotnet build"))

(defun dap-netcore-debug-attach (pid)
  "Attach the debugger to a .NET process with a given PID using the registered template."
  (let* ((config-name ".Net Core Attach (Console)")
         (config-cell (assoc config-name dap-debug-template-configurations))
         (config-plist (cdr config-cell)))
    (setcdr config-cell (plist-put config-plist :processId pid))
    (dap-debug (cdr config-cell))))

(defun dap-netcore-test-run (attach-buffer &optional args-string)
  "Run .NET tests process to obtain PID to attach for debugging."
  (with-environment-variables (("VSTEST_HOST_DEBUG" "1"))
    (start-process "dap-netcore-attach-process"
                   attach-buffer
                   "dotnet"
                   "test"
                   "--verbosity=Quiet"
                   (concat "" args-string))))

(defun dap-netcore-debug-tests-filter-pid (process output)
  "Custom filter to extract PID from the process output in real-time."
  (let ((buffer (process-buffer process)))
    ;; Insert the output into the buffer
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (if moving (goto-char (process-mark process))))))
    ;; Check for PID in the buffer
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "Process Id: " nil t)
            (let ((pid-string (buffer-substring (point) (line-end-position))))
              ;; Debug with PID
              (dap-netcore-debug-attach (string-to-number pid-string))
              ;; Remove the filter to avoid further checks
              (set-process-filter process nil))))))))

(defun dap-netcore-debug-test-init (&optional args-string)
  "Prepare .NET process to attach its PID for debugging."
  (let ((attach-buffer "*dap-netcore-attach*"))
    ;; Kill existing buffer if it exists
    (when (get-buffer attach-buffer)
      (kill-buffer attach-buffer))
    ;; Run dotnet process
    (let ((dotnet-process (dap-netcore-test-run attach-buffer args-string)))
      (when dotnet-process
        (set-process-filter dotnet-process #'dap-netcore-debug-tests-filter-pid)
        ;; Set process finalization event
        (set-process-sentinel
         dotnet-process
         (lambda (process event)
           (when (string-match "exited\\|finished" event)
             (message "Process exited with status: %d" (process-exit-status process))
             (display-buffer attach-buffer))))))))

(defun dap-netcore-debug-test (&optional directory)
  "Debug .NET tests with optional params."
  (interactive)
  (let ((params '())
        (filter (read-string "Filter: ")))
    (unless (string-empty-p filter)
      (push (concat params " --filter=" filter) params))
    (when directory
      (push directory params))
    (dap-netcore-debug-test-init (string-join (reverse params) " "))))

(provide 'dap-netcore)
;;; dap-netcore.el ends here
