(require 'f)

(require 'undercover nil t)
;; (undercover "*.el" (:exclude "*-test.el"))

(defun dap-steps--wait-minibuffer()
  (interactive)
  (sit-for 1))

(defvar dap-java-support-path (f-dirname load-file-name))
(defvar dap-java-features-path (f-parent dap-java-support-path))
(defvar dap-handlers-called (make-hash-table :test 'equal))
(defvar dap-java-maven-project-root (f-join dap-java-support-path "../fixtures/"))
(defvar dap-java-root-path (f-parent dap-java-features-path))
(defvar dap-java-test-root (f-join temporary-file-directory "tests"))

(add-to-list 'load-path dap-java-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'espuds)
  (require 'ert)
  (require 'dap-mode)
  (require 'dap-java)
  (require 'lsp-java)
  (require 'dap-ui))

(add-hook 'java-mode-hook 'lsp)

(defun dap--get-sessions ()
  "Get sessions for WORKSPACE."
  (lsp-workspace-get-metadata "debug-sessions"))

(Setup
 (setq lsp-java-workspace-dir (f-join dap-java-test-root "workspace")
       lsp-java-workspace-cache-dir (f-join dap-java-test-root "workspace-cache/")
       lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
       dap-print-io t
       dap-inhibit-io nil
       lsp-java-bundles (thread-first "eclipse.jdt.ls/plugins/com.microsoft.java.debug.plugin-0.10.0.jar"
                          locate-user-emacs-file
                          expand-file-name
                          list)
       lsp-response-timeout 60)

 (lsp-java-update-server)
 (when (file-exists-p dap-java-test-root)
   (delete-directory dap-java-test-root t))
 (mkdir lsp-java-workspace-dir t)
 (mkdir lsp-java-workspace-cache-dir t)

 (dap-turn-on-dap-mode)
 (lsp-workspace-folders-add (f-join dap-java-maven-project-root "test-project"))

 (find-file (f-join dap-java-maven-project-root "pom.xml"))
 (lsp)
 (toggle-debug-on-error))

(Before)

(After
 (with-current-buffer "pom.xml"
   (dap-breakpoint-delete-all)
   (dap-delete-all-sessions))

 (when (get-buffer "*out*")
   (kill-buffer "*out*")))

(Teardown)
