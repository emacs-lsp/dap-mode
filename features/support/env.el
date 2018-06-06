(require 'f)

(when (require 'undercover nil t)
  (undercover "*.el" "*.el"))

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (defvar dap-java-root-path project-directory)
  (defvar dap-java-test-root (f-join temporary-file-directory "tests")))

(defvar dap-java-support-path
  (f-dirname load-file-name))

(defvar dap-java-features-path
  (f-parent dap-java-support-path))

(defvar dap-handlers-called (make-hash-table :test 'equal))

(defvar dap-java-root-path
  (f-parent dap-java-features-path))

(add-to-list 'load-path dap-java-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'espuds)
  (require 'ert)
  (require 'dap-mode)
  (require 'dap-java)
  (require 'lsp-java)
  (require 'dap-ui))

(Setup)

(Before
 (setq lsp-java-workspace-dir (f-join dap-java-test-root "workspace")
       lsp-java-workspace-cache-dir (f-join dap-java-test-root "workspace-cache/")
       lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/")
       dap-print-io t
       lsp-print-io nil
       lsp-java-bundles (thread-first "eclipse.jdt.ls/plugins/com.microsoft.java.debug.plugin-0.9.0.jar"
                          locate-user-emacs-file
                          expand-file-name list)
       lsp-response-timeout 60)
 (when (file-exists-p dap-java-test-root)
   (delete-directory dap-java-test-root t)))

(After
 (mapc 'kill-buffer
       (seq-filter
        (lambda (b)
          (with-current-buffer b
            (equal 'java-mode major-mode)))
        (buffer-list)))
 (when (get-buffer "*out*")
   (kill-buffer "*out*")))

(Teardown
 ;; After when everything has been run
 )
