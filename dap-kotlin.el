;; Commentary:
;; Adapter for https://github.com/fwcd/kotlin-debug-adapter

(require 'lsp-kotlin)
(require 'dap-mode)

(defun dap-kotlin-populate-launch-args (conf)
  ;; we require mainClass and projectRoot to be filled in in the launch configuration.
  ;; dap-kotlin does currently not support a fallback if not defined
  (-> conf
	  (dap--put-if-absent :request "launch")
	  (dap--put-if-absent :name "Kotlin Launch")))

(defun dap-kotlin-populate-attach-args (conf)
  (-> conf
      (dap--put-if-absent :request "attach")
      (dap--put-if-absent :name "Kotlin Attach")
      (dap--put-if-absent :projectRoot (lsp-workspace-root))
      (dap--put-if-absent :hostName "localhost")
      (dap--put-if-absent :port 5005)
      (dap--put-if-absent :timeout 2000)))

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
