;;; dap-java.el --- Debug Adapter Protocol mode for Java      -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0") (lsp-java "0.1"))
;; Version: 0.2
;; DAP Adapter for java

;;; Commentary:

;;; Code:

(require 'lsp-mode)
(require 'lsp-java)
(require 'dap-mode)

(defvar dap-java--classpath-separator (if (string= system-type "windows-nt")
                                          ";"
                                        ":"))

(defvar dap-java--var-format (if (string= system-type "windows-nt")
                                 "%%%s%%"
                               "$%s"))

(defcustom dap-java-java-command "java"
  "Path of the java executable."
  :group 'dap-java
  :type 'string)

(defcustom  dap-java-compile-port 33000
  "The debug port which will be used for compile/attach configuration.
If the port is taken, DAP will try the next port."
  :group 'dap-java
  :type 'number)

(defcustom dap-java-test-runner
  (expand-file-name (locate-user-emacs-file "eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar"))
  "DAP Java test runner."
  :group 'dap-java-java
  :type 'file)

(defcustom dap-java-build 'ask
  "Perform build before running project behaviour."
  :group 'dap-java
  :type '(choice (const ask)
                 (const always)
                 (const never)))

(defcustom dap-java-test-additional-args ()
  "Additional arguments for JUnit standalone runner."
  :group 'dap-java
  :type '(list string))

(defcustom  dap-java-default-debug-port 1044
  "Default debug port."
  :group 'dap-java
  :type 'number)

(defun dap-java-test-class ()
  "Get class FDQN."
  (-if-let* ((symbols (lsp--get-document-symbols))
             (package-name (-some->> symbols
                                     (-first (-lambda ((&hash "kind")) (= kind 4)))
                                     (gethash "name")))
             (class-name (->> symbols
                              (--first (= (gethash "kind" it) 5))
                              (gethash "name"))))
      (concat package-name "." class-name)
    (user-error "No class found")))

(defun dap-java-test-method-at-point ()
  "Get method at point."
  (-let* ((symbols (lsp--get-document-symbols))
          (package-name (-some->> symbols
                                  (-first (-lambda ((&hash "kind")) (= kind 4)))
                                  (gethash "name"))))
    (or (->> symbols
             (-keep (-lambda ((&hash "children" "kind" "name" class-name))
                      (and (= kind 5)
                           (seq-some
                            (-lambda ((&hash "kind" "range" "selectionRange" selection-range))
                              (-let (((beg . end) (lsp--range-to-region range)))
                                (and (= 6 kind ) (<= beg (point) end)
                                     (concat package-name "." class-name "#"
                                             (lsp-region-text selection-range)))))
                            children))))
             (cl-first))
        (user-error "No method at point"))))

(defun dap-java--select-main-class ()
  "Select main class from the current workspace."
  (let* ((main-classes (lsp-send-execute-command "vscode.java.resolveMainClass"))
         (main-classes-count (length main-classes))
         current-class)
    (cond
     ((= main-classes-count 0) (error "Unable to find main class.
Please check whether the server is configured propertly"))
     ((= main-classes-count 1) (cl-first main-classes))
     ((setq current-class (--first (string= buffer-file-name (gethash "filePath" it))
                                   main-classes))
      current-class)
     (t (dap--completing-read "Select main class to run: "
                              main-classes
                              (lambda (it)
                                (format "%s(%s)"
                                        (gethash "mainClass" it)
                                        (gethash "projectName" it)))
                              nil
                              t)))))

(defun dap-java--populate-launch-args (conf)
  "Populate CONF with launch related configurations."
  (when (not (and (plist-get conf :mainClass)
                  (plist-get conf :projectName)))
    (-let [(&hash "mainClass" main-class "projectName" project-name) (dap-java--select-main-class)]
      (setq conf (plist-put conf :mainClass main-class))
      (plist-put conf :projectName project-name)))

  (-let [(&plist :mainClass main-class :projectName project-name) conf]
    (dap--put-if-absent conf :args "")
    (dap--put-if-absent conf :cwd (lsp-java--get-root))
    (dap--put-if-absent conf :stopOnEntry :json-false)
    (dap--put-if-absent conf :host "localhost")
    (dap--put-if-absent conf :request "launch")
    (dap--put-if-absent conf :modulePaths (vector))
    (dap--put-if-absent conf
                        :classPaths
                        (or (cl-second
                             (lsp-send-execute-command "vscode.java.resolveClasspath"
                                                       (vector main-class project-name)))
                            (error "Unable to resolve classpath")))
    (dap--put-if-absent conf :name (format "%s (%s)"
                                           (if (string-match ".*\\.\\([[:alnum:]_]*\\)$" main-class)
                                               (match-string 1 main-class)
                                             main-class)
                                           project-name))
    conf))

(defun dap-java--populate-attach-args (conf)
  "Populate attach arguments.
CONF - the startup configuration."
  (dap--put-if-absent conf :hostName (read-string "Enter host: " "localhost"))
  (dap--put-if-absent conf :port (string-to-number (read-string "Enter port: "
                                                                (number-to-string dap-java-default-debug-port))))
  (dap--put-if-absent conf :host "localhost")
  (dap--put-if-absent conf :name (format "%s(%s)"
                                         (plist-get conf :host)
                                         (plist-get conf :port)))
  conf)

(defun dap-java--populate-compile-attach-args (conf)
  "Populate the CONF for running compile/attach.
Populate the arguments like normal 'Launch' request but then
initiate `compile' and attach to the process."
  (dap-java--populate-launch-args conf)
  (-let* (((&plist :mainClass :projectName :classPaths classpaths) conf)
          (port   (dap--find-available-port "localhost" dap-java-compile-port))
          (program-to-start (format "%s -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=%s,quiet=y -cp %s %s"
                                    dap-java-java-command
                                    port
                                    (format dap-java--var-format "CLASSPATH_ARGS")
                                    mainClass)))
    (dap-java--populate-attach-args
     (list :type "java"
           :request "attach"
           :hostName "localhost"
           :projectName projectName
           :host "localhost"
           :wait-for-port t
           :program-to-start program-to-start
           :port port
           :environment-variables `(("CLASSPATH_ARGS" . ,(s-join dap-java--classpath-separator classpaths)))))))

(defun dap-java--populate-default-args (conf)
  "Populate all of the fields that are not present in CONF."
  (setq conf (plist-put conf :type "java"))

  (setq conf (pcase (plist-get conf :request)
               ("launch" (dap-java--populate-launch-args conf))
               ("attach" (dap-java--populate-attach-args conf))
               ("compile_attach" (dap-java--populate-compile-attach-args conf))
               (_ (dap-java--populate-launch-args conf))))
  (plist-put conf :debugServer (lsp-send-execute-command "vscode.java.startDebugSession"))
  (plist-put conf :__sessionId (number-to-string (float-time)))
  conf)

(defun dap-java-debug (debug-args)
  "Start debug session with DEBUG-ARGS."
  (interactive (list (dap-java--populate-default-args nil)))
  (dap-start-debugging debug-args))

(defun dap-java--run-unit-test-command (runner run-method?)
  "Run debug test with the following arguments.
RUNNER is the test executor. RUN-METHOD? when t it will try to
run the surrounding method. Otherwise it will run the surronding
test."
  (-let* ((to-run (if run-method?
                      (dap-java-test-method-at-point)
                    (dap-java-test-class)))
          (test-class-name (cl-first (s-split "#" to-run)))
          (class-path (->> (vector test-class-name nil)
                           (lsp-send-execute-command "vscode.java.resolveClasspath")
                           cl-second
                           (s-join dap-java--classpath-separator))))
    (list :program-to-start (s-join " "
                                    (cl-list* runner "-jar" dap-java-test-runner
                                              "-cp" (format dap-java--var-format "JUNIT_CLASS_PATH")
                                              (if (and (s-contains? "#" to-run) run-method?) "-m" "-c")
                                              (if run-method? to-run test-class-name)
                                              dap-java-test-additional-args))
          :environment-variables `(("JUNIT_CLASS_PATH" . ,class-path))
          :cwd (lsp-java--get-root))))

(defun dap-java-run-test-method ()
  "Run JUnit test.
If there is no method under cursor it will fallback to test class."
  (interactive)
  (-> (dap-java--run-unit-test-command dap-java-java-command t)
      (plist-put :skip-debug-session t)
      dap-start-debugging))

(defun dap-java-debug-test-method (port)
  "Debug JUnit test.
If there is no method under cursor it will fallback to test class.
PORT is the port that is going to be used for starting and
attaching to the test."
  (interactive (list (dap--find-available-port "localhost" dap-java-compile-port)))
  (-> (list :type "java"
            :request "attach"
            :hostName "localhost"
            :port port
            :wait-for-port t)
      (append (dap-java--run-unit-test-command
               (format "%s -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=%s"
                       dap-java-java-command
                       port)
               t))
      dap-debug))

(defun dap-java-run-test-class ()
  "Run JUnit test."
  (interactive)
  (-> (dap-java--run-unit-test-command dap-java-java-command nil)
      (plist-put :skip-debug-session t)
      dap-start-debugging))

(defun dap-java-debug-test-class (port)
  "Debug JUnit test class.

PORT is the port that is going to be used for starting and
attaching to the test."
  (interactive (list (dap--find-available-port "localhost" dap-java-compile-port)))
  (dap-debug
   (append (list :type "java"
                 :request "attach"
                 :hostName "localhost"
                 :port port
                 :wait-for-port t)
           (dap-java--run-unit-test-command
            (format "%s -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=%s"
                    dap-java-java-command
                    port)
            nil))))

(dap-register-debug-provider "java" 'dap-java--populate-default-args)
(dap-register-debug-template "Java Run Configuration"
                             (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :cwd nil
                                   :stopOnEntry :json-false
                                   :host "localhost"
                                   :request "launch"
                                   :modulePaths (vector)
                                   :classPaths nil
                                   :name "Run Configuration"
                                   :projectName nil
                                   :mainClass nil))
(dap-register-debug-template "Java Run Configuration (compile/attach)"
                             (list :type "java"
                                   :request "compile_attach"
                                   :args ""
                                   :cwd nil
                                   :host "localhost"
                                   :request "launch"
                                   :modulePaths (vector)
                                   :classPaths nil
                                   :name "Run"
                                   :projectName nil
                                   :mainClass nil))
(dap-register-debug-template "Java Attach"
                             (list :type "java"
                                   :request "attach"
                                   :hostName "localhost"
                                   :port nil))

(provide 'dap-java)
;;; dap-java.el ends here
