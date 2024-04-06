;;; dap-docker.el --- Debug Adapter Protocol mode for docker-wrapped servers      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Andrei Mochalov

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
;; Debug Adapter Protocol client support for Emacs and docker-wrapped servers.

;;; Code:

(require 'dap-mode)
(require 'lsp-mode)
(require 'lsp-docker)
(require 'f)
(require 's)
(require 'ht)
(require 'dash)
(require 'yaml)

(defvar dap-docker-supported-server-types-subtypes
  (ht ('docker (list 'container 'image)))
  "A list of all supported server types and subtypes, currently only docker
is supported.")

(defun dap-docker--is-enabled? (config)
  "Check whether debugging is enabled"
  (-when-let* ((server-info (gethash 'debug config))
               (server-enabled (gethash 'enabled server-info)))
    (not (equal server-enabled :false))))

(defun dap-docker--get-debug-provider-name (config)
  "Get the debug provider name also checking whether debugging is enabled"
  (-if-let* ((server-info (gethash 'debug config))
             (server-enabled (dap-docker--is-enabled? config)))
      (gethash 'provider server-info)
    (user-error "Either debug is not enabled or the config is invalid!")))

(defun dap-docker--get-debug-template-name (config)
  "Get the debug template name also checking whether debugging is enabled"
  (-if-let* ((server-info (gethash 'debug config))
             (server-enabled (dap-docker--is-enabled? config)))
      (gethash 'template server-info)
    (user-error "Either debug is not enabled or the config is invalid!")))

(defun dap-docker--get-server-type-subtype (config)
  "Get the server type"
  (-if-let* ((server-info (gethash 'debug config))
             (server-type (gethash 'type server-info))
             (server-subtype (gethash 'subtype server-info)))
      (cons (if (stringp server-type)
                (intern server-type)
              server-type)
            (if (stringp server-subtype)
                (intern server-subtype)
              server-subtype))
    (lsp-docker-get-server-type-subtype config)))

(defun dap-docker--get-server-container-name (config)
  "Get the server container name"
  (-if-let* ((server-info (gethash 'debug config))
             (server-subtype (gethash 'subtype server-info)))
      (if (equal server-subtype "container")
          (gethash 'name server-info))
    (user-error "No debug container names are specified, you cannot use single container both for a language server and a debugging server")))

(defun dap-docker--get-server-image-name (config)
  "Get the server image name"
  (-if-let* ((server-info (gethash 'debug config))
             (server-subtype (gethash 'subtype server-info)))
      (if (equal server-subtype "image")
          (gethash 'name server-info))
    (lsp-docker-get-server-image-name config)))

(defun dap-docker--get-server-id (config)
  "Get the server id"
  (let ((server-info (gethash 'debug config)))
    (if (stringp (gethash 'server server-info))
        (intern (gethash 'server server-info))
      (gethash 'server server-info))))

(defun dap-docker--get-path-mappings (config project-root)
  "Get the server path mappings"
  (lsp-docker-get-path-mappings config project-root))

(defun dap-docker--get-launch-command (config)
  "Get the server launch command"
  (let ((server-info (gethash 'debug config)))
    (gethash 'launch_command server-info)))

(defun dap-docker--check-server-type-subtype (supported-server-types-subtypes server-type-subtype)
  "Verify that the combination of server (type . subtype) is supported by the
current implementation"
  (if (not server-type-subtype)
      (user-error "No server type and subtype specified!"))
  (if (ht-find (lambda (type subtypes)
                 (let ((server-type (car server-type-subtype))
                       (server-subtype (cdr server-type-subtype)))
                   (if (and (equal server-type type) (-contains? subtypes server-subtype))
                       t)))
               supported-server-types-subtypes)
      server-type-subtype
    (user-error "No compatible server type and subtype found!")))

(defun dap-docker--check-path-mappings (path-mappings)
  "Verify that specified path mappings are all inside the project directory"
  (--all? (or (f-descendant-of? (f-canonical (car it)) (f-canonical (lsp-workspace-root)))
              (f-same? (f-canonical (car it)) (f-canonical (lsp-workspace-root))))
          path-mappings))

(defun dap-docker--verify-path-mappings-against-container (path-mappings container-name)
  "Verify that specified path mappings are all included in container's
path mappings."
  (--all? (let ((source (car it))
                (destination (cdr it)))
            (-any? (lambda (mapping)
                     (and (f-same? source (car mapping))
                          (equal destination (cdr mapping))))
                   (dap-docker--get-path-mappings-from-container container-name)))
          path-mappings))

(defun dap-docker--get-path-mappings-from-container (container-name)
  "Get path mappings from a container"
  (-let (((inspection-command-program . inspection-command-arguments)
          (--map-when
           (equal it "'{{.Mounts}}'")
           "'{{json .Mounts}}'"
           (s-split " " (format "%s container inspect -f '{{.Mounts}}' %s" lsp-docker-command container-name)))))
    (-let (((exit-code . raw-output) (with-temp-buffer
                                       (cons
                                        (apply #'call-process inspection-command-program nil (current-buffer) nil inspection-command-arguments)
                                        (buffer-string)))))
      (if (equal exit-code 0)
          (let* ((output (s-chop-prefix "'" (s-chop-suffix "'" (s-chomp raw-output))))
                 (raw-mappings (append (json-parse-string output) nil)) ; using append to convert a vector to a list
                 (bind-mappings (--filter (and (equal (gethash "Type" it) "bind") (equal (gethash "RW" it) t)) raw-mappings)))
            (--map (cons (f-canonical (gethash "Source" it)) (f-canonical (gethash "Destination" it))) bind-mappings))
        (user-error "Cannot analyze the following container: %s, exit code: %d" container-name exit-code)))))

(defconst dap-docker-image-start-string "docker run --rm &mappings& -i &name& &entrypoint&")
(defconst dap-docker-container-start-string "docker start -i &name&")

(defun dap-docker--prepare-image-start-path (image-name path-mappings entrypoint)
  "Get a command (splitted by spaces in a list form) for launching a server in
a docker image."
  (let* ((command (copy-sequence dap-docker-image-start-string))
         (mappings-list (--map (s-join " " (list "-v" (s-join ":" (list (car it) (cdr it))))) path-mappings))
         (mappings (s-join " " mappings-list)))
    (--> command
         (s-replace "&name&" image-name it)
         (s-replace "&mappings&" mappings it)
         (s-replace "&entrypoint&" entrypoint it))))

(defun dap-docker--prepare-container-start-path (container-name _ _)
  "Get a command (splitted by spaces in a list form) for launching a server in
a docker container."
  (let ((command (copy-sequence dap-docker-container-start-string)))
    (--> command
         (s-replace "&name&" container-name it))))

(defun dap-docker--get-dockerized-debug-provider-name (debug-provider-name project-root)
  "Create a new debug provider name using the original one and the project root."
  (s-join "-"
          (list
           (->> project-root
                (s-replace-all '(("/" . "-") ("." . "")))
                (s-chop-suffix "-")
                (s-chop-prefix "-"))
           (s-concat debug-provider-name "Docker"))))

(defun dap-docker--dockerize-start-file-args (conf project-config project-root)
  "Add container launching and path mapping wrappers to start arguments"
  (let* ((provider-name (plist-get conf :type))
         (original-launch-command (if (listp (plist-get conf :dap-server-path))
                                      (s-join " " (plist-get conf :dap-server-path))
                                    (plist-get conf :dap-server-path)))
         (original-name (plist-get conf :name))
         (launch-command (or (dap-docker--get-launch-command project-config) original-launch-command))
         (path-mappings (dap-docker--get-path-mappings project-config project-root))
         (server-type (car (dap-docker--check-server-type-subtype dap-docker-supported-server-types-subtypes (dap-docker--get-server-type-subtype project-config))))
         (server-subtype (cdr (dap-docker--check-server-type-subtype dap-docker-supported-server-types-subtypes (dap-docker--get-server-type-subtype project-config))))
         (container-name (dap-docker--get-server-container-name project-config))
         (image-name (dap-docker--get-server-image-name project-config)))
    (cond ((equal server-subtype 'container)
           (-> conf
               (plist-put :dap-server-path (s-split " " (dap-docker--prepare-container-start-path container-name path-mappings launch-command)))
               (plist-put :name (s-concat original-name "DockerContainer"))
               (plist-put :path-mappings path-mappings)
               (plist-put :local-to-remote-path-fn (-partial #'dap--local-to-remote-path path-mappings))
               (plist-put :remote-to-local-path-fn (-partial #'dap--remote-to-local-path path-mappings))))
          ((equal server-subtype 'image)
           (-> conf
               (plist-put :dap-server-path (s-split " " (dap-docker--prepare-image-start-path image-name path-mappings launch-command)))
               (plist-put :name (s-concat original-name "DockerImage"))
               (plist-put :path-mappings path-mappings)
               (plist-put :local-to-remote-path-fn (-partial #'dap--local-to-remote-path path-mappings))
               (plist-put :remote-to-local-path-fn (-partial #'dap--remote-to-local-path path-mappings)))))))

(defun dap-docker--dockerize-debug-provider (debug-provider-name project-config project-root)
  "Make a particular debug provider docker-aware in a project folder"
  (-if-let* ((original-debug-provider (gethash debug-provider-name dap--debug-providers)))
      (dap-register-debug-provider (dap-docker--get-dockerized-debug-provider-name debug-provider-name project-root)
                                   `(lambda (conf)
                                      (funcall #'dap-docker--dockerize-start-file-args (funcall (quote ,original-debug-provider) conf) ,project-config ,project-root)))))

(defun dap-docker--dockerize-debug-template (debug-template-name project-config project-root)
  "Make a particular debug template docker-aware in a project folder"
  (-if-let* ((original-debug-template (--first (let* ((key (-first-item it))
                                                      (properties (-slice it 1))
                                                      (name (plist-get properties :name))
                                                      (type (plist-get properties :type)))
                                                 (equal name debug-template-name))
                                               dap-debug-template-configurations)))
      (let* ((key (-first-item original-debug-template))
             (properties (-slice original-debug-template 1))
             (name (plist-get properties :name)))
        (dap-register-debug-template
         (s-concat key ": Dockerized")
         (-> (copy-sequence properties)
             (plist-put :type (dap-docker--get-dockerized-debug-provider-name (dap-docker--get-debug-provider-name project-config) project-root))
             (plist-put :name (s-concat name "Dockerized")))))))

(defun dap-docker-register ()
  "Make the current project debugger docker-aware"
  (interactive)
  (-if-let* ((config (lsp-docker-get-config-from-lsp))
             (project-root (lsp-workspace-root))
             (original-provider-name (dap-docker--get-debug-provider-name config))
             (original-template-name (dap-docker--get-debug-template-name config))
             (containerized-debugging-enabled (dap-docker--is-enabled? config)))
      (progn
        (dap-docker--dockerize-debug-provider original-provider-name config project-root)
        (dap-docker--dockerize-debug-template original-template-name config project-root))
    (user-error "Containerized debugging is not enabled or the config is invalid!")))

(provide 'dap-docker)
;;; dap-docker.el ends here
