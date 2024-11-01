;;; dap-julia.el --- Debug Adapter Protocol mode for Julia      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Michael Kovarik

;; Author: Michael Kovarik <michaelkovarik@outlook.com>
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
;; Adapter for https://github.com/julia-vscode/DebugAdapter.jl

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defun dap-julia--find-project-root (dir)
  "Search upwards from DIR to find Julia project root."
  (let ((parent (file-name-directory (directory-file-name dir))))
    (cond
     ((or (file-exists-p (expand-file-name "Project.toml" dir))
          (file-exists-p (expand-file-name "JuliaProject.toml" dir)))
      dir)
     ((or (null parent) (equal parent dir))
      (error "No Project.toml or JuliaProject.toml found in any parent directories.  Is this a Julia project?"))
     (t (dap-julia--find-project-root parent)))))

(defun dap-julia--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let* ((project-root (dap-julia--find-project-root (file-name-directory (buffer-file-name))))
         (port (dap--find-available-port))
         (command (format "julia --project=%s -e \"import DebugAdapter: DebugSession; using Sockets; \
port = %d; server = listen(port); \
while true; conn = accept(server); @async begin; \
session = DebugSession(conn); run(session); close(conn); end; end;\"" project-root port)))
    (-> conf
        (dap--put-if-absent :cwd project-root)
        (dap--put-if-absent :name "Julia Debug")
        (dap--put-if-absent :debugServer port)
        (dap--put-if-absent :host "localhost")
        (dap--put-if-absent :type "Julia")
        (dap--put-if-absent :program (buffer-file-name))
        (dap--put-if-absent :program-to-start command))))

(dap-register-debug-provider "Julia" #'dap-julia--populate-start-file-args)


(dap-register-debug-template "Julia Run Configuration"
                             (list :type "Julia"
                                   :cwd nil
                                   :request "launch"
                                   :name "Julia Debug"
                                   :sourceMaps t))

(provide 'dap-julia)
;;; dap-julia.el ends here
