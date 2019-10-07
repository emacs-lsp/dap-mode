;;; dap-elixir.el --- Debug Adapter Protocol mode for Elixir      -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; Adapter for https://github.com/elixir-lsp/elixir-ls

;;; Code:

(require 'dap-mode)

(defun dap-elixir--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path '("debugger.sh"))
      (dap--put-if-absent :type "mix_task")
      (dap--put-if-absent :name "mix test")
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :task "test")
      (dap--put-if-absent :taskArgs (list "--trace"))
      (dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
      (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
      (dap--put-if-absent :requireFiles (list
                                         "test/**/test_helper.exs"
                                         "test/**/*_test.exs"))))

(dap-register-debug-provider "Elixir" 'dap-elixir--populate-start-file-args)
(dap-register-debug-template "Elixir Run Configuration"
                             (list :type "Elixir"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Elixir::Run"))

(provide 'dap-elixir)
;;; dap-elixir.el ends here
