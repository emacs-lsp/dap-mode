;;; dap-erlang.el --- Debug Adapter Protocol mode for Erlang      -*- lexical-binding: t; -*-

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
;; Adapter for https://github.com/erlang-ls/erlang_ls
;; Created by Roberto Aloi (@robertoaloi)

;;; Code:

(require 'dap-mode)

(defun dap-erlang--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
    (plist-put :dap-server-path '("els_dap"))
    (plist-put :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
    (plist-put :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
    ))
(dap-register-debug-provider "erlang" 'dap-erlang--populate-start-file-args)

(provide 'dap-erlang)
;;; dap-erlang.el ends here
