;;; dap-swi-prolog.el --- Debug Adapter Protocol mode for SWI-Prolog      -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Eshel Yaron

;; Author: Eshel Yaron <eshelshay.yaron@gmail.com>
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
;; Version: 0.3

;;; Commentary:
;; Adapter for https://www.swi-prolog.org

;;; Code:

(require 'dap-mode)

(defcustom dap-swi-prolog-debug-program
    '("swipl" "-g" "[library(debug_adapter/main)]" "-t" "halt")
  "The path to the SWI-Prolog debug adapter."
  :group 'dap-swi-prolog
  :type '(repeat string))

(defun dap-swi-prolog--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :dap-server-path dap-swi-prolog-debug-program)
                  (dap--put-if-absent :type "swi-prolog")
                  (dap--put-if-absent :cwd default-directory)
                  (dap--put-if-absent :module (buffer-file-name))
                  (dap--put-if-absent :goal (read-string "?- " nil nil "true"))
                  (dap--put-if-absent :name "SWI-Prolog Debug"))))
    conf))

(dap-register-debug-provider "swi-prolog" #'dap-swi-prolog--populate-start-file-args)

(dap-register-debug-template "SWI-Prolog Run Configuration"
                             (list :type "swi-prolog"
                                   :request "launch"
                                   :name "SWI-Prolog::Run"))
(dap-register-debug-template "SWI-Prolog Start Terminal"
                             (list :type "swi-prolog"
                                   :goal "$run_in_terminal"
                                   :request "launch"
                                   :name "SWI-Prolog::Terminal"))

(defun dap-swi-prolog--populate-start-tcp-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :host "localhost")
                  (dap--put-if-absent :debugServer 3443)
                  (dap--put-if-absent :request "attach")
                  (dap--put-if-absent :name "SWI-Prolog::Connected"))))
    conf))

(dap-register-debug-provider "swi-prolog-tcp" #'dap-swi-prolog--populate-start-tcp-args)

(provide 'dap-swi-prolog)
;;; dap-swi-prolog.el ends here
