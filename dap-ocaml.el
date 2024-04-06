;;; dap-mode/dap-ocaml.el -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Austin Theriault

;; Author: Austin Theriault <austin@cutedogs.org>
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
;; Adapter for OCaml Earlybird (https://github.com/hackwaly/ocamlearlybird)

;;; Code:

(require 'dap-mode)

(defcustom dap-ocaml-executable "ocamlearlybird"
  "Path to the OCaml Earlybird executable."
  :group 'dap-ocaml
  :risky t
  :type 'file)

(defun dap-ocaml--populate-start-file-args (conf)
  "Populate CONF with the start file argument."
  (-> conf
      (dap--put-if-absent :console "internalConsole") ;; integratedTerminal doesn't work
      (dap--put-if-absent :dap-server-path (list dap-ocaml-executable "debug"))))

(dap-register-debug-provider "ocaml.earlybird" 'dap-ocaml--populate-start-file-args)
(dap-register-debug-template "OCaml Debug Template"
                             (list :type "ocaml.earlybird"
                                   :request "launch"
                                   :name "OCaml::Run"
                                   :program nil
                                   :arguments nil))


(provide 'dap-ocaml)
;;; dap-ocaml.el ends here
