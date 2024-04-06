;;; dap-gdscript.el --- Debug Adapter Protocol mode for gdscript      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  xiliuya

;; Author:  xiliuya <xiliuya@aliyun.com>
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

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-gdscript-debug-port 6006
  "The port to the gdscript/godot4 debugger."
  :group 'dap-gdscript
  :type 'number)

(defcustom dap-gdscript-debug-host "127.0.0.1"
  "The host to the gdscript/godot4 debugger."
  :group 'dap-gdscript
  :type 'string)

(defun dap-gdscript--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :host dap-gdscript-debug-host)
                  (dap--put-if-absent :debugServer dap-gdscript-debug-port)
                  (dap--put-if-absent :type "gdscript")
                  (dap--put-if-absent :cwd default-directory)
                  (dap--put-if-absent :name "Gdscript Debug")
                  (dap--put-if-absent :args ""))))
    conf))

(dap-register-debug-provider "gdscript" #'dap-gdscript--populate-start-file-args)

(dap-register-debug-template "Gdscript Run Configuration"
                             (list :type "gdscript"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Gdscript::Run"))

(provide 'dap-gdscript)
;;; dap-gdscript.el ends here
