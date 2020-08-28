;;; dap-variables-test.el --- Test dap-launch -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
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
;; Tests dap-variables, in particular its launch configuration expansion
;; routines with bugs gathered from issues.

;;; Code:

(require 'ert)
(require 'dap-variables)

(defmacro dap-variables-test--compare (name conf expanded &optional docstring)
  "Make an ert test asserting that CONF, expanded, is EXPANDED.
NAME is the name used in `ert-deftest', as an unquoted symbol."
  `(ert-deftest ,name () ,@docstring
     (let ((prev ,conf))
       (should (equal (dap-variables-expand-in-launch-configuration prev)
                      ,expanded)))))

(dap-variables-test--compare
 dap-variables-test-conspair
 '("${$}" . "test${$}") '("${$}" . "test$")
 "A cons' `car' should not be expanded. Its `cdr' should.")

(dap-variables-test--compare
 dap-variables-test-single-el-list
 '("${$}x") '("$x"))

(dap-variables-test--compare
 dap-variables-test-quoted-var
 '("\\${$}x") '("${$}x")
 "Quoted variables shall not be expanded.")

(dap-variables-test--compare
 dap-variables-test-vec-yields-vec
 ["${$}" "\\${$}"] ["$" "${$}"])

(provide 'dap-variables-test)
;;; dap-variables-test.el ends here
