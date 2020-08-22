;;; dap-launch-test.el --- Test dap-launch -*- lexical-binding: t -*-

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
;; Tests dap-launch. Its comment deletion function is tested in particular.

;;; Code:

(require 'ert)
(require 'dap-launch)

(defun dap-launch-test--delete-string-comments (s)
  "Delete all comments in S."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))

    (dap-launch-remove-comments)
    (buffer-string)))

(ert-deftest dap-launch-test--delete-comments ()
  (let* ((orig "// removed
/* no nesting // */
// \" // non-string
/* \" // non-string */
\"// string\"
\"/*string*/\"")
         (post-exp (dap-launch-test--delete-string-comments orig)))
    (should (string= post-exp "



\"// string\"
\"/*string*/\""))))

(ert-deftest dap-launch-test--comment-in-string ()
  (let ((orig "\"// orig\""))
    (should (string= orig (dap-launch-test--delete-string-comments orig)))))

(ert-deftest dap-launch-test--comment-star ()
  (let ((orig "/* * **/"))
    (should (string-empty-p (dap-launch-test--delete-string-comments orig)))))

(provide 'dap-launch-test)
;;; dap-launch-test.el ends here
