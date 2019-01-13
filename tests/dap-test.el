;;; dap-mode-test.el --- Unit tests for dap-mode     -*- lexical-binding: t; -*-

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

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'dap-mode)

(ert-deftest dap-mode--parser-read--multiple-messages ()
  (let* ((p (make-dap--parser))
         (messages-in '("Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"
                        "Content-Length: 2\r\n\r\n{}"))
         (messages (dap--parser-read p (string-join messages-in))))
    (should (equal messages '("{}" "{}" "{}" "{}" "{}")))))

(ert-deftest dap--parser-read--multiple-chunks ()
  (let* ((p (make-dap--parser)))
		(should (equal (dap--parser-read p "Content-Length: 14\r\n\r\n{") nil))
		(should (equal (dap--parser-read p "\"somedata\":1") nil))
		(should (equal (dap--parser-read p "}Content-Length: 14\r\n\r\n{")
									 '("{\"somedata\":1}")))
		(should (equal (dap--parser-read p "\"somedata\":2}")
									 '("{\"somedata\":2}")))))

(ert-deftest dap--parser-read--multiple-multibyte-chunks ()
  (let ((p (make-dap--parser)))
		(should (equal (dap--parser-read p "Content-Length: 18\r") nil))
		(should (equal (dap--parser-read p "\n\r\n{\"somedata\":\"\xe2\x80") nil))
		(should (equal (dap--parser-read p "\x99\"}Content-Length: 14\r\n\r\n{")
									 '("{\"somedata\":\"’\"}")))
		(should (equal (dap--parser-read p "\"somedata\":2}")
									 '("{\"somedata\":2}"))))
  (let ((p (make-dap--parser)))
	  (should (equal (dap--parser-read p "Content-Length: 238\r\n\r\n{\"event\":\"output\",\"body\":{\"category\":\"stdout\",\"output\":\"2019-01-13 00:16:36  [ main ] - [ INFO com.inspur.common.utils.PropertiesBuilder.getConfig(32)]  从jar内加载:kafka-producer.properties\\n\",\"type\":\"output\"},\"seq\":9,\"type\":\"event\"}") '("{\"event\":\"output\",\"body\":{\"category\":\"stdout\",\"output\":\"2019-01-13 00:16:36  [ main ] - [ INFO com.inspur.common.utils.PropertiesBuilder.getConfig(32)]  从jar内加载:kafka-producer.properties\\n\",\"type\":\"output\"},\"seq\":9,\"type\":\"event\"}")))))


(provide 'dap-test)
;;; dap-test.el ends here
