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


(ert-deftest dap--python-test-free-function ()
  (let* ((document-symbols
	  (list
	   #s(lsp-symbol "dataclass" "Function" #s(location #s(point 0 0) #s(point 0 33)))
	   #s(lsp-symbol "Foo" "Class" #s(location #s(point 4 0) #s(point 6 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 5 4) #s(point 5 14)))
	   #s(lsp-symbol "Bar" "Class" #s(location #s(point 9 0) #s(point 11 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 10 4) #s(point 10 14)))
	   #s(lsp-symbol "test_foo" "Function" #s(location #s(point 13 0) #s(point 16 0)))
	   #s(lsp-symbol "foo" "Variable" #s(location #s(point 14 4) #s(point 14 20)))
	   #s(lsp-symbol "test_bar" "Function" #s(location #s(point 18 0) #s(point 21 0)))
	   #s(lsp-symbol "bar" "Variable" #s(location #s(point 19 4) #s(point 19 20)))))
	 
	 (cursor #s(point 15 4))

	 (actual (dap-python--lsp-symbols-before-point cursor document-symbols))

	 (expected
	  (list
	   #s(lsp-symbol "dataclass" "Function" #s(location #s(point 0 0) #s(point 0 33)))
	   #s(lsp-symbol "Foo" "Class" #s(location #s(point 4 0) #s(point 6 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 5 4) #s(point 5 14)))
	   #s(lsp-symbol "Bar" "Class" #s(location #s(point 9 0) #s(point 11 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 10 4) #s(point 10 14)))
	   #s(lsp-symbol "test_foo" "Function" #s(location #s(point 13 0) #s(point 16 0)))
	   #s(lsp-symbol "foo" "Variable" #s(location #s(point 14 4) #s(point 14 20))))))

    (should (== actual expected))
    (should (== (dap-python--nearest-test expected) "::test_foo"))))

(ert-deftest dap--python-test-class-method ()
  (let* ((document-symbols
	  (list
	   #s(lsp-symbol "dataclass" "Function" #s(location #s(point 0 0) #s(point 0 33)))
	   #s(lsp-symbol "Foo" "Class" #s(location #s(point 4 0) #s(point 6 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 5 4) #s(point 5 14)))
	   #s(lsp-symbol "Bar" "Class" #s(location #s(point 9 0) #s(point 11 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 10 4) #s(point 10 14)))
	   #s(lsp-symbol "TestClass" "Class" #s(location #s(point 13 0) #s(point 21 0)))
	   #s(lsp-symbol "test_foo" "Function" #s(location #s(point 14 4) #s(point 17 0)))
	   #s(lsp-symbol "foo" "Variable" #s(location #s(point 15 8) #s(point 15 24)))
	   #s(lsp-symbol "test_bar" "Function" #s(location #s(point 18 4) #s(point 21 0)))
	   #s(lsp-symbol "bar" "Variable" #s(location #s(point 19 8) #s(point 19 24)))))
	 
	 (cursor #s(point 15 4))

	 (actual (dap-python--lsp-symbols-before-point cursor document-symbols))

	 (expected
	  (list 
	   #s(lsp-symbol "dataclass" "Function" #s(location #s(point 0 0) #s(point 0 33)))
	   #s(lsp-symbol "Foo" "Class" #s(location #s(point 4 0) #s(point 6 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 5 4) #s(point 5 14)))
	   #s(lsp-symbol "Bar" "Class" #s(location #s(point 9 0) #s(point 11 0)))
	   #s(lsp-symbol "value" "Variable" #s(location #s(point 10 4) #s(point 10 14)))
	   #s(lsp-symbol "TestClass" "Class" #s(location #s(point 13 0) #s(point 21 0)))
	   #s(lsp-symbol "test_foo" "Function" #s(location #s(point 14 4) #s(point 17 0))))))

    (should (== actual expected))
    (should (== (dap-python--nearest-test expected) "::TestClass::test_foo"))))


(provide 'dap-test)
;;; dap-test.el ends here
