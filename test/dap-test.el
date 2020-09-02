;;; dap-test.el --- Unit tests for dap-mode          -*- lexical-binding: t; -*-

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
;; Implement dap-mode's `ert'-based test-suite.

;; To run it, execute the shell command 'cask exec ert-runner' at the project's
;; root, after installing the project's dependencies invoking 'cask install' the
;; same way.

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

(require 'dap-python)

(ert-deftest dap--python-test-free-function ()
  (let* ((document-symbols
    (list
     #s(dap-python--symbol "dataclass" "Function" #s(dap-python--location #s(dap-python--point 0 0) #s(dap-python--point 0 33)))
     #s(dap-python--symbol "Foo" "Class" #s(dap-python--location #s(dap-python--point 4 0) #s(dap-python--point 6 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 5 4) #s(dap-python--point 5 14)))
     #s(dap-python--symbol "Bar" "Class" #s(dap-python--location #s(dap-python--point 9 0) #s(dap-python--point 11 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 10 4) #s(dap-python--point 10 14)))
     #s(dap-python--symbol "test_foo" "Function" #s(dap-python--location #s(dap-python--point 13 0) #s(dap-python--point 16 0)))
     #s(dap-python--symbol "foo" "Variable" #s(dap-python--location #s(dap-python--point 14 4) #s(dap-python--point 14 20)))
     #s(dap-python--symbol "test_bar" "Function" #s(dap-python--location #s(dap-python--point 18 0) #s(dap-python--point 21 0)))
     #s(dap-python--symbol "bar" "Variable" #s(dap-python--location #s(dap-python--point 19 4) #s(dap-python--point 19 20)))))

   (cursor #s(dap-python--point 15 4))

   (actual (dap-python--symbols-before-point cursor document-symbols))

   (expected
    (list
     #s(dap-python--symbol "dataclass" "Function" #s(dap-python--location #s(dap-python--point 0 0) #s(dap-python--point 0 33)))
     #s(dap-python--symbol "Foo" "Class" #s(dap-python--location #s(dap-python--point 4 0) #s(dap-python--point 6 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 5 4) #s(dap-python--point 5 14)))
     #s(dap-python--symbol "Bar" "Class" #s(dap-python--location #s(dap-python--point 9 0) #s(dap-python--point 11 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 10 4) #s(dap-python--point 10 14)))
     #s(dap-python--symbol "test_foo" "Function" #s(dap-python--location #s(dap-python--point 13 0) #s(dap-python--point 16 0)))
     #s(dap-python--symbol "foo" "Variable" #s(dap-python--location #s(dap-python--point 14 4) #s(dap-python--point 14 20))))))

    (should (dap-python--equal actual expected))
    (should (dap-python--equal (dap-python--nearest-test expected) "::test_foo"))))

(ert-deftest dap--python-test-class-method ()
  (let* ((document-symbols
    (list
     #s(dap-python--symbol "dataclass" "Function" #s(dap-python--location #s(dap-python--point 0 0) #s(dap-python--point 0 33)))
     #s(dap-python--symbol "Foo" "Class" #s(dap-python--location #s(dap-python--point 4 0) #s(dap-python--point 6 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 5 4) #s(dap-python--point 5 14)))
     #s(dap-python--symbol "Bar" "Class" #s(dap-python--location #s(dap-python--point 9 0) #s(dap-python--point 11 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 10 4) #s(dap-python--point 10 14)))
     #s(dap-python--symbol "TestClass" "Class" #s(dap-python--location #s(dap-python--point 13 0) #s(dap-python--point 21 0)))
     #s(dap-python--symbol "test_foo" "Function" #s(dap-python--location #s(dap-python--point 14 4) #s(dap-python--point 17 0)))
     #s(dap-python--symbol "foo" "Variable" #s(dap-python--location #s(dap-python--point 15 8) #s(dap-python--point 15 24)))
     #s(dap-python--symbol "test_bar" "Function" #s(dap-python--location #s(dap-python--point 18 4) #s(dap-python--point 21 0)))
     #s(dap-python--symbol "bar" "Variable" #s(dap-python--location #s(dap-python--point 19 8) #s(dap-python--point 19 24)))))

   (cursor #s(dap-python--point 15 4))

   (actual (dap-python--symbols-before-point cursor document-symbols))

   (expected
    (list
     #s(dap-python--symbol "dataclass" "Function" #s(dap-python--location #s(dap-python--point 0 0) #s(dap-python--point 0 33)))
     #s(dap-python--symbol "Foo" "Class" #s(dap-python--location #s(dap-python--point 4 0) #s(dap-python--point 6 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 5 4) #s(dap-python--point 5 14)))
     #s(dap-python--symbol "Bar" "Class" #s(dap-python--location #s(dap-python--point 9 0) #s(dap-python--point 11 0)))
     #s(dap-python--symbol "value" "Variable" #s(dap-python--location #s(dap-python--point 10 4) #s(dap-python--point 10 14)))
     #s(dap-python--symbol "TestClass" "Class" #s(dap-python--location #s(dap-python--point 13 0) #s(dap-python--point 21 0)))
     #s(dap-python--symbol "test_foo" "Function" #s(dap-python--location #s(dap-python--point 14 4) #s(dap-python--point 17 0))))))

    (should (dap-python--equal actual expected))
    (should (dap-python--equal (dap-python--nearest-test expected) "::TestClass::test_foo"))))

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


(defmacro dap-variables--define-compare-test (name conf expanded
                                                   &optional docstring)
  "Make an ert test asserting that CONF, expanded, is EXPANDED.
NAME is the name used in `ert-deftest', as an unquoted symbol.
DOCSTRING, if set, specifies the docstring to use for
`ert-deftest'."
  `(ert-deftest
       ,name () ,@docstring
       (should (equal (dap-variables-expand-in-launch-configuration ,conf)
                      ,expanded))))

(dap-variables--define-compare-test
 dap-variables-test-conspair
 '("${$}" . "test${$}") '("${$}" . "test$")
 "A cons' `car' should not be expanded. Its `cdr' should.")

(dap-variables--define-compare-test
 dap-variables-test-single-el-list
 '("${$}x") '("$x"))

(dap-variables--define-compare-test
 dap-variables-test-quoted-var
 '("\\${$}x") '("${$}x")
 "Quoted variables shall not be expanded.")

(dap-variables--define-compare-test
 dap-variables-test-vec-yields-vec
 ["${$}" "\\${$}"] ["$" "${$}"])

(dap-variables--define-compare-test
 dap-variables-test-function-untouched
 (list :startup-fn #'identity)
 (list :startup-fn #'identity)
 "Functions should be passed trough, without modification.")

(dap-variables--define-compare-test
 dap-variables-test-lambda-untouched
 (list :startup-fn (lambda () "Doc." "${$}"))
 (list :startup-fn (lambda () "Doc." "${$}"))
 "Lambdas should be passed trough, without modification.")

(ert-deftest dap-variables-test-lambda-stays-lambda ()
  "A lambda should still stay a lambda, and be callable."
  (should
   (string= (funcall (nth 1 (dap-variables-expand-in-launch-configuration
                             (list :startup-fn (lambda () "Doc." "foo/bar")))))
            "foo/bar")))

(provide 'dap-test)
;;; dap-test.el ends here
