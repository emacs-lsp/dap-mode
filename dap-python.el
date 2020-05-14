;;; dap-python.el --- Debug Adapter Protocol mode for Python      -*- lexical-binding: t; -*-

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
;; Adapter for ptvsd (https://github.com/Microsoft/ptvsd)

;;; Code:

(require 'dap-mode)

(defcustom dap-python-default-debug-port 32000
  "The debug port which will be used for ptvsd process.
If the port is taken, DAP will try the next port."
  :group 'dap-python
  :type 'number)

(defcustom dap-python-executable "python"
  "The python executable to use."
  :group 'dap-python
  :risky t
  :type 'file)

(defcustom dap-python-terminal nil
  "The terminal to use when running the debug process.
For example you may set it to `xterm -e' which will pop xterm console when you are debugging."
  :group 'dap-python
  :risky t
  :type 'string)

(defun dap-python--pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(cl-defstruct dap-python--point
  (line (:type integer) :named t)
  (character (:type integer) :named t))

(cl-defstruct dap-python--location
  (start (:type dap-python--point) :named t)
  (end (:type dap-python--point) :named t))

(cl-defstruct dap-python--symbol
  (name (:type string) :named t)
  (type (:type string) :named t)
  (location (:type dap-python--location) :named t))

(cl-defgeneric dap-python--equal (lhs rhs)
  (:documentation "Check if lhs and rhs are equal"))

(cl-defmethod dap-python--equal ((lhs symbol) (rhs symbol))
  (eq lhs rhs))

(cl-defmethod dap-python--equal ((lhs integer) (rhs integer))
  (eq lhs rhs))

(cl-defmethod dap-python--equal ((lhs string) (rhs string))
  (string-equal lhs rhs))

(cl-defmethod dap-python--equal ((lhs list) (rhs list))
  (and (dap-python--equal (length lhs) (length rhs))
       (-reduce (lambda (x y) (and x y)) (-zip-with 'dap-python--equal lhs rhs))))

(cl-defmethod dap-python--equal ((lhs dap-python--point) (rhs dap-python--point))
  (and (dap-python--equal (dap-python--point-line lhs) (dap-python--point-line rhs))
       (dap-python--equal (dap-python--point-character lhs) (dap-python--point-character rhs))))

(cl-defmethod dap-python--equal ((lhs dap-python--location) (rhs dap-python--location))
  (and (dap-python--equal (dap-python--location-start lhs) (dap-python--location-start rhs))
       (dap-python--equal (dap-python--location-end lhs) (dap-python--location-end rhs))))

(cl-defmethod dap-python--equal ((lhs dap-python--symbol) (rhs dap-python--symbol))
  (and (dap-python--equal (dap-python--symbol-name lhs) (dap-python--symbol-name rhs))
       (dap-python--equal (dap-python--symbol-type lhs) (dap-python--symbol-type rhs))
       (dap-python--equal (dap-python--symbol-location lhs) (dap-python--symbol-location rhs))))

(defun dap-python--parse-lsp-symbol (symbol)
  (-let* (((&hash "name" "kind" "location") symbol)
	  ((&hash "range") location)
	  ((&hash "start" "end") range))
    (make-dap-python--symbol
     :name name
     :type (alist-get kind lsp--symbol-kind)
     :location (make-dap-python--location
		:start (make-dap-python--point :line (gethash "line" start)
					       :character (gethash "character" start))
		:end (make-dap-python--point :line (gethash "line" end)
					     :character (gethash "character" end))))))

(defun dap-python--symbol-before-point (point lsp-symbol)
  (-> lsp-symbol
      dap-python--symbol-location
      dap-python--location-start
      dap-python--point-line
      (< (dap-python--point-line point))))

(defun dap-python--symbols-before-point (point lsp-symbols)
  (-filter (-partial 'dap-python--symbol-before-point point) lsp-symbols))

(defun dap-python--test-p (lsp-symbol)
  (let ((name (dap-python--symbol-name lsp-symbol)))
    (and (dap-python--equal (dap-python--symbol-type lsp-symbol) "Function")
	 (s-starts-with? "test_" name))))

(defun dap-python--test-class-p (test-symbol lsp-symbol)
  (when (dap-python--equal (dap-python--symbol-type lsp-symbol) "Class")
      (let* ((class-location (dap-python--symbol-location lsp-symbol))
	     (class-start-line (-> class-location dap-python--location-start dap-python--point-line))
	     (class-end-line (-> class-location dap-python--location-end dap-python--point-line))
	     (test-start-line (-> test-symbol dap-python--symbol-location dap-python--location-start dap-python--point-line)))
	(and (> test-start-line class-start-line)
	     (< test-start-line class-end-line)))))

(defun dap-python--nearest-test (lsp-symbols)
  (let* ((reversed (reverse lsp-symbols))
	 (test-symbol (-first 'dap-python--test-p reversed))
	 (class-symbol (-first (-partial 'dap-python--test-class-p test-symbol) reversed)))
    (if (eq nil class-symbol)
	(concat "::" (dap-python--symbol-name test-symbol))
        (concat "::" (dap-python--symbol-name class-symbol) "::" (dap-python--symbol-name test-symbol)))))

(defun dap-python--cursor-position ()
  (make-dap-python--point :line (line-number-at-pos)
			  :character (current-column)))

(defun dap-python--test-at-point ()
  (->> (lsp--get-document-symbols)
       (mapcar 'dap-python--parse-lsp-symbol)
       (dap-python--symbols-before-point (dap-python--cursor-position))
       dap-python--nearest-test))

(defun dap-python--template (template-name)
  (->> dap-debug-template-configurations
       (-first (-lambda ((name)) (dap-python--equal name template-name)))
       cdr))

(defun dap-python--debug-test-at-point ()
  (interactive)
  (dap-debug (dap-python--template "Python :: Run pytest (at point)")))

(defun dap-python--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let* ((host "localhost")
         (debug-port (dap--find-available-port))
         (python-executable (dap-python--pyenv-executable-find dap-python-executable))
         (python-args (or (plist-get conf :args) ""))
         (program (or (plist-get conf :target-module)
                      (plist-get conf :program)
                      (buffer-file-name)))
         (module (plist-get conf :module)))

    (plist-put conf :program-to-start
               (format "%s%s -m ptvsd --wait --host %s --port %s %s %s %s"
                       (or dap-python-terminal "")
                       (shell-quote-argument python-executable)
                       host
                       debug-port
                       (if module (concat "-m " (shell-quote-argument module)) "")
                       (shell-quote-argument program)
                       python-args))
    (plist-put conf :program program)
    (plist-put conf :debugServer debug-port)
    (plist-put conf :port debug-port)
    (plist-put conf :hostName host)
    (plist-put conf :host host)
    conf))

(defun dap-python--populate-test-at-point (conf)
  "Populate CONF with the required arguments."
  (let* ((host "localhost")
         (debug-port (dap--find-available-port))
         (python-executable (dap-python--pyenv-executable-find dap-python-executable))
         (python-args (or (plist-get conf :args) ""))
         (program (concat (buffer-file-name) (dap-python--test-at-point)))
         (module (plist-get conf :module)))

    (plist-put conf :program-to-start
               (format "%s%s -m ptvsd --wait --host %s --port %s %s %s %s"
                       (or dap-python-terminal "")
                       (shell-quote-argument python-executable)
                       host
                       debug-port
                       (if module (concat "-m " (shell-quote-argument module)) "")
                       (shell-quote-argument program)
                       python-args))
    (plist-put conf :program program)
    (plist-put conf :debugServer debug-port)
    (plist-put conf :port debug-port)
    (plist-put conf :hostName host)
    (plist-put conf :host host)
    (plist-put conf :cwd (lsp-workspace-root))
    conf))

(dap-register-debug-provider "python" 'dap-python--populate-start-file-args)
(dap-register-debug-template "Python :: Run file (buffer)"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :module nil
                                   :program nil
                                   :request "launch"
                                   :name "Python :: Run file (buffer)"))

(dap-register-debug-template "Python :: Run pytest (buffer)"
                             (list :type "python"
                                   :args ""
                                   :cwd nil
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: Run pytest (buffer)"))

(dap-register-debug-provider "python-test-at-point" 'dap-python--populate-test-at-point)
(dap-register-debug-template "Python :: Run pytest (at point)"
			     (list :type "python-test-at-point"
				   :args ""
				   :module "pytest"
				   :request "launch"
				   :name "Python :: Run pytest (at point)"))

(provide 'dap-python)
;;; dap-python.el ends here
