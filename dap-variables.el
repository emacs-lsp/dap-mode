;;; dap-variables.el --- VSCode variables -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; Keywords: emulations
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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
;; Implements a library to expand VSCode-style variables in JSON trees. VSCode
;; makes use of variable expansion in many features: the task runner, the
;; debugger, .... `dap-mode' needed a way to handle such variables, and so
;; `dap-variables' was created; it is now available as a library independent of
;; dap.
;;
;; The main entry point is `dap-variables-expand', which handles most of
;; VSCode's standard variables, and a few `dap-variables'-specific extensions,
;; and is even customizable by the user. Additionally, it automatically handles
;; platform-properties like "macos", "linux", .... If you don't want the latter
;; feature, use `dap-variables-standard-expand-tree'.
;;
;; Additionally, `dap-variables-find-vscode-config' can be used to locate
;; VSCode-style per-project configuration files, which are usually JSON5 files
;; that contain variables.

;;; Code:

(require 'cl-lib)

;;; core expansion logic
(defun dap-variables--eval-poly-type (value args)
  "Get the value from VALUE depending on its type.
VALUE is evaluated in two stages:

1. If it is a function, call it using `funcall', applying ARGS (a
list of arguments) to it. If VALUE is a symbol and that symbol
does not have an associated function, yield that symbol's value.

2. If the result of the first stage is a number, make it a
string, yielding stage 1's result otherwise."
  (let ((pre-res
         (pcase value
           ((pred functionp) (apply value args))
           ((pred symbolp) (symbol-value value))
           (_ value))))
    ;; stage two expansion: VALUE as a function could have returned something,
    ;; or a number was stored in the symbol.
    (pcase pre-res
      ((pred numberp) (number-to-string pre-res))
      ;; by using a pcase here, this function becomes more extensible.
      (_ pre-res))))

(defun dap-variables--expand-escapes (s)
  "Expand all characters escaped with backslashes in string S.
Return the result, also a string."
  (replace-regexp-in-string "\\\\\\(.\\)" (lambda (match) (match-string 1 match))
                            s nil t))

(defun dap-variables--expand-in-string (var-cb s)
  "Expand all variables of the form ${VARIABLE} in string S.
Return the result. To expand each variable, VAR-CB is
`funcall'ed, with VARIABLE as argument. If it returns nil, no
expansion is performed for that part of S."
  (replace-regexp-in-string
   (rx (or (: "\\" (group "$"))
           (: "${" (group (* (or (not (any "\\}")) (: "\\" anything)))) "}")))
   (lambda (match)
     (let ((escaped-var (match-string 1 match))
           (var (match-string 2 match)))
       (or escaped-var
           (and var (funcall var-cb (dap-variables--expand-escapes var)))
           match))) s nil t))

(defun dap-variables--find-matching (variable-alist var)
  "Return the corresponding VALUE to the REGEX matching VAR.
Return nil if no matching VALUE is found. VARIABLE-ALIST is a
list of the form (REGEX . VARIABLE). This function modifies the
`match-data': REGEX may contain captures, which may be accessed
with regular (`match-string' <n> VAR) or `match-data'."
  (cdr (cl-find-if (lambda (x) (string-match (car x) var)) variable-alist)))

(defun dap-variables--all-matches (&optional s)
  "Return all strings matched by the last search.
The result is a list of strings, each of which is as if from
`match-string'. If S is not nil, acquire the match texts from S
and not from the current buffer."
  (cl-loop for i from 1 for submatch = (match-string i s)
           while submatch collect submatch))

(defun dap-variables--walk-tree (var-cb tree)
  "Recursively walk TREE and call VAR-CB on all variables.
The return value of a VAR-CB invocation is ignored.

Special case: if TREE (or one of its subtrees) is cons whose
`cdr' does not satisfy `listp' (e.g. '(\"foo\" . \"bar\")), its
`car' is left as is and its `cdr' is expanded recursively. Note
that a dotted pair whose `cdr' is a list cannot be told from a
list. So, '(4 5) and '(4 . (5)) are `equal'. As such, you should
use plists when parsing from JSON.

TREE can be a ...:
- cons whose `cdr' does not satisfy `listp'
- list (nil counts)
- vector
- keyword (:foo)
- string
- a hashmap
Everything else is ignored."
  ;; dotted pair that is not a list (e.g.: ("JUNIT_CLASS_PATH" . "foo"))
  (cond ((functionp tree) tree)
        ((and (consp tree) (not (listp (cdr tree))))
         (dap-variables--walk-tree var-cb (cdr tree)))
        ((or (listp tree) (vectorp tree))
         (mapc (apply-partially #'dap-variables--walk-tree var-cb) tree))

        ((stringp tree) (dap-variables--expand-in-string var-cb tree))

        ((hash-table-p tree)
         (maphash (lambda (a b)
                    (dap-variables--walk-tree var-cb a)
                    (dap-variables--walk-tree var-cb b))
                  tree))
        ;; base case: ignore TREE
        ))

(defun dap-variables--map-tree (var-cb tree)
  "Recursively walk TREE, replacing all variables using VAR-CB.
When a variable in a string is encountered, VAR-CB is called on
it and the result used in its place. TREE is copied in the
process.

This function is similar to `dap-variables--walk-tree'. In
particular, the walking rules and constraints on TREE are the
same."
  ;; dotted pair that is not a list (e.g.: ("JUNIT_CLASS_PATH" . "foo"))
  (cond ((functionp tree) tree)         ; edge case: don't touch functions
        ((and (consp tree) (not (listp (cdr tree))))
         (cons (car tree) (dap-variables--map-tree var-cb (cdr tree))))

        ((or (listp tree) (vectorp tree))
         (cl-map
          (type-of tree) (apply-partially #'dap-variables--map-tree var-cb)
          tree))

        ((stringp tree) (dap-variables--expand-in-string var-cb tree))

        ;; base case: just yield tree. Note: also handles keyword arguments.
        (t tree)))

(defun dap-variables--call-alist-variable (variable-alist var)
  "Look up VAR in VARIABLE-ALIST and call the result.
The looked up value is called for side-effects only; this
function always returns nil."
  (when-let ((cb (dap-variables--find-matching variable-alist var)))
    (ignore (apply cb (dap-variables--all-matches var)))))

(defvar dap-variables-current-variables '()
  "The current list of expansion variables.
See `dap-variables-standard-variables' for details. You can push
to this to add new variables for expansion.")

(defun dap-variables--expand-alist-variable (variable-alist var)
  "Expand VAR by looking it up in VARIABLE-ALIST."
  ;; `dap-variables--find-matching' modifies the match data
  (save-match-data
    ;; VSCode ignores unknown variables (leaves them as they are, unexpanded),
    ;; so return nil (leave variable unchanged)
    (when-let ((value (dap-variables--find-matching variable-alist var)))
      (or (dap-variables--eval-poly-type
           value (dap-variables--all-matches var))
          (progn (lwarn '(dap-variables-expand-variable) :warning
                        "variable ${%s} is nil here" var) "")))))

(defun dap-variables-expand-variable (var)
  "Expand VAR according to `dap-variables-current-variables'.
This function may be used in nested variables."
  (dap-variables--expand-alist-variable dap-variables-current-variables var))

(defun dap-variables-expand-tree
    (tree vars &optional pre-expand-vars post-walk-hook)
  "Non-destructively expand all variables in all strings of TREE.
TREE is a recursive data-structure, whose constraints are
described in `dap-variables--map-tree'.

VARS has the form (REGEX . VALUE). During expansion, variables of
the form ${var} get replaced by finding a matching REGEX and
processing the value as in `dap-variables--eval-poly-type'.

TREE is expanded in two stages: first, the tree is walked, with
the set of variables being PRE-EXPAND-VARS. Their results are
ignored. Then, all functions in POST-WALK-HOOK are called, with
the original variable string encountered as arguments. They may
introduce new variables by prepending (`cl-pushnew', `push') to
`dap-variables-current-variables'."
  ;; do a first-pass walk to gather variables
  (when pre-expand-vars
    (dap-variables--walk-tree
     (apply-partially #'dap-variables--call-alist-variable pre-expand-vars)
     tree))

  (let ((dap-variables-current-variables vars))
    (mapc #'funcall post-walk-hook)
    (dap-variables--map-tree #'dap-variables-expand-variable tree)))

;;; project variables
(defun dap-variables--project-current-root ()
  "Find the current buffer's project root using `project-current'."
  (directory-file-name (cdr (project-current t))))

(defcustom dap-variables-project-root-function
  #'dap-variables--project-current-root
  "Function to acquire the project root of the current buffer.
The function will be called with no arguments and shall return
the path to the project of the current buffer, without a trailing
slash.

Used in project variables like ${workspaceFolder}. When using
`dap-variables' as a library, this can be overridden using
`let'."
  :group 'dap-variables
  :type '(function))

(defun dap-variables-project-root ()
  "Find the current buffer's project root.
Respects and uses `dap-variables-project-root-function'."
  (funcall dap-variables-project-root-function))

(defun dap-variables-project-basename ()
  "Return the basename of the project root directory."
  (file-name-nondirectory (directory-file-name (dap-variables-project-root))))

(defun dap-variables-project-relative-file (&optional file)
  "Return the path to FILE relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'."
  (file-relative-name (or file buffer-file-name) (dap-variables-project-root)))

;;; basic variables
(defun dap-variables-project-relative-dirname (&optional file)
  "Return the path to the directory of file relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'"
  (dap-variables-project-relative-file
   (file-name-directory (or file buffer-file-name))))

(defun dap-variables-buffer-basename ()
  "Return the name of the current buffer's file without its directory."
  (file-name-nondirectory buffer-file-name))

(defun dap-variables-buffer-basename-sans-extension ()
  "Same as `dap-variables-buffer-basename', but without the extension."
  (file-name-sans-extension (dap-variables-buffer-basename)))

(defun dap-variables-buffer-extension ()
  "Return the extension of the buffer's file with a leading dot.
If there is either no file associated with the current buffer or
if that file has no extension, return the empty string."
  (and (buffer-file-name) (file-name-extension (buffer-file-name) t)))

(defun dap-variables-buffer-dirname ()
  "Return the directory the buffer's file is in."
  (file-name-directory (buffer-file-name)))

(defun dap-variables-buffer-selected-text ()
  "Return the text selected in the current buffer.
If no text is selected, return the empty string."
  ;; Cannot fail, as if there is no mark, (mark) and (point) will be equal, and
  ;; (`buffer-substring-no-properties') will yield "", as it should.
  (buffer-substring-no-properties (mark) (point)))

(defun dap-variables-getenv-or-empty (variable)
  "`getenv' VARIABLE; yield the empty string if it is undefined."
  (or (getenv variable) ""))

;;; ${command:pickProcess}
(defface dap-variables-pid-face '((t :inherit font-lock-function-name-face))
  "Face process ids are shown in for ${command:pickProcess}.
Each entry in that function consists of the process args followed
by the PID, in parentheses (). Only the PID (not the parentheses)
is shown with that face."
  :group 'dap-faces)

(defun dap-variables-pick-process ()
  "Prompt the user to select a system process and return it.
The return value is the process' PID, as an integer. If the user
fails to select an entry, return nil."
  (let* ((pids (list-system-processes))
         (pids-propertized (cl-loop for pid in pids collect
                                    (propertize (number-to-string pid) 'face
                                                'dap-variables-pid-face)))
         (proc-attrs (mapcar #'process-attributes pids))
         (proc-names (mapcar (apply-partially #'alist-get 'args) proc-attrs))
         (entries (cl-mapcar (apply-partially #'format "%s (%s)")
                             proc-names pids-propertized))
         (message-pid-alist (cl-mapcar #'cons entries pids))

         ;; History doesn't make sense here since PIDs are usually very unstable
         ;; (sometimes randomized for security and because processes spawn and
         ;; die).
         ;;
         ;; The list does not live-update itself, which is why we don't
         ;; require-match here: the user may find out the PID after running
         ;; `dap-debug'.
         (chosen-entry (completing-read "Select a PID: " message-pid-alist)))
    (alist-get chosen-entry message-pid-alist chosen-entry nil #'string=)))

;;; numbered prompts
(defun dap-variables--count-unique-numbered-prompts (prompts)
  "Count the unique prompt numbers in PROMPTS.
PROMPTS must have the form of `dap-variables-numbered-prompts'
and it must be sorted by NUMBER."
  (let ((prev-n nil))
    (cl-count-if-not
     (lambda (prompt)
       (let ((promptn (car prompt)))
         (prog1 (eq prev-n promptn) (setq prev-n promptn))))
     prompts)))

(defvar dap-variables-numbered-prompts '()
  "Mapping between numbered variables ${1} and their prompts.
List of lists (NUMBER QUESTION VAR) where NUMBER is the number of
the question (questions are asked in correct order), QUESTION is
a prompt to be displayed to the user and VAR is the variable
corresponding to the prompt. So if there is a variable
${1:hostname}, NUMBER would be 1, QUESTION would be hostname and
VAR would be 1:hostname.")

(defun dap-variables--colon-prompt-var (var id question)
  "Implement a variable of the form ${num:question}.
Register a question variable in `dap-variables-numbered-prompts'.
VAR is the variable encountered, ID is the number associated with
it and QUESTION is the string used to ask the user for a value."
  (push (list (string-to-number id) question var)
        dap-variables-numbered-prompts))

(defvar dap-variables-prompt-histories (make-hash-table :test #'equal)
  "History of the user's answers to variable prompts (${1:host?}).")
;;; `savehist' integration
(when (boundp 'savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'dap-variables-prompt-histories))

(defvar dap-variables--temp-hist nil
  "Temporarily the history list during expansion of a prompting variable.
To overcome the limitation of read-string's history having to
always be a symbol, this variable is temporarily `let'-bound to
the correct history list, as looked up in
`dap-variables-prompt-histories'.")

(defun dap-variables--do-prompts ()
  "Ask the questions in `dap-variables-numbered-prompts'.
Prepend the answers in the form of additional variables to
`dap-variables-current-variables'."
  (cl-loop with numbered-prompts =
           (sort dap-variables-numbered-prompts (lambda (a b) (< (car a) (car b))))
           with nunique-prompts =
           (dap-variables--count-unique-numbered-prompts numbered-prompts)

           with prev-answer = nil
           with prev-id = nil
           for (id prompt var) in numbered-prompts
           for current-promptn from 1
           if (eq id prev-id) do
           (lwarn '(dap-variables-prompt) :error
                  "multiple prompts for %d (in ${%s})" id var)
           else do
           (setq prev-id id)
           (let ((dap-variables--temp-hist
                  (gethash prompt dap-variables-prompt-histories)))
             (setq
              prev-answer
              (read-string
               (format "(%d/%d) %s: " current-promptn nunique-prompts prompt)
               nil 'dap-variables--temp-hist))
             (puthash prompt dap-variables--temp-hist
                      dap-variables-prompt-histories))
           ;; cannot be a duplicate, because duplicate numbers are weeded out in
           ;; the if above; as such, `cl-pushnew' is unnecessary.
           and do (push (cons (format "\\`%d\\'" id) prev-answer)
                        dap-variables-current-variables)
           ;; no new answer, because it's a duplicate; substitute with the old
           ;; answer
           do (cl-pushnew
               (cons (format "\\`%s\\'" (regexp-quote var)) prev-answer)
               dap-variables-current-variables)))

(defun dap-variables--do-prompts-reset (&optional _)
  "Ask and reset the questions in `dap-variables-numbered-prompts'.
For details, see `dap-variables--do-prompts'."
  (unwind-protect (dap-variables--do-prompts)
    (setq dap-variables-numbered-prompts nil)))

;;; standard API
(defcustom dap-variables-standard-variables
  ;; list taken from https://code.visualstudio.com/docs/editor/variables-reference
  '(("\\`workspaceFolderBasename\\'" . dap-variables-project-basename)
    ("\\`workspaceFolder\\'" . dap-variables-project-root)
    ("\\`relativeFileDirname\\'" . dap-variables-project-relative-dirname)
    ("\\`relativeFile\\'" . dap-variables-project-relative-file)
    ("\\`fileBasenameNoExtension\\'" . dap-variables-buffer-basename-sans-extension)
    ("\\`fileBasename\\'" . dap-variables-buffer-basename)
    ("\\`fileDirname\\'" . dap-variables-buffer-dirname)
    ("\\`fileExtname\\'" . dap-variables-buffer-extension)
    ("\\`lineNumber\\'" . line-number-at-pos)
    ("\\`selectedText\\'" . dap-variables-buffer-selected-text)
    ("\\`file\\'" . buffer-file-name)
    ;; behave like VSCode: undefined environment variables expand to the empty
    ;; string, without warning
    ("\\`env:\\(.*\\)\\'" . dap-variables-getenv-or-empty)
    ;; technically not in VSCode, but I still wanted to add a way to escape $
    ("\\`\\$\\'" . "$")
    ;; the following variables are valid in VSCode, but have no meaning in
    ;; Emacs, and are as such unsupported.
    ;; ("cwd") ;; the task runner's current working directory,
    ;;         ;; not `default-directory'
    ;; ("execPath")
    ;; ("defaultBuildTask")
    ("\\`command:pickProcess\\'" . dap-variables-pick-process))
  "Alist of (REGEX . VALUE) pairs listing variables usable in launch.json files.
This list is iterated from the top to bottom when expanding
variables in the strings of the selected launch configuration
`dap-variables-expand-variable'.

When a REGEX matches (`string-match'), its corresponding VALUE is
evaluated as follows: if it is a function (or a quoted lambda),
that function is called with `funcall', and its result, which
must be a string, is used in place of the variable. The function
gets the strings of all capture groups as arguments. Otherwise,
if it is a symbol, the symbol's value is used the same way.
Lastly, if it is a string, the string is used as a replacement.
If no regex matches, the empty string is used as a replacement
and a warning is issued.

Do not push to this variable directly to add new ones; instead,
use `dap-variables-current-variables'."
  :type '(alist :key-type string :value-type (choice integer string function))
  :group 'dap-variables)

(defvar dap-variables-post-walk-hook
  '(dap-variables--do-prompts-reset)
  "Functions to be run after first walking the launch configuration.
When expanding a launch configuration, first
`dap-variables-pre-expand-hook' is called. Then, the launch
configuration is walked, visiting, but not expanding, all
variables in `dap-variables-pre-expand-variables'. Then all
functions in this list are called, with the launch configuration
as their only argument. They shall return a list of additional
variables of the form (REGEX . VALUE) (see
`dap-variables-standard-variables'). All lists are concatenated
and added to the list of variables used for expansion.")

(defvar dap-variables-pre-expand-variables
  '(("\\`\\(\\([[:digit:]]+\\):\\(.*\\)\\)'" . dap-variables--colon-prompt-var))
  "Alist of (REGEX . FUNCTION) pairs listing pre-expansion variables.
Before any expansion occurs, all variables matching REGEX have
their corresponding FUNCTION called, with the variable as
argument. Its result is ignored, but FUNCTION could initialize
something for use in `dap-variables-post-walk-hook'.")

(defvar dap-variables-pre-expand-hook '()
  "List of functions to be run before a launch configuration is expanded.
They take one argument: the run configuration.")

(defvar dap-variables-post-expand-hook '()
  "List of functions to be run after a launch configuration has been expanded.
They take one argument: the run configuration, this time with all
variables expanded.")

(defun dap-variables-standard-expand-tree (tree)
  "Expand TREE using the user's set of variables.
See `dap-variables-expand-tree' for details.

Note that this function does not handle os-specific
properties (e.g. \"linux\": ..., \"macosx\": ...) specially.

Respects `dap-variables-standard-variables',
`dap-variables-pre-expand-hook',
`dap-variables-post-expand-hook', `dap-variables-post-walk-hook',
`dap-variables-pre-expand-hook' and
`dap-variables-post-expand-hook'."
  (run-hook-with-args 'dap-variables-pre-expand-hook tree)
  (let ((newconf (dap-variables-expand-tree
                  tree dap-variables-standard-variables
                  dap-variables-pre-expand-variables
                  dap-variables-post-walk-hook)))
    (prog1 newconf
      (run-hook-with-args 'dap-variables-post-expand-hook newconf))))

(defun dap-variables-find-vscode-config (f root)
  "Find a project-specific VSCode configuration file.
ROOT specifies the root of the project to search for F; search
for F in either ROOT/ or ROOT/.vscode/."
  (let* ((root (file-name-as-directory root))
         (root-f (concat root f))
         (root-vscode (concat root (file-name-as-directory ".vscode") f)))
    (cl-find-if #'file-exists-p (list root-f root-vscode))))

(defconst dap-variables-os-property-alist
  '((windows-nt . :windows)
    (gnu/linux . :linux)
    (darwin . :macos))
  "Alist mapping system types to os-specific properties.
See `dap-variables-elevate-os-properties'.

Alist (SYSTEM-TYPE . PROP) which maps a given `system-type' to a
property that should hold a plist of attributes that should be
set for that platform only in the plist above.")

(defun dap-variables-elevate-os-properties (plist)
  "Replace properties with their platform-specific counterparts.
VSCode often allows specifying some properties for some platforms
only, so there could be a launch.json \"linux\" section listing
some properties to set when debugging from Linux, ....

Transform PLIST so that those sections are added (removing
previous values) at the top-level."
  (let* ((new-list (cl-copy-list plist))
         (platform-prop
          (cdr (assoc system-type dap-variables-os-property-alist)))
         (override (plist-get new-list platform-prop)))
    (dolist (prop dap-variables-os-property-alist)
      (cl-remf new-list (cdr prop)))
    (cl-loop for (k _) on override by #'cddr do (cl-remf new-list k))
    (append override new-list)))

(defun dap-variables-expand (plist)
  "Handle PLIST as if it were a VSCode *.json item.
`dap-variables-elevate-os-properties' +
`dap-variables-standard-expand-tree'."
  (dap-variables-standard-expand-tree
   (dap-variables-elevate-os-properties plist)))

;;; LocalWords: Nikita
;;; LocalWords: Bloshchanevich
;;; LocalWords: customizable
;;; LocalWords: JSON5
;;; LocalWords: JSON
;;; LocalWords: dap
;;; LocalWords: macosx
;;; LocalWords: VSCode
;;; LocalWords: VSCode's

(provide 'dap-variables)
;;; dap-variables.el ends here
