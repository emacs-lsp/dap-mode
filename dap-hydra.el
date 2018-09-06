;;; hydra-dap.el --- dap-mode integration with hydra -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "25.1") (hydra "0.14.0"))
;; Version: 0.2
;; Hydra

;;; Commentary:
;; Defines hydra for `dap-mode' operations

;;; Code:
(require 'hydra)
(require 'dap-mode)
(require 'dap-ui)

(defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
  "
^Stepping^          ^Switch^                 ^Breakpoints^           ^Eval
^^^^^^^^-----------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bt_: Toggle            _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete            _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add               _es_: Eval thing at point
_c_: Continue       _sl_: List locals        _bc_: Set condition     _eii_: Inspect
_r_: Restart frame  _sb_: List breakpoints   _bh_: Set hit count     _eir_: Inspect region
_Q_: Disconnect     _sS_: List sessions      _bl_: Set log message   _eis_: Inspect thing at point
"
  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("sl" dap-ui-locals)
  ("sb" dap-ui-breakpoints)
  ("sS" dap-ui-sessions)
  ("bt" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("ee" dap-eval)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("eii" dap-ui-inspect)
  ("eir" dap-ui-inspect-region)
  ("eis" dap-ui-inspect-thing-at-point)
  ("q" nil "quit" :color blue)
  ("Q" dap-disconnect :color red))

;;;###autoload
(defun dap-hydra ()
  "Run `dap-hydra/body'."
  (interactive)
  (dap-hydra/body))

(provide 'dap-hydra)

;;; dap-hydra.el ends here
