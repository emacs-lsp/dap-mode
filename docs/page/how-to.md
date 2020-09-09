How to
======

## Activate minor modes when stepping through code

You may want to activate minor modes, e.g. `read-only-mode`, when the debugger is active in a buffer. If so, you could use a trick like this:

```elisp
;; -*- lexical-binding: t -*-
(define-minor-mode +dap-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil
  nil
  (make-sparse-keymap)
  (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
  ;; The following code adds to the dap-terminated-hook
  ;; so that this minor mode will be deactivated when the debugger finishes
  (when +dap-running-session-mode
    (let ((session-at-creation (dap--cur-active-session-or-die)))
      (add-hook 'dap-terminated-hook
                (lambda (session)
                  (when (eq session session-at-creation)
                    (+dap-running-session-mode -1)))))))

;; Activate this minor mode when dap is initialized
(add-hook 'dap-session-created-hook '+dap-running-session-mode)

;; Activate this minor mode when hitting a breakpoint in another file
(add-hook 'dap-stopped-hook '+dap-running-session-mode)

;; Activate this minor mode when stepping into code in another file
(add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                          (when (dap--session-running session)
                                            (+dap-running-session-mode 1))))
```

## Stop and delete sessions

To kill a running debug session (and its inferior process), use the command `M-x
dap-disconnect`. This will still show it as a dead (grayed out) debug session in
`dap-ui-sessions`, and you will still be able to browse its output. If you want
to kill the debug session and remove it from the session list, use `M-x
dap-delete-session` instead. The latter command removes the last debug session
(dead or alive) from the session list: if you run `dap-disconnect`, the last
session will be a dead one, which you would remove by running `M-x
dap-delete-session`. Sessions can be selectively deleted by putting the cursor
on one of the sessions in `dap-ui-sessions` and pressing `D`.
