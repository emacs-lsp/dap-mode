Features
========

- Launch/Attach
- Breakpoints
- Exceptions
- Pause & Continue
- Step In/Out/Over
- Callstacks
- Threads
- Multiple simultaneous debug sessions
- Evaluating statements
- Debug/Run configurations (also with launch.json files)
- Expressions

## Debugger commands

| Command                        | Description                                                     |
| ------------------------------ | --------------------------------------------------------------- |
| `dap-breakpoint-toggle`        | Toggle breakpoint at line                                       |
| `dap-breakpoint-delete`        | Delete breakpoint at line                                       |
| `dap-breakpoint-add`           | Add breakpoint at line                                          |
| `dap-breakpoint-condition`     | Set/unset breakpoint condition                                  |
| `dap-breakpoint-hit-condition` | Set/unset breakpoint hit condition                              |
| `dap-breakpoint-log-message`   | Set/unset breakpoint log message                                |
| `dap-eval`                     | Eval string                                                     |
| `dap-eval-region`              | Eval region string                                              |
| `dap-eval-thing-at-point`      | Eval symbol at point                                            |
| `dap-step-in`                  | Debug step in                                                   |
| `dap-next`                     | Debug next                                                      |
| `dap-step-out`                 | Debug step out                                                  |
| `dap-stop-thread`              | Stop thread                                                     |
| `dap-restart-frame`            | Restart frame                                                   |
| `dap-continue`                 | Debug continue                                                  |
| `dap-disconnect`               | Cancel current debug session                                    |
| `dap-switch-stack-frame`       | Switch active stack frame                                       |
| `dap-switch-thread`            | Switch active thread                                            |
| `dap-switch-session`           | Switch active session                                           |
| `dap-debug-edit-template`      | Generate run command                                            |
| `dap-debug`                    | Create and run new configuration using the available templates  |
| `dap-debug-last`               | Debug previous configuration                                    |
| `dap-debug-recent`             | Select configuration to run from the previously started command |
| `dap-go-to-output-buffer`      | Go output buffer                                                |

## Windows

| Command              | Description                          |
| -------------------- | ------------------------------------ |
| `dap-ui-sessions`    | Show active/terminated sessions view |
| `dap-ui-locals`      | Show locals view                     |
| `dap-ui-expressions` | Show expressions view                |
| `dap-ui-breakpoints` | Show breakpoints view                |
| `dap-ui-repl`        | DAP UI REPL                          |

## Sessions

The session view is shown after invoking `dap-ui-sessions` . It
represents the list of the active sessions.

## Locals

Locals can be viewed after invoking `dap-ui-locals`.

## Expressions

Watch expressions can be viewed after invoking `dap-ui-expressions`. You
could add remove watch expressions via `dap-ui-expressions-add` and
`dap-ui-expressions-remove`.

## Breakpoints

Breakpoints can be viewed after invoking `dap-ui-breakpoints`.

1.  Keybindings

    | Command                              | Description                    | Keybindings |
    | ------------------------------------ | ------------------------------ | ----------- |
    | `dap-ui-breakpoints-goto`            | Go to breakpoint under cursor  | \<return\>  |
    | `dap-ui-breakpoints-delete`          | Delete breakpoint under cursor | d           |
    | `dap-ui-breakpoints-delete-selected` | Delete selected breakpoints    | D           |
    | `bui-list-mark`                      | Mark breakpoint under point    | m           |
    | `bui-list-unmark`                    | Unmark breakpoint under point  | u           |
    | `bui-list-unmark-all`                | Unmark breakpoint under point  | U           |


## Loaded sources

Loaded sources can be viewed by requiring `dapui` and invoking `dapui-loaded-sources`.

## DAP debug REPL

DAP provides a debug shell to execute commands when the program has hit
breakpoints. The REPL has the same features as regular emacs shells
(e.g. command history, `C-p/n` navigation through history, etc.) in
addition to optional `company-mode` autocompletion.
![](screenshots/dap-ui-repl.png)

## launch.json support

DAP supports `launch.json` files out of the box, and there is nothing that needs
to be enabled. All that needs to be done is to add a `launch.json` file at the
project root or the `.vscode` directory within the project root
and to run `dap-debug`. All configurations stored in the
`launch.json` will automatically show up in the selection. `launch.json` files in
DAP are just like in VSCode and even support variables. See:

- [launch.json](https://code.visualstudio.com/docs/editor/debugging)
- [launch.json variables](https://code.visualstudio.com/docs/editor/variables-reference)

## Compiling the project before debugging

Many modern IDEs, for example Eclipse and VSCode (`preLaunchTask`), provide
functionality to compile the project before starting the debug session.

`dap-mode` also has such a feature: the `:dap-compilation` property of launch
configurations ("dap-compilation" in `launch.json`) specifies a shell command
that needs to execute successfully before the debug session is started.
`:dap-compilation-dir` can be used to control where the compilation is started.

In the future, we want to support VSCode's `preLaunchTask` instead, but
currently there is no tasks.json-compatible task runner for Emacs.
