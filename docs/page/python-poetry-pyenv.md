Debugging Python when using Poetry and Pyenv
==============================================

This guide is based on [this guide on reddit](https://www.reddit.com/r/emacs/comments/k5dsar/emacs_ide_for_python_setting_up_the_debugger_with/)

One seemingly sane stack for Python development is [Poetry](https://github.com/python-poetry/poetry) + [Pyenv](https://github.com/pyenv/pyenv).
To debug a project using poetry, start by running 
``` bash
poetry config virtualenvs.in-project true
poetry add --group dev debugpy
poetry install
```
Then, in emacs-land, install [with-venv](https://github.com/10sr/with-venv-el/tree/4a59ef8251f10ea772d4f504beeab08edf1f223e)
and add the following snippet wherever you tend to add snippets:
``` emacs-lisp
  (use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))
```

You should now be able to debug your projects by calling `dap-hydra` as normal.
