Configuration
===============

## DAP mode configuration

For an auto-configuration enable the `dap-auto-configure-mode`. You can configure which features from `dap-mode` do you want with `dap-auto-configure-features`:

```elisp
;; Enabling only some features
(setq dap-auto-configure-features '(sessions locals controls tooltip))
```

Or if you want to enable only specific modes instead:

```elisp
(dap-mode 1)

;; The modes above are optional

(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)
```

After enabling DAP mode on emacs side follow the language specific
settings.

## Java

1.  Installation
    
    Latest version of [LSP Java](https://github.com/emacs-lsp/lsp-java)
    will automatically discover if `dap-mode` is present and it will
    download and install the required server side components. If you
    have already downloaded a `Eclispe JDT Server` you will have to
    force a server update via <kbd>C-u</kbd> <kbd>M-x</kbd> `lsp-install-server` and select
    "jdtls". In order to enable lsp java, you will have to require
    `dap-java.el`.

    ``` elisp
    (require 'dap-java)
    ```

2.  Commands
    
    | Command                      | Description       |
    | ---------------------------- | ----------------- |
    | `dap-java-debug`             | Debug java        |
    | `dap-java-run-test-method`   | Run test method   |
    | `dap-java-debug-test-method` | Debug test method |
    | `dap-java-run-test-class`    | Run test class    |
    | `dap-java-debug-test-class`  | Debug test class  |
    

    You can also edit one of the existing templates and execute it with
    `dap-debug`. dap-mode will take care of filling missing values, such
    as classpath. JVM arguments can be specified with `:vmArgs`:
    
    ``` elisp
    (dap-register-debug-template "My Runner"
                                 (list :type "java"
                                       :request "launch"
                                       :args ""
                                       :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
                                       :projectName "myapp"
                                       :mainClass "com.domain.AppRunner"
                                       :env '(("DEV" . "1"))))
    ```

## Python

1.  Installation
    
      - install latest version of ptvsd.
        
        ``` bash
        pip install "ptvsd>=4.2"
        ```
        
          - Then add the following line in your config:
            
            ``` elisp
            (require 'dap-python)
            ```
        
        This will add the python related configuration to `dap-debug`.

2.  Usage
    
    A template named "Python :: Run Configuration" will appear, which
    will execute the currently visited module. This will fall short
    whenever you need to specify arguments, environment variables or
    execute a setuptools based script. In such case, define a template:
    
    ``` elisp
    (dap-register-debug-template "My App"
      (list :type "python"
            :args "-i"
            :cwd nil
            :env '(("DEBUG" . "1"))
            :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
            :request "launch"
            :name "My App"))
    ```

## Ruby

  - Download and extract [VSCode Ruby
    Extension](https://marketplace.visualstudio.com/items?itemName=rebornix.Ruby).
    You can do that either by:
    
      - Calling `dap-ruby-setup`, the extension will be downloaded and
        all your path will be automatically set up.
      - Or download the extension manually. Make sure that
        `dap-ruby-debug-program` is: `("node" path-to-main-js)` where
        `node` is either "node" if nodejs is on the path or path to
        nodejs and `path-to-main-js` is full path
        `./dist/debugger/main.js` which is part of the downloaded VScode
        package.

  - Follow the instructions on installing `rdebug-ide` from [Ruby Debug
    Installation](https://github.com/rubyide/vscode-ruby/wiki/1.-Debugger-Installation)

  - Put in your emacs configuration.
    
    ``` elisp
    (require 'dap-ruby)
    ```

## Dart

[LSP Dart](https://github.com/emacs-lsp/lsp-dart) has support for
[debug](https://github.com/emacs-lsp/lsp-dart#debug) using `dap-mode`.

You only need to run `dap-dart-setup` to setup automatically and then
you are good to debug dart.

1.  Flutter
    
    [LSP Dart](https://github.com/emacs-lsp/lsp-dart) also supports
    [Flutter debug](https://github.com/emacs-lsp/lsp-dart#flutter) with
    options to debug a device or emulator.

## LLDB

1.  Installation
    
    LLDB is a debugger that supports, among others, C, C++, Objective-C
    and Swift.
    
      - Clone and follow the instructions to compile lldb-vscode from
        <https://github.com/llvm/llvm-project/tree/master/lldb/tools/lldb-vscode>
    
      - Put in your emacs configuration.
        
        ``` elisp
        (require 'dap-lldb)
        ```
    
    **Note**: For proper Swift support, you need to compile LLDB from
    <https://github.com/apple/swift-lldb> and put the compiled LLDB
    library/framework in the "extensions" folder.

## Elixir

Make sure that you have properly configured `Elixir` and that you have
[Elixir LS](https://github.com/elixir-lsp/elixir-ls) binaries on the
path and put in your emacs configuration.

``` elisp
(require 'dap-elixir)
```

Then when you do `dap-debug-edit-template` and select Elixir which will
generate runnable debug configuration. For more details on supported
settings by the Elixir Debug Server refer to its documentation.

## PHP

Simplify setup of vscode extension with `dap-php-setup` after requiring
`dap-php`.

This is using
[felixbecker/vscode-php-debug](https://github.com/felixfbecker/vscode-php-debug)
([downloadable from the
marketplace](https://marketplace.visualstudio.com/items?itemName=felixfbecker.php-debug))
as dap-server between emacs and the xdebug-extension on the http-server
side. Make sure it is trans/compiled to javascript properly. Only tested
under linux with node.

``` elisp
(require 'dap-php)
```

Start debugging by selecting "PHP Run Configuration" from the
`dap-debug` menu, issue the debug request in your browser by choosing
the running thread (`dap-switch-thread`) and then `dap-step-in`.

## Native Debug (GDB/LLDB)

Using <https://github.com/WebFreak001/code-debug>

1.  Configuration
    
    For easier of setting up vscode extension, you only need call
    `dap-gdb-lldb-setup` after requiring `dap-gdb-lldb`.
    
    Or download and extract [VSCode
    extension](https://marketplace.visualstudio.com/items?itemName=webfreak.debug)
    (make sure that `dap-gdb-lldb-path` is pointing to the extract
    location).
    
    ``` elisp
    (require 'dap-gdb-lldb)
    ```
    
    Then do `dap-debug` or `dap-debug-edit-template` and selet GBD or
    LLDB configuration.

## Go

1.  Installation
    
      - For easier of setting up vscode extension, you only need call
        `dap-go-setup` after requiring `dap-go`.
        
          - Or manually download and extract [VSCode Go
            Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode.Go)..
            it is actually zip file.
          - check that you now have
            .emacs.d/.extension/vscode/golang.go/extension/out/src/debugAdapter/goDebug.js
    
      - install latest stable nodejs
    
      - install gopls
    
      - Install the delve command by following instructions on [delve -
        installation](https://github.com/go-delve/delve/tree/master/Documentation/installation).
    
      - install lsp-mode
    
      - Put in your emacs configuration.
        
        ``` elisp
        (require 'dap-go)
        ```
    
      - set up hydra hook as instructed above
    
    <!-- end list -->
    
    1.  Usage
        
        assume you have your code at \~/src/cool/cmd/app/app.go
        
          - open your main package file e.g \~/src/cool/cmd/app/app.go
          - or open a test file e.g app<sub>test</sub>.go
          - add folder to lsp session where your go.mod is or would be
              - M-x lsp-workspace-folders-add \~/src/cool
          - set break point
          - M-x dap-debug
          - if you are debugging test files use "Go Launch File
            Configuration"
          - else select e.g "Go Launch Unoptimized Debug Package
            Configuration"
    
    2.  Trouble shooting
        
          - put (setq dap-print-io t) and check messages buffer
          - e.g linter can return note at debug session response
            resulting debug session to fail

## Javascript

1.  Firefox
    
    1.  Installation
        
          - For easier of setting up vscode extension, you only need
            call `dap-firefox-setup` after requiring `dap-firefox`.
            
              - Or manually download and extract [VSCode Firefox Debug
                Extension](https://marketplace.visualstudio.com/items?itemName=hbenl.vscode-firefox-debug).
        
          - Make sure that `dap-firefox-debug-program` is pointing to
            the proper file.
        
          - Put in your configuration file:
            
            ``` elisp
            (require 'dap-firefox)
            ```
    
    2.  Usage
        
        `dap-debug` or `dap-debug-edit-template` and select the firefox
        template. For additional documentation on the supported template
        parameters or about different configuration templates refer to
        [Firefox Debug
        Adapter](https://github.com/hbenl/vscode-firefox-debug).

2.  Chrome
    
    1.  Installation
        
          - For easier of setting up vscode extension, you only need
            call `dap-chrome-setup` after requiring `dap-chrome`.
            
              - Or manually download and extract [VSCode Chrome Debug
                Extension](https://marketplace.visualstudio.com/items?itemName=msjsdiag.debugger-for-chrome).
        
          - Make sure that `dap-chrome-debug-program` is pointing to the
            proper file.
        
          - Put in your configuration file:
            
            ``` elisp
            (require 'dap-chrome)
            ```
    
    2.  Usage
        
        `dap-debug` or `dap-debug-edit-template` and select the chrome
        template. For additional documentation on the supported template
        parameters or about different configuration templates refer to
        [Chrome Debug
        Adapter](https://github.com/Microsoft/vscode-chrome-debug).

3.  Microsoft Edge
    
    1.  Installation
        
          - For easier of setting up vscode extension, you only need
            call `dap-edge-setup` after requiring `dap-edge`.
            
              - Or manually download and extract [VSCode Edge Debug
                Extension](https://marketplace.visualstudio.com/items?itemName=msjsdiag.debugger-for-edge).
        
          - Make sure that `dap-edge-debug-program` is pointing to the
            proper file.
        
          - Put in your configuration file:
            
            ``` elisp
            (require 'dap-edge)
            ```
    
    2.  Usage
        
        `dap-debug` or `dap-debug-edit-template` and select the edge
        template. For additional documentation on the supported template
        parameters or about different configuration templates refer to
        [Edge Debug
        Adapter](https://github.com/microsoft/vscode-edge-debug2).

4.  Node
    
    1.  Installation
        
          - For easier of setting up vscode extension, you only need
            call `dap-node-setup` after requiring `dap-node`.
            
              - Or manually download and extract [VSCode Node Debug
                Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode.node-debug2).
        
          - Make sure that `dap-node-debug-program` is pointing to the
            proper file.
        
          - Put in your configuration file:
            
            ``` elisp
            (require 'dap-node)
            ```
    
    2.  Usage
        
        `dap-debug` or `dap-debug-edit-template` and select the node
        template. For additional documentation on the supported template
        parameters or about different configuration templates refer to
        [Nodejs
        Debugging](https://code.visualstudio.com/docs/nodejs/nodejs-debugging).

## Powershell

``` elisp
(require 'dap-pwsh)
```

Start debugging by selecting "Powershell: Launch Script" from
`dap-debug` menu.
