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

;; The modes below are optional

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

## Kotlin

1.  Installation 
    To use dap-mode with Kotlin, you need to download the [kotlin-debug-adapter](https://github.com/fwcd/kotlin-debug-adapter). 
    The releases are a bit infrequent, so it is recommended to build it from source yourself.
    You will also need to have installed `lsp-mode`, as `dap-kotlin` shares some configuration with it.
    After building it, point the variable `lsp-kotlin-debug-adapter-path ` to the path of the kotlin-debug-adapter executable.
    You will find this in the path `adapter/build/install/adapter/bin` (from the kotlin-debug-adapter root).
    You should also make sure that `lsp-kotlin-debug-adapter-enabled` is set to true.

2.  Usage
    **First of all, each time you you want to debug, make sure you BUILD YOUR PROJECT FIRST!**
    Simply running your regular build with Maven or Gradle should be enough.
    
    You can set up debug templates using Kotlin. `dap-kotlin`provides some sensible defaults, 
    but there are one parameters you MUST give yourself:
    - `:mainClass`: The class name, including package for the main class you want to run. If the class takes argument, you can give them as well.
    If project root path needs to be different, you can give it using the parameter `:projectRoot`. 
    Other parameters include:
    - `:type`: `launch` or `attach`
    - `:hostName`: If type is `attach`, you can specify a hostname to connect to. Defaults to `localhost`.
    - `:port`: If type is `attach`, you can specify a port to connect to. Defaults to `5005`.
    - `:noDebug`: Whether or not to use a debug session
    - `:enableJsonLogging`: Enable logging of adapter communication logs.
    - `:jsonLogFile`: File to log to.
   
   
   Thanks to interop with `lsp-kotlin`, you can have it set up code lenses with run/debug-options for main classes.
   For this to work, you need kotlin-langauge-server running, be in a file with a main method, and have activated `lsp-kotlin-lens-mode`
   
   Sadly, there is no test-running functionality like in Java `dap-mode`. This can be combated by setting up a debug template with
   the Junit5 `ConsoleLauncher`. Remember that this class needs to be part of your classpath. Sometimes this is included in bigger frameworks
   testing utilities, but can also be included explicitly by adding the `junit-platform-console` dependency.
   
   ```elisp
   (dap-register-debug-template "Kotlin tests with launcher"
                             (list :type "kotlin"
                                   :request "launch"
                                   :mainClass "org.junit.platform.console.ConsoleLauncher --scan-class-path"
                                   :enableJsonLogging nil
                                   :noDebug nil))
   ```
   This will run all tests in the projects in debug mode, with no json logging. You can experiment with the 
   arguments to `ConsoleLauncher`. Arguments are documented on [the official JUnit website](https://junit.org/junit5/docs/current/user-guide/#running-tests-console-launcher-options).
   

## Python

1.  Installation

	Make sure to install the required modules in the project's virtual
    environment (if any).

    - install debugpy
    ``` bash
    pip install "debugpy"
    ```
	- Or install ptvsd
	```bash
	pip install "ptvsd>=4.2"
	```
	NOTE: ptvsd is depracated, and as of 8/10/2022, ptvsd caused dap to break when it hits a breakpoint.
	This comment and issue has context: https://github.com/emacs-lsp/dap-mode/issues/625#issuecomment-1128961454

    - Then add the following line in your config:
    ```elisp
    (require 'dap-python)
    ;; if you installed debugpy, you need to set this
    ;; https://github.com/emacs-lsp/dap-mode/issues/306
    (setq dap-python-debugger 'debugpy)
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

3. Template parameters

These parameters are handled by templates of type `python`:

- `:target-module`: path to some python file to be executed
                    (supersedes `:program`)
- `:program`: path to some python script/program to be executed
              (defaults to the buffer's file-name)
- `:module`: Python module to be executed using `-m …`.
             If given, this will be added to the argument list in
             front of program resp. target-module.
- `:args`: either a string or a list of strings (`["foo" "bar"]`)
           If a string, it will be split into arguments and shell-unqouted.
- `:debugger`: Debugger to use, either `"debugpy"` or `"ptvsd"`
               (defaults to the value defined in `dap-python-debugger`)

Remaining parameters are forwarded to the respective debugger.

4. Attach to an existing process
  `dap-python` supports also the "attach" mode to attach and debug a long running script.
    A template named "Python :: Attach to running process" is also pre-registered for this purpose.
    ```elisp
    (dap-register-debug-template "Python :: Attach to running process"
      (list :type "python"
            :request "attach"
            :processId "${command:pickProcess}"
            :name "Python :: Attach to running process"))
    ```
    The `${command:pickProcess}` configuration variable used by default to facilitate the user
    selecting the debug process by a completion popping up window. The real `processId` can
    always be specified using its *pid*.
    **Note**: on Linux this is achieved using the [ptrace syscall](https://www.man7.org/linux/man-pages/man2/ptrace.2.html "ptrace syscall man page")
    wrapped inside the GDB tool, which means that for distributions that enable YAMA (e.g. Ubuntu) some additional steps may be necessary:
      - Install GDB.
      - Turn on classic ptrace permission
        ```bash
        sudo sh -c 'echo 0 > /proc/sys/kernel/yama/ptrace_scope'
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

## C and C++

Please see the [Native Debugging](#native_debugging) section.

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

## Erlang

Make sure that you have properly configured `Erlang` and that you have
[Erlang LS](https://github.com/erlang-ls/erlang_ls) binaries on the
path and put in your emacs configuration.

``` elisp
(require 'dap-erlang)
```

For more information about DAP support in Erlang LS please refer to
[the official documentation](https://erlang-ls.github.io/).

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

## SWI-Prolog

1. With SWI-Prolog [installed](https://www.swi-prolog.org/Download.html),
   run the following command to build and install the debug adapter executable `swipl_debug_adapter`:
   ```shell
   swipl -g "pack_install(debug_adapter)" -t halt
   ```
2. Add the following line to your Emacs setup:
   ``` elisp
   (require 'dap-swi-prolog)
   ```
3. Run `dap-debug` in a Prolog buffer and start debugging.

## <a name="native_debugging"></a>Native Debuging (C, C++, Rust etc)

There are a number of options for using `dap-mode` with languages which compile
to native code. All of these use the [GDB](https://sourceware.org/gdb/) or
[LLDB](https://lldb.llvm.org/) debuggers in one way or another.

### `dap-gdb`

This package requires GDB version 14 or later. It communicates directly with the
debugger using DAP JSON (without the need for an adapter layer), and there is no
directly equivalent VSCode extension.

This package registers two types of debugger:

* `:type "gdb"`
* `:type "gdbserver"`

The latter allows debugging of programs which are running (usually remotely)
under the
[`gdbserver`](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Server.html)
program.

1.  Installation

    Please ensure that you have GDB version 14 or later installed on your
    system, and add the following line to your Emacs setup:
    ``` elisp
    (require 'dap-gdb)
    ```
    If `gdb` is not on your `PATH`, set or customize `dap-gdb-debug-program` with
    the flags requried to start it in DAP mode, e.g.:
    ``` elisp
    (setq dap-gdb-debug-program '("/path/to/gdb" "-i" "dap"))
    ```

2.  Usage

    Create a configuration with <kbd>M-x</kbd> `dap-debug-edit-template`, using
    either "GDB Run Configuration" or "GDBServer Connect Configuration" as the
    base settings. See the [GDB
    documentation](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Debugger-Adapter-Protocol.html)
    for full descriptions of launch configuration options.

    **Note** you must set the `:program` property in your debug template,
    otherwise you will get an error when the debugger attempts to execute your
    source file.

### `dap-lldb`

LLDB is a debugger that supports, among others, C, C++, Objective-C and
Swift. LLDB supports direct communication with the debugger using DAP JSON,
without an adapter layer, similar to newer versions of GDB. Unlike GDB, the DAP
server is provided by a separate binary which ships with the main LLDB command
tool. The binary is called `lldb-vscode` on older versions, and is called
`lldb-dap` since version 18.

This package registers a single debugger type:

* `:type lldb-vscode`

The official VSCode extension is [LLDB
DAP](https://marketplace.visualstudio.com/items?itemName=llvm-vs-code-extensions.lldb-dap),
although it is not necessary to install this to use `dap-lldb` in Emacs.

1.  Installation

    Ensure that you have the command-line program `lldb-dap` (or `lldb-vscode`)
    installed on your system. If you want to build the tool from scratch, clone
    and follow the instructions to compile lldb-dap from:

    <https://github.com/llvm/llvm-project/tree/main/lldb/tools/lldb-dap>

    **Note**: For proper Swift support, you need to compile LLDB from
    <https://github.com/apple/swift-lldb>.

    Then put the following in your emacs configuration:
    ``` elisp
    (require 'dap-lldb)
    ```

    This package expects to find the `lldb-vscode` program in the extensions
    folder used by VSCode. If you have this installed elsewhere, or are using
    the more modern `lldb-dap` version, you will need to set or customize
    `dap-lldb-debug-program` appropriately:

    ``` elisp
    (setq dap-lldb-debug-program '("/path/to/lldb-dap"))
    ```

2.  Usage

    Create a configuration with <kbd>M-x</kbd> `dap-debug-edit-template`,
    selecting the template prefixed "LLDB (VS Code)" for the base
    settings. Detailed descriptions of the launch configuration options are
    provided in the [LLDB DAP
    README](https://github.com/llvm/llvm-project/tree/main/lldb/tools/lldb-dap). Remember
    to set the `:program` property to the path to the binary you wish to debug.


### `dap-cpptools`

This package utilizes the official Microsoft extension [VSCode
CPPTools](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools),
which ships with a debug adapter which translates DAP JSON into GDB's [Machine
Interface (MI)
protocol](https://sourceware.org/gdb/current/onlinedocs/gdb.html/GDB_002fMI.html),
(used by many debugger GUIs, including Emacs' built-in gud debugger). It
therefore works with older versions of GDB which do not support the DAP protocol
directly. The adapter is written in C# and is called
[OpenDebugAD7](https://github.com/microsoft/MIEngine/wiki/Architecture-of-OpenDebugAD7). The
installation steps below will install this program as an executable binary on
your system, so it can be used by this package.

This package registers a single debugger type:

* `:type cpptools`

which uses the OpenDebugAD7 adapter to run GDB debugging sessions.

1.  Installation

    Add the following to your Emacs configuration:
    ``` elisp
    (require 'dap-cpptools)
    ```

    A one-time step is required to download the extension and set it up
    automatically - <kbd>M-x</kbd> `dap-cpptools-setup` - then you are good
    start debugging. Note that this will download the extension from the address
    specified in `dap-cpptools-download-url`, which is by default set to a
    specific release under
    <https://github.com/microsoft/vscode-cpptools/releases>.

    The location of the OpenDebugAD7 binary is specified by
    `dap-cpptools-debug-program`, which can be customized if needed.

2.  Usage

    Run <kbd>M-x</kbd> `dap-debug-edit-template` and select the "cpptools"
    prefixed template as a base. Additional configuration options are documented
    on the extension's [official
    page](https://code.visualstudio.com/docs/cpp/launch-json-reference).


### `dap-gdb-lldb`

This package uses the VSCode extension [WebFreak Native
Debug](https://marketplace.visualstudio.com/items?itemName=webfreak.debug). This
extension is written in Typescript and implements a DAP JSON to MI
translation layer, similar to the CPPTools extension. The extension advertizes
support for both GDB and LLDB - LLDB support requires the additional binary
`lldb-mi`; to install this follow the instructions in the link above.

This package registers several debugger types:

* `:type gdb`
* `:type gdbserver`
* `:type lldb-mi`

Note that the first two conflict with the types registered by the `dap-gdb`
package, so you must avoid adding both `dap-gdb` and `dap-gdb-lldb` to your
Emacs configuration.

The source code for the extension may be found at
<https://github.com/WebFreak001/code-debug>.

1.  Installation

    Add the following to your Emacs configuration:
    ``` elisp
    (require 'dap-gdb-lldb)
    ```

    If you do not have the VSCode extension installed already, you can install it with
    <kbd>M-x</kbd> `dap-gdb-lldb-setup`. Or download and extract [VSCode
    extension](https://marketplace.visualstudio.com/items?itemName=webfreak.debug)
    (make sure that `dap-gdb-lldb-path` is pointing to the extract
    location).

    You will also need to have [NodeJS](https://nodejs.org/) installed on your
    system, to run the adapter.

2.  Usage

    Run <kbd>M-x</kbd> `dap-debug-edit-template` and select one of the following
    base templates, depending on your needs:

    * "GDB Run Configuration"
    * "GDBServer Connect Configuration"
    * "LLDB Run Configuration"

    See the project's [README](https://github.com/WebFreak001/code-debug) for
    additional documentation on the launch configurations. **Note** - be sure to
    set the working directory (`:cwd` property) to an absolute path - the
    default may insert a tilde (~) shortcut for your home directory (which the
    adapter does not understand) and this will cause a launch failure with a
    misleading error message questioning the existence of the target binary.

## Rust

First, please see the [Native Debugging](#native_debugging) section.

The rustup tool (at least for the x86_64 toolchain) provides additional
configuration for both GDB and LLDB to provide pretty-printers for standard Rust
types. The most straightforward way to use this functionality is to configure
the debug adapters to pick up these scripts rather than the debugger
directly. For `dap-gdb`, you can configure the GDB debug program as follows
(assuming `~/.cargo/bin` is in your `$PATH`):

```elisp
(setq dap-gdb-debug-program '("rust-gdb" "-i" "dap"))
```

For the MI debug adapters, you can supply the debugger program as part of the
launch configuration - for `dap-cpptools` the relevant property is
`:miDebuggerPath`, and for `dap-gdb-lldb` it is `:gdbpath`.

Another way to enable pretty-printing is to specify the relevant configuration
steps as part of the launch intialization (and this is the only option for
`dap-lldb`). Looking at the `rust-lldb` script in
`~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin`, we see that it
sources a python file to define the pretty printers, and then a command file to
register them. The same things can be done as an initialization step when
launching the DAP debugger, for example:

```elisp
(dap-register-debug-template
 "MyProgram"
 (list :type "lldb-vscode"
       :initCommands '("command script import ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_lookup.py"
                       "command source ~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc/lldb_commands")
       :request "launch"
       ...
 ))
```

## Go

1.  Installation

      - install gopls

      - Install the delve command by following instructions on [delve -
        installation](https://github.com/go-delve/delve/tree/master/Documentation/installation).

      - install lsp-mode

      - Put in your emacs configuration.

        ``` elisp
        (require 'dap-dlv-go)
        ```
    <!-- end list -->

    1.  Usage

        assume you have your code at \~/src/cool/cmd/app/app.go

          - open your main package file e.g \~/src/cool/cmd/app/app.go
          - or open a test file e.g app<sub>test</sub>.go
          - add folder to lsp session where your go.mod is or would be
              - M-x lsp-workspace-folders-add \~/src/cool
          - set break point
          - M-x dap-debug
          - if you want to launch a binary or test to debug, use "Go
            Dlv Launch File Configuration"
          - if you want to debug current test function inside test
            file use "Go Dlv Test Current Function Configuration"
          - if you want to debug current subtest put your cursor on this
            subtest and use "Go Dlv Test Current Subtest Configuration"
          - if you want to debug already running application select
            "Go Dlv Attach Configuration"
          - if you want to debug binary and need to interact with your
            application, install `vterm` package and call `M-x`
            `dap-dlv-go-debug-in-vterm`. If you need to add arguments
            to your command, use `C-u` `M-x` `dap-dlv-go-debug-in-vterm`
          - if you want to debug remote application you need start
            delve on remote machine first, for example: `dlv --headless
            --accept-multiclient attach 123 -l :1080` (see [dlv usage
            documentation](https://github.com/go-delve/delve/tree/master/Documentation/usage#using-delve)
            for more command-line options) and select "Go Dlv Remote Debug"
          - if your build environment differs from your development
            environment (for example, you build project inside docker
            container or in CI pipeline) or you use `-trimpath` build
            flag you can use `substitutePath`. `M-x`
            `dap-debug-edit-template` then select some template and
            add `substitutePath` option to it:
            ```elisp
            (dap-register-debug-template
             "Launch Executable trimmed path"
             (list :type "go"
                   :request "launch"
                   :name "Launch Executable trimmed path"
                   :mode "exec"
                   :program nil
                   :args nil
                   :env nil
                   :substitutePath (vector (ht ("from" "/home/user/projects/tribonacci") ("to" "github.com/s-kostyaev/tribonacci")))))
            ```
            and after evaluation you can select this edited template
            and debug as usual. You also can use
            [dir-locals](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
            to save this template for your project. Create
            `.dir-locals.el` in root directory of your project:
			```elisp
            ((go-mode  . ((eval . (progn
                                    (dap-register-debug-template
                                     "Remote debug in docker container"
                                     (list :type "go"
                                           :request "attach"
                                           :name "Remote debug in docker container"
                                           :mode "remote"
                                           :substitutePath (vector (ht ("from" "/home/user/projects/tribonacci") ("to" "/app"))))))))))
            ```
	    If you need to provide build flags, use `:buildFlags` key.

    2.  Trouble shooting

          - put (setq dap-print-io t) and check messages buffer
          - add `--log --log-output debugger,rpc,dap` to `dlv` command
            and check `dlv` logs
          - e.g linter can return note at debug session response
            resulting debug session to fail

## Go (VS Code debug adapter)

This debug adapter is deprecated. Use `dap-dlv-go` instead.

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

## Netcoredbg

[Netcoredbg](https://github.com/Samsung/netcoredbg) is debugger that supports C#, F# and probably other
languages of Dotnet platform.

``` elisp
(require 'dap-netcore)
```

If you have Emacs 26 or older you also need customize `dap-netcore-download-url`:

`M-x` `customize` `RET` `dap-netcore-download-url` `RET`

Start debugging by selecting ".Net Core Launch (Console)" or ".Net
Core Attach (Console)" from `dap-debug` menu.

## Unity

1.  Installation

	- Automatic Installation:

		- Add `dap-unity` to your configuration file

		- Call `dap-unity-setup` to automatically download and extract the debugger

		- If automatic installation fails, see the manual installation steps below

	- Manual Installation:

		- Download the unity-debug extension from the [VisualStudio Marketplace](https://marketplace.visualstudio.com/items?itemName=Unity.unity-debug)
		
		- Extract the extension contents (Default path: "{emacsconfigdir}/.extension/vscode/Unity.unity-debug/")
		
		- On non-Windows os, the debugger "{extensiondir}/extension/bin/UnityDebug.exe" must be flagged as executable
			Using your favorite terminal: `chmod u+x UnityDebug.exe`
		
2.  Usage

	Call `dap-debug` and select the "Unity Editor" template

## OCaml

[earlybird](https://github.com/hackwaly/ocamlearlybird) is an OCaml debugger with support for DAP.

1. Installation

   - Install earlybird through `opam install earlybird`
   
   - Put in your configuration file:

   ``` elisp
   (require 'dap-ocaml)
   ```

2. Usage

    Call `dap-debug` and edit the "OCaml Debug Template". Make sure to set the program field as the path of your *byte-code* compiled program.
    Note that this debugger _only_ works with bytecode compiled OCaml programs.


## Julia

[DebugAdapter.jl](https://github.com/julia-vscode/DebugAdapter.jl) is DAP-compatible server built on top of [Debugger.jl](https://github.com/JuliaDebug/Debugger.jl).

To setup, first install `DebugAdapter.jl`:

```
$ julia -e 'import Pkg; Pkg.add("DebugAdapter")'
```

Then add the following to your configuration:


``` elisp
(require 'dap-julia)
```

To launch a debug session, call `dap-debug` with the `Julia Run Configuration` template.
