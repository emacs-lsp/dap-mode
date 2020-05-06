Clojure debugging and introspecting Java Code
==============================================

One of the limitations of Cider is that it does not support debugging Java(or it is not very easy to do so). This page provides information on how to do that using `dap-mode`
As a bonus, it will also enable navigation through java code like finding definitions, references, implementations, etc.

## Java code navigation

1. Follow the guides on installing `lsp-java`. If you are `Spacemacs` user you need to add the following layers `dap`,`lsp`, `(lsp-java :variables java-backend 'lsp)`. `lsp-java` automatically downloads all required server-side components(`JDT Language Server`, `Java Debug Adapter` and `JUnit Test Runner`).
2. Go to the root of your project and do
``` bash
lein pom
```
3. Open a file from your project and do `C-u`- `M-x` - `lsp` which will prompt you to select a server to start, select `jdtls` server. Once the server has started(the initialization may take some time since JDT LS is downloading sources) you may call `xref-appropos` and open type a Java Type like `ArrayList` or `PersistentList`. When you are in java file you can inspect documentation, look for class implementations, references and so on.
4. Invoke `xref-apropos`, type a Java class name and then you can set breakpoints, go to definitions, find references, etc.

### Implementations of `clojure.lang.IReduce`
![Debug cider](/screenshots/implementations.png)

## Debugging
1. Configure cider to start nrepl with remote debugging enabled and do `cider-jack-in`

```emacs-lisp
(setenv "JAVA_OPTS" "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=1044")
```

2. Call `dap-debug` and select `Java Attach configuration` and enter port 1044 (the same port as the port above.).
3. Go to java class and place a breakpoint using `dap-breakpoint-toggle`.

### Debugging in progress
![Debug cider](/screenshots/debug.png)

