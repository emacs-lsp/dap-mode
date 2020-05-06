# Extending DAP with new Debug servers

There are two methods that are used for registering remote extensions:

  - `dap-register-debug-provider` - register a method to call for
    populating startup parameters. It should either populate
    `:debugPort` and `:host` in case of TCP Debug Adapter Server or
    `:dap-server-path` when STD out must be used for Debug Adapter
    Server communication.
  - `dap-register-debug-template` register a debug template which will
    be available when `dap-debug` is called. The debug template must
    specify `:type` key which will be used to determine the provider to
    be called to populate missing fields.

Example

For full example you may check `dap-java.el`.

``` elisp
(dap-register-debug-provider
 "programming-language-name"
 (lambda (conf)
   (plist-put conf :debugPort 1234)
   (plist-put conf :host "localhost")
   conf))

(dap-register-debug-template "Example Configuration"
                             (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :name "Run Configuration"))
```
