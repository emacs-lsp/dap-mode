Feature: Threads
  Background:
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App {
    public static void main(String[] args) {
    foo();
    }
    public static void foo() {
    System.out.print(123);
    }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I place the cursor before "System"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And the cursor should be before "System"

  @Threads
  Scenario: Listing sessions
    When I call "dap-ui-sessions"
    Then I should be in buffer "*sessions*"
    Then I should see:
    """
    [+] temp.App (m) (running)
    """

  @Threads
  Scenario: Listing sessions - listing threads
    When I call "dap-ui-sessions"
    Then I should be in buffer "*sessions*"
    And I should see:
    """
    [+] temp.App (m) (running)
    """
    When I place the cursor before "temp"
    And I attach handler "threads-expanded" to hook "dap-ui-stack-frames-loaded"
    And I call "tree-mode-expand-level"
    Then I should see:
    """
    [-] temp.App (m) (running)
     ‘-Loading...
    """
    And The hook handler "threads-expanded" would be called
    Then I should see:
    """
    [-] temp.App (m) (running)
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[+] Thread [main]
    """

  @Threads @UI @Stackframes
  Scenario: Stackframes
    When I call "dap-ui-sessions"
    Then I should be in buffer "*sessions*"
    And I should see:
    """
    [+] temp.App (m) (running)
    """
    When I place the cursor before "temp"
    And I attach handler "threads-expanded" to hook "dap-ui-stack-frames-loaded"
    And I call "tree-mode-expand-level"
    And The hook handler "threads-expanded" would be called
    And I should see:
    """
    [-] temp.App (m) (running)
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[+] Thread [main] (stopped)
    """
    When I place the cursor before "main"
    And I call "tree-mode-expand-level"
    And I should see:
    """
    [-] temp.App (m) (running)
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[-] Thread [main] (stopped)
        |-App.foo() (App.java:8)
        ‘-App.main(String[]) (App.java:5)
    """
