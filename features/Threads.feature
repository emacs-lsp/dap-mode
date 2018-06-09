Feature: Sessions

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
    And The server status must become "LSP::Started"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And the cursor should be before "System"


  @ThreadsList
  Scenario: Listing sessions
    When I call "dap-ui-list-sessions"
    Then I should be in buffer "*sessions*"
    Then I should see:
    """
    [+] Default Debug
    """

  @ThreadList
  Scenario: Listing sessions - listing threads
    When I call "dap-ui-list-sessions"
    Then I should be in buffer "*sessions*"
    And I should see:
    """
    [+] Default Debug
    """
    When I place the cursor before "Default"
    And I attach handler "threads-expanded" to hook "dap-ui-stack-frames-loaded"
    And I call "tree-mode-expand-level"
    Then I should see:
    """
    [-] Default Debug
     ‘-Loading...
    """
    And The hook handler "threads-expanded" would be called
    Then I should see:
    """
    [-] Default Debug
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[+] Thread [main]
    """

  @Threads @UI @Stackframes
  Scenario: Stackframes
    When I call "dap-ui-list-sessions"
    Then I should be in buffer "*sessions*"
    And I should see:
    """
    [+] Default Debug
    """
    When I place the cursor before "Default"
    And I attach handler "threads-expanded" to hook "dap-ui-stack-frames-loaded"
    And I call "tree-mode-expand-level"
    And The hook handler "threads-expanded" would be called
    And I should see:
    """
    [-] Default Debug
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[+] Thread [main] (Stopped)
    """
    When I place the cursor before "main"
    And I call "tree-mode-expand-level"
    And I should see:
    """
    [-] Default Debug
     |-[+] Thread [Signal Dispatcher]
     |-[+] Thread [Finalizer]
     |-[+] Thread [Reference Handler]
     ‘-[-] Thread [main]
        |-App.foo()
        ‘-App.main(String[])
    """
