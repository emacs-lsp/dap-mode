Feature: Breakpoint tests

  Background:
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App {
    public static void main(String[] args) {
    System.out.print(123);
    foo();
    bar();
    }

    static int foo() {
    new App();
    return 10;
    }

    static int bar() {
    new App();
    return 10;
    }

    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "LSP::Started"

  @Breakpoints
  Scenario: Breakpoint navigation/cursor position
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "System"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Next
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "System"
    And I call "dap-next"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "foo"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Step in
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "System"
    And I call "dap-step-in"
    Then The hook handler "breakpoint" would be called
    When I go in active window
    Then I should be in buffer "java.io.PrintStream.java"
    And the cursor should be before "        write(String.valueOf(i));"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Terminate
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I attach handler "terminated" to hook "dap-terminated-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "System"
    And I call "dap-disconnect"
    Then The hook handler "terminated" would be called

  @Breakpoints
  Scenario: Step out
    When I place the cursor before "new App"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "stopped" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "stopped" would be called
    And the cursor should be before "new App"
    And I call "dap-step-out"
    Then The hook handler "stopped" would be called
    And the cursor should be before "foo()"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @WIP
  Scenario: Two breakpoints
    When I place the cursor before "foo()"
    And I call "dap-toggle-breakpoint"
    And I place the cursor before "bar()"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "foo()"
    When I call "dap-continue"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "bar()"
    When I call "dap-continue"
    And I should see buffer "*out*" with content "123"
