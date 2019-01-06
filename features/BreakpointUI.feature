Feature: Breakpoint UI tests

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
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I call "dap-ui-mode"

  @Breakpoints @UI
  Scenario: Inactive breakpoint
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    Then I should see the following overlay "dap-ui-pending-breakpoint-face"

  @UI @Breakpoints
  Scenario: Cursor placement
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    Then I should see the following overlay "dap-ui-marker-face"

  @UI @Breakpoints
  Scenario: Cursor removed - continue
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-continue"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI @Breakpoints
  Scenario: Cursor removed - next
    When I place the cursor before "System"
    Then I should not see the following overlay "dap-ui-marker-face"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-next"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI @Breakpoints
  Scenario: Cursor removed - step-in
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-step-in"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI @Breakpoints
  Scenario: Cursor removed - step-out
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-step-out"
    Then I should not see the following overlay "dap-ui-marker-face"

  @Breakpoints @UI @WIP
  Scenario: Verified breakpoint
    Given I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I place the cursor before "foo"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    When I place the cursor before "foo"
    Then I should see the following overlay "dap-ui-verified-breakpoint-face"

  @Breakpoints @UI
  Scenario: Enabled breakpoint
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    Then I should see the following overlay "dap-ui-verified-breakpoint-face"

  @Breakpoints @UI
  Scenario: Disable breakpoints after session shutdown
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I attach handler "terminated" to hook "dap-terminated-hook"
    When I call "dap-continue"
    Then The hook handler "terminated" would be called
    And I should see the following overlay "dap-ui-pending-breakpoint-face"

  @Breakpoints @UI
  Scenario: Filter dead sessions.
    And I attach handler "terminated" to hook "dap-terminated-hook"
    And I call "dap-java-debug"
    Then The hook handler "terminated" would be called
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
