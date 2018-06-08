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
    And I call "dap-ui-mode"

  @Breakpoints
  @UI
  Scenario: Inactive breakpoint
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    Then I should see the following overlay "dap-ui-pending-breakpoint-face"

  @UI
  @Breakpoints
  Scenario: Cursor placement
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    Then I should see the following overlay "dap-ui-marker-face"

  @UI
  @Breakpoints
  Scenario: Cursor removed - continue
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-continue"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI
  @Breakpoints
  Scenario: Cursor removed - next
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-next"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI
  @Breakpoints
  Scenario: Cursor removed - step-in
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-step-in"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI
  @Breakpoints
  Scenario: Cursor removed - step-out
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-step-out"
    Then I should not see the following overlay "dap-ui-marker-face"

  @UI
  @Breakpoints
  Scenario: Cursor removed - terminate
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-terminate"
    Then I should not see the following overlay "dap-ui-marker-face"
