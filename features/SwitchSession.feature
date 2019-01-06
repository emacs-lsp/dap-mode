Feature: Switching active session

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
    Integer toEvaluate = 123123;
    System.out.print(toEvaluate);
    }

    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"

  @SwitchSession
  Scenario: No active session
    And I invoke "dap-switch-session" I should see error message "No active session to switch to"

  @SwitchSession
  Scenario: One running session
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And I invoke "dap-switch-session" I should see error message "No active session to switch to"

  @SwitchSession
  Scenario: Switch current session.
    Given I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I place the cursor before "Integer"
    And I call "dap-toggle-breakpoint"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-continue"
    And The hook handler "breakpoint" would be called
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "Integer"
    And I call "dap-switch-session"
    And the cursor should be before "System"

  @SwitchSession @UI
  Scenario: Switch current session - overlays
    Given I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-ui-mode"
    And I place the cursor before "Integer"
    And I call "dap-toggle-breakpoint"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I call "dap-continue"
    And The hook handler "breakpoint" would be called
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "Integer"
    And I call "dap-switch-session"
    And the cursor should be before "System"
    Then I should see the following overlay "dap-ui-marker-face"
    When I place the cursor before "Integer"
    Then I should not see the following overlay "dap-ui-marker-face"
