Feature: Sessions

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
    }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "LSP::Started"

  @WIP
  Scenario: Listing sessions
    Given I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And the cursor should be before "System"
    When I call "dap-ui-list-sessions"
    Then I should be in buffer "*sessions*"
    Then I should see:
    """
    [+] Default Debug
    """
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"
