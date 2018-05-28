Feature: Running without debug

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


  @Breakpoints
  Scenario: Add breakpoint
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I call "dap-java-debug"
    Then I should see buffer "*out*" with content "123"
