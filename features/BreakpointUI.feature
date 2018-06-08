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

  @WIP
  Scenario: Toggle breakpoint
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    Then I should see the following overlay "dap-ui-pending-breakpoint-face"
