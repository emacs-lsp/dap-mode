Feature: Running without debug

  @Running
  Scenario: Organize imports
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I have a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    import java.util.ArrayList;

    class App {
      public static void main(String[] args) {
        System.out.print(123);
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "LSP::Started"
    When I call "dap-java-debug"
    Then I should see buffer "*out*" with content "123"
