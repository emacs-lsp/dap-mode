Feature: Running without breakpoints

  @Running
  Scenario: Run without breakpoints
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
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I attach handler "terminated" to hook "dap-terminated-hook"
    When I call "dap-java-debug"
    Then The hook handler "terminated" would be called
    And I should see buffer "*out*" with content "123"

  @Running
  Scenario: Run without breakpoints - select class to execute
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/m/src/main/java/temp/App1.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App1 {
      public static void main(String[] args) {
        System.out.print(1);
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And I open a java file "tmp/m/src/main/java/temp/App2.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App2 {
      public static void main(String[] args) {
        System.out.print(2);
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I attach handler "terminated" to hook "dap-terminated-hook"
    And I start an action chain
    And I press "M-x"
    When I type "dap-java-debug"
    And I press "<return>"
    When I type "temp.App2"
    And I press "<return>"
    And I execute the action chain
    Then The hook handler "terminated" would be called
    And I should see buffer "*out*" with content "2"
    And I start an action chain
    And I press "M-x"
    When I type "dap-java-debug"
    And I press "<return>"
    When I type "temp.App1"
    And I press "<return>"
    And I execute the action chain
    Then The hook handler "terminated" would be called
    And I should see buffer "*out*<2>" with content "1"

  @Running @WIP
  Scenario: Run last configuration
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/m/src/main/java/temp/App1.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App1 {
      public static void main(String[] args) {
        System.out.print(1);
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And I open a java file "tmp/m/src/main/java/temp/App2.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    class App2 {
      public static void main(String[] args) {
        System.out.print(2);
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I attach handler "terminated" to hook "dap-terminated-hook"
    And I start an action chain
    And I press "M-x"
    When I type "dap-java-debug"
    And I press "<return>"
    When I type "temp.App2"
    And I press "<return>"
    And I execute the action chain
    Then The hook handler "terminated" would be called
    And I should see buffer "*out*" with content "2"
    When I call "dap-debug-last-configuration"
    Then The hook handler "terminated" would be called
    And I should see buffer "*out*<2>" with content "2"
