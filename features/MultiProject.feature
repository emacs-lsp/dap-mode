Feature: Switching active session

  Background:
    Given I have maven project "projectA" in "tmp"
    And I add project "projectA" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/projectA/src/main/java/projectA/ProjectA.java"
    And I clear the buffer
    And I insert:
    """
    package projectA;

    class ProjectA {
    public static void main(String[] args) {
    Integer toEvaluate = 123;
    System.out.print(toEvaluate);
    }

    }
    """
    And I call "save-buffer"
    Given I have maven project "projectB" in "tmp"
    And I add project "projectB" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/projectB/src/main/java/projectB/ProjectB.java"
    And I clear the buffer
    And I insert:
    """
    package projectB;

    class ProjectB {
    public static void main(String[] args) {
    Integer toEvaluate = 123;
    System.out.print(toEvaluate);
    }

    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And I switch to buffer "ProjectA.java"
    And I start lsp-java
    And The server status must become "LSP::Started"

  @MultiProject @SwitchSession @MultiSession @WIP
  Scenario: No active session
    And I attach handler "terminated" to hook "dap-terminated-hook"
    When I call "dap-java-debug"

    # And I start an action chain
    # And I press "M-x"
    # When I type "dap-java-debug"
    # And I press "<return>"
    # When I type "projectA.ProjectA"
    # And I press "<tab>"
    # # # And I press "C-n"
    # And I press "<return>"
    # And I execute the action chain
    # # When I call "dap-java-debug"
    # Then The hook handler "terminated" would be called
    # And I should see buffer "*out*" with content "123"
