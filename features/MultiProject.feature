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
    And I call "dap-ui-mode"
    And I switch to buffer "ProjectA.java"
    And I start lsp-java
    And I call "dap-ui-mode"
    And The server status must become "LSP::Started"

  @MultiProject @SwitchSession @MultiSession @WIP
  Scenario: No active session
    Given I attach handler "breakpoint" to hook "dap-stopped-hook"
    # place breakpoints
    And I switch to buffer "ProjectA.java"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I switch to buffer "ProjectB.java"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "terminated" to hook "dap-terminated-hook"
    # Start ProjectA
    And I start an action chain
    And I press "M-x"
    And I type "dap-java-debug"
    And I press "<return>"
    And I type "projectA"
    And I press "TAB"
    And I press "RET"
    And I execute the action chain
    And The hook handler "breakpoint" would be called
    # Start ProjectB
    And I start an action chain
    And I press "M-x"
    And I type "dap-java-debug"
    And I press "<return>"
    And I type "projectB"
    And I press "TAB"
    And I press "RET"
    And I execute the action chain
    And The hook handler "breakpoint" would be called

    # Switch session
    And I call "dap-switch-session"
    Then I should be in buffer "ProjectA.java"
    And I should see the following overlay "dap-ui-verified-breakpoint-face"
    And I should see the following overlay "dap-ui-marker-face"

    # Check markers the other buffer
    And I switch to buffer "ProjectB.java"
    And I should see the following overlay "dap-ui-verified-breakpoint-face"
    And I should not see the following overlay "dap-ui-marker-face"
