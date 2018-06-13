Feature: Stack traces

  Background:
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    public class App {
      public static void main(String[] args) {
        String inMainMethod = "inMainMethod";
        foo();
      }

      private static void foo() {
        String inFooMethod = "inFooMethod";
        System.out.println();
      }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "LSP::Started"

  @StackFrames
  Scenario: Eval multiple frames
    When I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And I attach handler "executed" to hook "dap-executed-hook"
    And I start an action chain
    When I press "M-x"
    When I type "dap-eval"
    And I press "<return>"
    When I type "inFooMethod"
    And I press "<return>"
    And I execute the action chain
    Then The hook handler "executed" would be called
    And I should see message matching regexp "\"inFooMethod\".*"
    And I start an action chain
    When I press "M-x"
    When I type "dap-switch-stack-frame"
    And I press "<return>"
    When I type "App.main"
    And I press "<return>"
    And I execute the action chain
    And the cursor should be before "    foo();"
