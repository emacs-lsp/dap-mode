Feature: Multiple threads

  @Threads
  Scenario: Switching threads when multiple stopped threads.
    Given I have maven project "m" in "tmp"
    And I add project "m" folder "tmp" to the list of workspace folders
    And I open a java file "tmp/m/src/main/java/temp/App.java"
    And I clear the buffer
    And I insert:
    """
    package temp;

    import java.util.concurrent.Executors;

    public class App {
        public static void main(String[] args) {
            Executors.newSingleThreadExecutor().execute(new Runnable() {
                @Override
                public void run() {
    System.out.println(999);
                }
        });
    System.out.println(888);
        }
    }
    """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I place the cursor before "888"
    And I call "dap-toggle-breakpoint"
    And I place the cursor before "999"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I attach handler "minibuffer" to hook "minibuffer-setup-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I start an action chain
    And I press "M-x"
    And I type "dap-switch-thread"
    And I press "<return>"
    And I press "M-x"
    And I type "dap-steps--wait-minibuffer"
    And I press "<return>"
    When I type "Thread [pool-1-thread-1]"
    And I press "<return>"
    And I execute the action chain
    And The hook handler "breakpoint" would be called
    Then the cursor should be before "System.out.println(999);"
    And I start an action chain
    And I press "M-x"
    And I type "dap-switch-thread"
    And I press "<return>"
    And I press "M-x"
    And I type "dap-steps--wait-minibuffer"
    And I press "<return>"
    When I type "Thread [main]"
    And I press "<return>"
    And I execute the action chain
    And The hook handler "breakpoint" would be called
    Then the cursor should be before "System.out.println(888);"
