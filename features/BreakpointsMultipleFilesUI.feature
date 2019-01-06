Feature: Breakpoint UI tests
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
      System.out.print(123);
      }
      }
      """
    And I call "save-buffer"
    And I start lsp-java
    And I open a java file "tmp/m/src/main/java/temp/Foo.java"
    And I clear the buffer
    And I insert:
      """
      package temp;

      class Foo {
      public static void bar() {
      System.out.print(123);
      }
      }
      """
    And I call "save-buffer"
    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"
    And I call "dap-ui-mode"

  @Breakpoints @UI
  Scenario: Open/close file workspace started/without debug session running.
    Given I switch to buffer "App.java"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I kill buffer "App.java"
    And I open a java file "tmp/m/src/main/java/temp/App.java"
    And I start lsp-java
    And I call "dap-ui-mode"
    When I place the cursor before "System"
    Then I should see the following overlay "dap-ui-pending-breakpoint-face"

  @Breakpoints @UI
  Scenario: Open/close file workspace started/with debug session running.
    Given I switch to buffer "App.java"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    And The hook handler "breakpoint" would be called
    And I kill buffer "App.java"
    And I open a java file "tmp/m/src/main/java/temp/App.java"
    And I start lsp-java
    And I call "dap-ui-mode"
    When I place the cursor before "System"
    Then I should see the following overlay "dap-ui-verified-breakpoint-face"
