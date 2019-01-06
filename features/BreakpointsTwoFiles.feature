Feature: Breakpoint tests which require multiple files

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
    Foo.bar();
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
    System.out.println(123);
    }
    }
    """
    And I call "save-buffer"

    And I start lsp-java
    And The server status must become "^LSP[jdtls:[0-9]+]$"

  @Breakpoints
  Scenario: Breakpoints in two files
    Given I switch to buffer "App.java"
    And I place the cursor before "Foo.bar()"
    And I call "dap-toggle-breakpoint"
    Given I switch to buffer "Foo.java"
    And I place the cursor before "System"
    And I call "dap-toggle-breakpoint"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    When I go in active window
    And the cursor should be before "Foo.bar()"
    When I call "dap-continue"
    Then The hook handler "breakpoint" would be called
    When I go in active window
    Then I should be in buffer "Foo.java"
    And the cursor should be before "System"
    And I call "dap-disconnect"
