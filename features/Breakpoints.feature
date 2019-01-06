Feature: Breakpoint tests

  Background:
    And I open a project file "test-project/src/main/java/temp/App.java"
    And The server status must become "^LSP[jdtls:[0-9]+]$"

  @Breakpoints
  Scenario: Breakpoint + continue
    When I place the cursor before "System"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    System"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Next
    When I place the cursor before "System"
    And I call "dap-breakpoint-add"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And I attach handler "terminated" to hook "dap-terminated-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    System"
    And I call "dap-disconnect"
    Then The hook handler "terminated" would be called

  @Breakpoints
  Scenario: Step out
    When I place the cursor before "new App"
    And I call "dap-breakpoint-add"
    And I go to beginning of buffer
    And I attach handler "stopped" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "stopped" would be called
    And the cursor should be before "    new App"
    And I call "dap-step-out"
    Then The hook handler "stopped" would be called
    And the cursor should be before "    foo()"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Two breakpoints
    When I place the cursor before "foo()"
    And I call "dap-breakpoint-toggle"
    And I place the cursor before "bar()"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    foo()"
    When I call "dap-continue"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    bar()"
    When I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Toggle(disable) breakpoint
    When I place the cursor before "System"
    And I call "dap-breakpoint-toggle"
    And I call "dap-breakpoint-toggle"
    And I call "dap-java-debug"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Toggle(disable) breakpoint when running
    When I place the cursor before "foo()"
    And I call "dap-breakpoint-toggle"
    And I place the cursor before "bar()"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    foo()"
    And I place the cursor before "    bar()"
    And I call "dap-breakpoint-toggle"
    When I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints
  Scenario: Two breakpoints (enable second after starting)
    When I place the cursor before "foo()"
    And I call "dap-breakpoint-toggle"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    foo()"
    And I place the cursor before "bar()"
    And I call "dap-breakpoint-toggle"
    When I call "dap-continue"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    bar()"
    When I call "dap-continue"
    And I should see buffer "*out*" with content "123"

  @Breakpoints @Persistence
  Scenario: Toggle(disable) breakpoint when running
    When I place the cursor before "System"
    And I call "dap-breakpoint-toggle"
    And I kill buffer "App.java"
    And I open a project file "test-project/src/main/java/temp/App.java"
    And I start lsp-java
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    System"
    And I call "dap-continue"
    And I should see buffer "*out*" with content "123"
