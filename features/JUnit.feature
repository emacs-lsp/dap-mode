Feature: JUnit tests

  Background:
    And I open a project file "test-project/src/test/java/temp/AppTest.java"
    And The server status must become "^LSP[jdtls:[0-9]+]$"

  @JUnit
  Scenario: Debug JUnit class
    When I place the cursor before "testAMarker"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug-test-class"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    System"
    And I call "dap-continue"
    And I should see buffer "*compilation*" which contains "testAMarker"

  @JUnit
  Scenario: Debug JUnit method
    When I place the cursor before "testAMarker"
    And I call "dap-breakpoint-toggle"
    And I go to beginning of buffer
    And I place the cursor before "public void testA"
    And I attach handler "breakpoint" to hook "dap-stopped-hook"
    And I call "dap-java-debug-test-class"
    Then The hook handler "breakpoint" would be called
    And the cursor should be before "    System"
    And I call "dap-continue"
    And I should see buffer "*compilation*" which contains "testAMarker"
