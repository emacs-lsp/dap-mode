package temp;

import org.junit.Test;

public class AppTest {
  @Test
  public void testA() {
    System.out.println("Entering testA...");
    foo();
  }

  @Test
  public void testB() {
    System.out.println("Entering testB...\n");
    foo();
  }

  private void foo() {
    System.out.println("Foo called.");
  }

  private void foo2() {
    System.out.println("Foo called.");
  }

}
