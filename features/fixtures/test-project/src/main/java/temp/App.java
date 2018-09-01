package temp;

class App {
  public static void main(String[] args) {
    System.out.print(123);
    foo();
    bar();
  }

  static int foo() {
    new App();
    return 10;
  }

  static int bar() {
    new App();
    return 10;
  }
}
