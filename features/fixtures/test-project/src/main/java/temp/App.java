package temp;

class App {
    public static void main(final String[] args) {
        System.out.println("Next line will throw...");
        throw new RuntimeException("Error message!");
    }
}
