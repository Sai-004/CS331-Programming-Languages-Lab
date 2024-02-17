public class Q3 extends Thread {

    // The run method is the entry point for the new thread
    public void run() {
        System.out.println("Hello, World!"); // Print a simple message when the thread is executed
    }

    public static void main(String[] args) {
        // Create an instance of the Q3 class, which extends Thread
        Q3 helloThread = new Q3();

        // Start the thread, invoking its run method concurrently
        helloThread.start();
    }    
}
