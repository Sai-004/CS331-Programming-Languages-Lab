public class Q2 {

    private static int counter = 0; // Shared counter variable

    public static void main(String[] args) {
        // Create two threads, one for incrementing and one for decrementing
        Thread incrementThread = new Thread(new IncrementTask());
        Thread decrementThread = new Thread(new DecrementTask());

        // Start both threads
        incrementThread.start();
        decrementThread.start();

        try {
            // Wait for both threads to finish
            incrementThread.join();
            decrementThread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // Display the final counter value
        System.out.println("Final Counter Value: " + counter);
    }

    // Runnable for incrementing the counter
    static class IncrementTask implements Runnable {
        @Override
        public void run() {
            for (int i = 0; i < 100; i++) {
                incrementCounter(); // Increment the counter in a synchronized manner
                System.out.println("Increment: " + (counter - 1) + " --> " + counter);
            }
        }
    }

    // Runnable for decrementing the counter
    static class DecrementTask implements Runnable {
        @Override
        public void run() {
            for (int i = 0; i < 100; i++) {
                decrementCounter(); // Decrement the counter in a synchronized manner
                System.out.println("Decrement: " + (counter + 1) + " --> " + counter);
            }
        }
    }

    // Synchronized method to increment the counter
    private synchronized static void incrementCounter() {
        counter++;
    }

    // Synchronized method to decrement the counter
    private synchronized static void decrementCounter() {
        counter--;
    }
}
