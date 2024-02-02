public class Q1 {
    private static final Object lock = new Object(); // Shared lock object for synchronization
    private static int number = 1; // Shared variable to keep track of the current number
    private static final int MAX_NUMBER = 100; // Maximum number to be printed

    public static void main(String[] args) {
        // Create two threads, one for odd numbers and one for even numbers
        Thread oddThread = new Thread(new OddNumberPrinter(), "Odd Thread");
        Thread evenThread = new Thread(new EvenNumberPrinter(), "Even Thread");

        // Start both threads
        oddThread.start();
        evenThread.start();
    }

    // Runnable for printing odd numbers
    static class OddNumberPrinter implements Runnable {
        @Override
        public void run() {
            while (number <= MAX_NUMBER) {
                synchronized (lock) {
                    // Check if the current number is odd
                    if (number % 2 != 0) {
                        // Print the current odd number and increment it
                        System.out.println(Thread.currentThread().getName() + "\t:" + number++);
                        lock.notify(); // Notify the other thread to start
                    } else {
                        try {
                            lock.wait(); // Wait for the other thread to notify
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    // Runnable for printing even numbers
    static class EvenNumberPrinter implements Runnable {
        @Override
        public void run() {
            while (number <= MAX_NUMBER) {
                synchronized (lock) {
                    // Check if the current number is even
                    if (number % 2 == 0) {
                        // Print the current even number and increment it
                        System.out.println(Thread.currentThread().getName() + "\t:" + number++);
                        lock.notify(); // Notify the other thread to start
                    } else {
                        try {
                            lock.wait(); // Wait for the other thread to notify
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }
}
