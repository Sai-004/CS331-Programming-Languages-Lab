import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

// Worker class representing individual construction workers
class Worker implements Runnable {
    
    private final int NO_OF_STAGES = 5;     // Number of stages in the construction process    
    private final int workerId;             // Worker's unique identifier
    private final CyclicBarrier barrier;    // Cyclic barrier for synchronization

    // Constructor to initialize worker with an ID and cyclic barrier
    public Worker(int workerId, CyclicBarrier barrier) {
        this.workerId = workerId;
        this.barrier = barrier;
    }

    // Run method to simulate the construction worker's tasks
    @Override
    public void run() {
        try {
            // Loop through each construction stage
            for (int stage = 1; stage <= NO_OF_STAGES; stage++) {
                // Simulating time taken to complete the stage (between 500 to 1000 milliseconds)
                int timeTaken = new Random().nextInt(501) + 500;

                // Print the start time for the current stage
                System.out.printf("[%s] Worker %d \tstarted Stage %d\n", getCurrentTime(), workerId, stage);

                // Simulating work by sleeping for the specified time
                Thread.sleep(timeTaken);

                // Printing the completion time and time taken for the current stage
                System.out.printf("[%s] Worker %d \tcompleted Stage %d in %d milliseconds\n",
                        getCurrentTime(), workerId, stage, timeTaken);

                // Waiting for all workers to complete the current stage
                barrier.await();
            }
        } catch (InterruptedException | BrokenBarrierException e) {
            e.printStackTrace();
        }
    }

    // Method to get the current time in HH:mm:ss:ms format
    private String getCurrentTime() {
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss:ms");
        return sdf.format(new Date());
    }
}

// Main class for the House Building Simulation
public class HouseBuildingSimulation {
    public static void main(String[] args) {
        // Allowing user to input the number of worker threads
        int numWorkers = getUserInput("Enter the number of worker threads: ");

        // Creating a cyclic barrier with the number of worker threads
        CyclicBarrier barrier = new CyclicBarrier(numWorkers, () -> {
            System.out.printf("All workers completed the current stage. Proceeding to the next stage (if any)...\n");
        });

        // Starting worker threads
        for (int i = 1; i <= numWorkers; i++) {
            Thread workerThread = new Thread(new Worker(i, barrier));
            workerThread.start();
        }
    }

    // Helper method to get user input for the number of worker threads
    private static int getUserInput(String message) {
        System.out.print(message);
        try {
            return Integer.parseInt(System.console().readLine());
        } catch (NumberFormatException e) {
            System.out.printf("Invalid input. Please enter a valid number.\n");
            return getUserInput(message);
        }
    }
}
