import java.util.Random;
import java.util.Scanner;

public class Q5 {
    static int size;        // Size of the square matrices
    static int[][] matA;    // Matrix A
    static int[][] matB;    // Matrix B
    static int[][] matC;    // Resultant Matrix C
    static int step_i = 0;  // Counter for each thread's assigned row

    static class Worker implements Runnable {
        int i;

        Worker(int i) {
            this.i = i;
        }

        @Override
        public void run() {
            // Each thread computes a portion of the result matrix
            for (int j = 0; j < size; j++) {
                for (int k = 0; k < size; k++) {
                    matC[i][j] += matA[i][k] * matB[k][j];
                }
            }
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter the size of square matrix: ");
        size = scanner.nextInt();

        matA = new int[size][size];
        matB = new int[size][size];
        matC = new int[size][size];

        Random rand = new Random();

        // Generating random values in matA and matB
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matA[i][j] = rand.nextInt(10);
                matB[i][j] = rand.nextInt(10);
            }
        }

        // Displaying matA
        System.out.println("Matrix A");
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                System.out.print(matA[i][j] + " ");
            }
            System.out.println();
        }

        // Displaying matB
        System.out.println("Matrix B");
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                System.out.print(matB[i][j] + " ");
            }
            System.out.println();
        }

        // Declaring and creating threads
        Thread[] threads = new Thread[size];

        // Creating threads, each responsible for a portion of the computation
        for (int i = 0; i < size; i++) {
            threads[i] = new Thread(new Worker(step_i++));
            threads[i].start();
        }

        // Joining and waiting for all threads to complete
        for (int i = 0; i < size; i++) {
            try {
                threads[i].join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Displaying the result matrix
        System.out.println("Multiplication of A and B");
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                System.out.print(matC[i][j] + " ");
            }
            System.out.println();
        }

        scanner.close();
    }
}
