import java.util.Arrays;
import java.util.Scanner;

public class Q4 {
    private int[] array;    // The array to be sorted
    private int[] temp;     // Temporary array used in the merge step

    // Method to initiate the sorting process
    public void sort(int[] inputArr) {
        this.array = inputArr;
        int len = inputArr.length;
        this.temp = new int[len];
        mergeSort(0, len - 1);  // Call the mergeSort method to start the sorting process
    }

    // Recursive method to perform merge sort on the array
    private void mergeSort(int l, int h) {
        if (l < h) {
            int mid = (l + h) / 2;

            // Create two separate threads to sort the left and right halves
            Thread leftThread = new Thread(() -> mergeSort(l, mid));
            Thread rightThread = new Thread(() -> mergeSort(mid + 1, h));

            // Start both threads
            leftThread.start();
            rightThread.start();

            try {
                // Wait for both threads to complete
                leftThread.join();
                rightThread.join();

                // Merge the sorted halves
                merge(l, mid, h);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    // Method to merge two halves of the array
    private void merge(int l, int mid, int h) {
        // Copy both halves into the temporary array
        System.arraycopy(array, l, temp, l, h - l + 1);

        int left = l;
        int right = mid + 1;
        int current = l;

        // Merge the two halves back into the original array
        while (left <= mid && right <= h) {
            if (temp[left] <= temp[right]) {
                array[current] = temp[left];
                left++;
            } else {
                array[current] = temp[right];
                right++;
            }
            current++;
        }

        // Copy the remaining elements of the left half (if any)
        while (left <= mid) {
            array[current] = temp[left];
            left++;
            current++;
        }

        // Copy the remaining elements of the right half (if any)
        while (right <= h) {
            array[current] = temp[right];
            right++;
            current++;
        }
    }

    // Method to create an array with random values
    private static int[] createArray(int arraySize, int maxInt) {
        int[] array = new int[arraySize];
        for (int i = 0; i < arraySize; i++) {
            array[i] = (int) (Math.random() * (maxInt + 1));
        }
        return array;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.print("Enter the array size: ");
        int arraySize = scanner.nextInt();

        System.out.print("Enter the maximum integer value for random numbers: ");
        int maxInt = scanner.nextInt();

        int[] array = createArray(arraySize, maxInt);
        System.out.println("Before sorting: " + Arrays.toString(array));

        Q4 sorter = new Q4();
        sorter.sort(array);
        System.out.println("After sorting: " + Arrays.toString(array));

        scanner.close();
    }
}
