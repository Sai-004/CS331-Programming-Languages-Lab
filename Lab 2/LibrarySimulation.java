import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.Semaphore;

// Represents a Library with reading rooms and bookshelves
class Library {
    private List<ReadingRoom> readingRooms;
    private List<Bookshelf> bookshelves;

    private Semaphore readingRoomSemaphore;
    private Semaphore bookshelfSemaphore;

    // Constructor initializes semaphores, reading rooms, and bookshelves
    public Library(int maxReadingRooms, int maxBookshelves, int maxReadersPerRoom) {
        readingRoomSemaphore = new Semaphore(maxReadingRooms, true);
        bookshelfSemaphore = new Semaphore(maxBookshelves, true);

        readingRooms = new ArrayList<>();
        bookshelves = new ArrayList<>();

        // Create reading rooms
        for (int i = 0; i < maxReadingRooms; i++) {
            readingRooms.add(new ReadingRoom("Reading Room " + (i + 1), maxReadersPerRoom));
        }

        // Create bookshelves
        for (int i = 0; i < maxBookshelves; i++) {
            bookshelves.add(new Bookshelf("Bookshelf " + (i + 1)));
        }
    }

    // Acquires a reading room, allowing a reader to enter
    public ReadingRoom getReadingRoom() {
        try {
            readingRoomSemaphore.acquire();
            return readingRooms.stream()
                    .filter(room -> room.tryEnter())
                    .findFirst()
                    .orElse(null);
        } catch (InterruptedException e) {
            e.printStackTrace();
            return null;
        }
    }

    // Returns a reading room, freeing up the space
    public void returnReadingRoom(ReadingRoom readingRoom) {
        readingRoom.leave();
        readingRoomSemaphore.release();
    }

    // Acquires a bookshelf, allowing a reader to borrow a book
    public Bookshelf getBookshelf() {
        try {
            bookshelfSemaphore.acquire();
            return bookshelves.stream()
                    .filter(shelf -> shelf.tryEnter())
                    .findFirst()
                    .orElse(null);
        } catch (InterruptedException e) {
            e.printStackTrace();
            return null;
        }
    }

    // Returns a bookshelf, indicating the book is returned
    public void returnBookshelf(Bookshelf bookshelf) {
        bookshelf.leave();
        bookshelfSemaphore.release();
    }
}

// Represents a reading room in the library
class ReadingRoom {
    private String name;
    private int maxReaders;
    private int currentReaders;
    private Object lock;

    // Constructor initializes reading room with a name and maximum readers
    public ReadingRoom(String name, int maxReaders) {
        this.name = name;
        this.maxReaders = maxReaders;
        this.currentReaders = 0;
        this.lock = new Object();
    }

    // Getter for reading room name
    public String getName() {
        return name;
    }

    // Tries to enter the reading room, returns true if successful
    public boolean tryEnter() {
        synchronized (lock) {
            if (currentReaders < maxReaders) {
                currentReaders++;
                return true;
            }
            return false;
        }
    }

    // Leaves the reading room, notifying waiting readers that a slot is available
    public void leave() {
        synchronized (lock) {
            currentReaders--;
            lock.notify(); // Notify waiting readers that a slot is available
        }
    }

    // Getter for the current number of readers in the reading room
    public int getCurrentReaders() {
        return currentReaders;
    }
}

// Represents a bookshelf in the library
class Bookshelf {
    private String name;
    private Semaphore shelfSemaphore;

    // Constructor initializes a bookshelf with a name and semaphore
    public Bookshelf(String name) {
        this.name = name;
        this.shelfSemaphore = new Semaphore(1, true);
    }

    // Getter for bookshelf name
    public String getName() {
        return name;
    }

    // Tries to enter the bookshelf, returns true if successful
    public boolean tryEnter() {
        return shelfSemaphore.tryAcquire();
    }

    // Leaves the bookshelf, indicating the book is returned
    public void leave() {
        shelfSemaphore.release();
    }
}

// Represents a reader thread in the library
class Reader extends Thread {
    private String name;
    private Library library;

    // Constructor initializes a reader with a name and library reference
    public Reader(String name, Library library) {
        this.name = name;
        this.library = library;
    }

    // Run method simulates a reader's activities in the library
    @Override
    public void run() {
        ReadingRoom readingRoom = library.getReadingRoom();
        if (readingRoom == null) {
            System.out.println(name + " is waiting for a reading room.");
            return;
        }

        System.out.println(
                name + " entered " + readingRoom.getName() + ".\nCurrent readers : " + readingRoom.getCurrentReaders());

        Bookshelf bookshelf = library.getBookshelf();
        if (bookshelf == null) {
            library.returnReadingRoom(readingRoom);
            System.out.println(name + " is waiting for a bookshelf.");
            return;
        }

        System.out.println(name + " is borrowing a book from " + bookshelf.getName());

        // Simulate reading
        try {
            sleep(2000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        library.returnReadingRoom(readingRoom);
        library.returnBookshelf(bookshelf);

        System.out.println(name + " returned to the library.\nCurrent readers in " + readingRoom.getName() + ": "
                + readingRoom.getCurrentReaders());
    }
}

// Main class to run the library simulation
public class LibrarySimulation {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Take input for the number of reading rooms
        System.out.print("Enter the number of reading rooms: ");
        int numReadingRooms = scanner.nextInt();

        // Take input for the number of bookshelves
        System.out.print("Enter the number of bookshelves: ");
        int numBookshelves = scanner.nextInt();

        // Take input for the maximum number of readers per reading room
        System.out.print("Enter the maximum number of readers per reading room: ");
        int maxReadersPerRoom = scanner.nextInt();

        scanner.close();
        Library library = new Library(numReadingRooms, numBookshelves, maxReadersPerRoom);

        // Create and start 10 reader threads
        while (true) {

            for (int i = 1; i <= 10; i++) {
                new Reader("Reader " + i, library).start();
            }
        }

    }
}
