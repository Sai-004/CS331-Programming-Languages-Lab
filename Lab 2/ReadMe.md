# Library Simulation Readme

## Overview
This Java code simulates a basic library system with reading rooms and bookshelves, providing a multi-threaded environment to mimic the activities of readers in a controlled setting. The simulation employs semaphores to manage access to reading rooms and bookshelves, ensuring that the library operates efficiently and within capacity limits.

## Table of Contents
- [Library Simulation Readme](#library-simulation-readme)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Classes](#classes)
    - [Library](#library)
    - [ReadingRoom](#readingroom)
    - [Bookshelf](#bookshelf)
    - [Reader](#reader)
    - [LibrarySimulation](#librarysimulation)
  - [How to Run](#how-to-run)
  - [Assumptions](#assumptions)
  - [Important Notes](#important-notes)


## Classes

### Library
- **Responsibility:** Manages the entire library system, including reading rooms and bookshelves.
- **Key Components:**
  - Semaphores to control access to reading rooms and bookshelves.
  - Lists to store instances of ReadingRoom and Bookshelf.
- **Methods:**
  - `getReadingRoom()`: Acquires a reading room, allowing a reader to enter.
  - `returnReadingRoom(ReadingRoom readingRoom)`: Returns a reading room, freeing up space.
  - `getBookshelf()`: Acquires a bookshelf, allowing a reader to borrow a book.
  - `returnBookshelf(Bookshelf bookshelf)`: Returns a bookshelf, indicating the book has been returned.

### ReadingRoom
- **Responsibility:** Represents a reading room in the library.
- **Key Components:**
  - Synchronization to control access and limit the number of readers inside.
- **Methods:**
  - `tryEnter()`: Tries to enter the reading room, returns true if successful.
  - `leave()`: Leaves the reading room, notifying waiting readers that a slot is available.
  - `getCurrentReaders()`: Returns the current number of readers in the reading room.

### Bookshelf
- **Responsibility:** Represents a bookshelf in the library.
- **Key Components:**
  - Semaphore to control access and limit the number of readers borrowing books.
- **Methods:**
  - `tryEnter()`: Tries to enter the bookshelf, returns true if successful.
  - `leave()`: Leaves the bookshelf, indicating the book has been returned.

### Reader
- **Responsibility:** Represents a reader thread in the library.
- **Methods:**
  - `run()`: Simulates a reader's activities in the library, including entering reading rooms, borrowing books, and returning to the library.

### LibrarySimulation
- **Responsibility:** Main class to run the library simulation.
- **Key Components:**
  - User input for the number of reading rooms, bookshelves, and maximum readers per reading room.
  - Continuous loop for creating and starting reader threads.

## How to Run
1. **Compile the Code:**
   ```
   javac LibrarySimulation.java
   ```

2. **Run the Simulation:**
   ```
   java LibrarySimulation
   ```

3. **Follow Prompts:**
   - Enter the number of reading rooms, bookshelves, and maximum readers per reading room when prompted.

## Assumptions
In addition to the specified requirements, the following assumptions were made during the implementation:
- **Random Assignment of Reading Rooms**: Readers are assigned to a reading room which is free upon entry.
- **Infinite Simulation**: The program runs indefinitely, creating new readers and simulating book borrowing in a continuous loop.
- **Simulated Reading**: The program simulates reading by having readers sleep for a fixed duration to represent the time spent reading a book.
- **Basic Output**: The program outputs basic information about the actions of readers, such as entering a reading room, borrowing a book, and returning to the library.
- **Basic Input**: The program takes in the input of no.of reading room, book shelfs, and max no.of readers per reading room.
- **Simulaion**: The Program is being simulated for 10 readers entering the library. 

## Important Notes
- The simulation runs indefinitely, continuously creating and starting new reader threads.
- The sleep duration in the `Reader` class's `run` method simulates the time spent reading a book.

<!-- 
## Suggestions for Improvement
- Implement a graceful stop mechanism for the simulation.
- Enhance logging and output for a more comprehensive view of simulation activities.
- Consider adding additional features or constraints to make the simulation more realistic. -->