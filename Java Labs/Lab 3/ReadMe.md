# House Building Simulation

## Overview
This Java program simulates a house construction process with multiple workers, showcasing multithreading and synchronization using a Cyclic Barrier. The workers represent threads working through different construction stages concurrently.

## Table of Contents
- [House Building Simulation](#house-building-simulation)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Classes](#classes)
    - [1. Worker](#1-worker)
    - [2. HouseBuildingSimulation](#2-housebuildingsimulation)
  - [Features](#features)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Usage](#usage)
  - [Sample Output](#sample-output)
  - [Important Notes](#important-notes)

## Classes

### 1. Worker
- **Responsibility:** Represents an individual construction worker thread.
- **Key Components:**
  - Cyclic Barrier for synchronization among workers.
  - Randomized work time simulation for each construction stage.
- **Methods:**
  - `run()`: Simulates the worker's activities, including starting and completing construction stages.

### 2. HouseBuildingSimulation
- **Responsibility:** Main class to run the house building simulation.
- **Key Components:**
  - User input for the number of worker threads.
  - Cyclic Barrier for synchronization among workers.
- **Methods:**
  - `getUserInput(String message)`: Helper method to get user input for the number of worker threads.

## Features

- **Multithreading:** Utilizes Java threads to simulate concurrent work by construction workers.
- **Console Input Validation**: The code expects the user to input the number of worker threads. It will prompt you to enter a valid number if an invalid input is provided.
- **Cyclic Barrier:** Workers synchronize at each construction stage using a Cyclic Barrier, ensuring collective completion before moving to the next stage.
- **Randomized Work Time:** Introduces variability to the simulation by assigning random time durations (between 500 to 1000 milliseconds) for each construction stage.
- **Output Format**: The output will display the start time, completion time, and time taken for each worker thread at each stage.

## Getting Started

### Prerequisites

- Java Development Kit (JDK)

### Usage

1. **Compile the Java Program:**
   ```bash
   javac HouseBuildingSimulation.java
   ```

2. **Run the Compiled Program:**
   ```bash
   java HouseBuildingSimulation
   ```

3. **Enter the Number of Worker Threads:**
   Follow the on-screen prompt to input the desired number of worker threads.

4. **Observe the Simulation Output:**
   The program will display the start and completion times for each worker at each construction stage.

## Sample Output

```
Enter the number of worker threads: 2
[HH:mm:ss:ms] Worker 1  started Stage 1
[HH:mm:ss:ms] Worker 2  started Stage 1
All workers completed the current stage. Proceeding to the next stage (if any)...
[HH:mm:ss:ms] Worker 2  completed Stage 1 in 700 milliseconds
[HH:mm:ss:ms] Worker 1  completed Stage 1 in 800 milliseconds
[HH:mm:ss:ms] Worker 1  started Stage 2
[HH:mm:ss:ms] Worker 2  started Stage 2
All workers completed the current stage. Proceeding to the next stage (if any)...
.
.
.
```

## Important Notes
- The program uses a Cyclic Barrier to synchronize workers at the end of each construction stage.
- User input is required to specify the number of worker threads for the simulation.


<!-- ## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details. -->