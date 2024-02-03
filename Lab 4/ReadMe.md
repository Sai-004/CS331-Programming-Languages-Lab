# Multithreaded Database Concurrency Control

This Java project demonstrates a simple multithreaded database concurrency control scenario. It includes classes representing Users, Orders, and operations on a database.

## Table of Contents
- [Multithreaded Database Concurrency Control](#multithreaded-database-concurrency-control)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Database Schema](#database-schema)
    - [User](#user)
    - [Order](#order)
  - [Implementation Details](#implementation-details)
    - [Database Initialization](#database-initialization)
    - [Database Operations](#database-operations)
    - [Concurrency Control](#concurrency-control)
    - [Multithreaded Transactions](#multithreaded-transactions)
  - [Usage](#usage)

## Introduction

This Java program serves as a simulation of a multithreaded database system with a focus on implementing a basic form of concurrency control where concurrent transactions (reads and writes) may occur, ensuring the correctness and consistency of the database.

## Database Schema

### User
- `userID` (`int`)
- `userName` (`String`)
- `email` (`String`)

### Order
- `orderID` (`int`)
- `userID` (`int`, foreign key referencing `users.userID`)
- `product` (`String`)
- `quantity` (`int`)

## Implementation Details

### Database Initialization

The `DatabaseInitializer` class provides methods to initialize the Users and Orders tables with sample data, ensuring a meaningful starting point for the database.

```java
    DatabaseInitializer.initializeUsers();
    DatabaseInitializer.initializeOrders();
```

### Database Operations

The `DatabaseOperations` class includes methods for read and write operations on the database. Utilizing a `ReentrantLock` for thread synchronization, this class guarantees safe and consistent access to shared resources.

### Concurrency Control

The `Database` class manages the overall database system. The choice of a `ReentrantLock` for concurrency control ensures that transactions occur in a consistent and thread-safe manner, preventing data corruption during concurrent operations.

### Multithreaded Transactions

The `DatabaseTransaction` class represents an individual transaction, where each instance performs a series of read and/or write operations on the database.

## Usage

1. Compile and run the Java program:

   ```bash
   javac databaseControl.java
   java databaseControl
   ```

2. View the console output to observe the multithreaded database transactions.