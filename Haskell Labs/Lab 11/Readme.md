# Coin Change Problem Solver in Haskell

## Table of Contents
- [Coin Change Problem Solver in Haskell](#coin-change-problem-solver-in-haskell)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Commands to Run](#commands-to-run)
  - [Input](#input)
  - [Output](#output)
  - [Example](#example)
  - [Implementation Details](#implementation-details)
- [Stock Buy and Sell Problem Solver in Haskell](#stock-buy-and-sell-problem-solver-in-haskell)
  - [Overview](#overview-1)
  - [Commands to Run](#commands-to-run-1)
  - [Input](#input-1)
  - [Output](#output-1)
  - [Example](#example-1)
  - [Implementation Details](#implementation-details-1)

## Overview

This Haskell program solves the Coin Change problem, which involves finding the minimum number of coins needed to make up the given amount using a set of coin denominations.

## Commands to Run

1. Navigate to the directory containing the code file (CoinChangeProblem.hs) and compile the code using GHC:
    ```
    ghc CoinChangeProblem.hs
    ```

2. Run the executable file by executing the following command:
    ```
    ./CoinChangeProblem
    ```

## Input

- The program prompts the user to provide input in the following format:
    1. Enter coin denominations separated by spaces.
    2. Enter the amount of money to be made up.

## Output

- The program outputs the minimum number of coins needed to make up the amount using the given denominations. If it's not possible to make up the amount, it returns -1.

## Example

Suppose you want to make up an amount of 11 from coins [1, 2, 5], user inputs and output are:

```
Enter coin denominations (separated by spaces): 
1 2 5
Enter the amount: 
11
Min number of coins needed: 3
```

## Implementation Details

- The program utilizes dynamic programming to efficiently compute the minimum number of coins needed for each amount.
- It handles cases where the amount cannot be made up using any combination of the given denominations set by returning -1.
- Input is taken from the user interactively from the console/terminal, and the output is displayed accordingly.

# Stock Buy and Sell Problem Solver in Haskell

## Overview

This Haskell program solves the Stock Buy and Sell problem, which involves finding the maximum profit achievable by completing at most k transactions, given an array of stock prices for each day.

## Commands to Run

1. Navigate to the directory containing the code file (StockBuynSell.hs) and compile the code using GHC:
    ```
    ghc StockBuynSell.hs
    ```

2. Run the executable file by executing the following command:
    ```
    ./StockBuynSell
    ```

## Input

- The program prompts the user to provide input in the following format:
    1. Enter prices for stocks on each day separated by spaces.
    2. Enter the transactions limit (k).

## Output

- The program outputs an integer representing the maximum profit achievable by completing at most k transactions.

## Example

Suppose you want to get the max profit for stock prices [3, 2, 6, 5, 0, 3] with at most 2 transactions, user inputs and output are:

```
Enter prices of stock each day (separated by spaces): 
3 2 6 5 0 3
Enter the transactions limit: 
2
Maximum Profit achieved: 7
```

## Implementation Details

- The program utilizes dynamic programming to efficiently compute the maximum profit achievable by completing at most `k` transactions.
- The code handles cases where `k` is 0 by returning 0, indicating that no transactions are possible. It also ensures that the input list of prices is non-empty and that `k` is a non-negative integer.
- Input is taken in the form of an integer array `prices` and an integer `k`. The program computes the maximum profit achievable and displays it as the output.