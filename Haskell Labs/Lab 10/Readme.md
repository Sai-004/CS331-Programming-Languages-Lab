# 4-Digit Numbers Divisible by 3 in Haskell

## Table of Contents
- [4-Digit Numbers Divisible by 3 in Haskell](#4-digit-numbers-divisible-by-3-in-haskell)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Commands to Run](#commands-to-run)
  - [Input](#input)
  - [Output](#output)
  - [Example](#example)
  - [Implementation Details](#implementation-details)

## Overview

This Haskell program aims to determine the count of 4-digit positive integers that are divisible by 3 and can be formed using only the digits {1, 3, 4, 6, 7}, ensuring each digit appears exactly once.

## Commands to Run

1. Navigate to the directory containing the code file (4DigitDivisibleBy3.hs) and compile the code using GHC:
    ```
    ghc 4DigitDivisibleBy3.hs
    ```

2. Run the executable file by executing the following command:
    ```
    ./4DigitDivisibleBy3
    ```

## Input

- The program doesn't require any user input. It internally generates all valid permutations of the given digits {1, 3, 4, 6, 7} and counts those forming 4-digit numbers divisible by 3.

## Output

- The program outputs the count of 4-digit positive integers divisible by 3, formed using the digits {1, 3, 4, 6, 7} without repetition.

## Example

Suppose you run the program and execute the steps as instructed. The output might look like:
```
Number of 4-digit positive integers divisible by 3: 48
```
This indicates that there are 48 4-digit positive integers that meet the specified criteria.

## Implementation Details

- The program generates all valid permutations of the digits {1, 3, 4, 6, 7} ensuring no repetition.
- It filters out permutations forming 4-digit numbers and checks their divisibility by 3.
- The implementation ensures correctness and efficiency using Haskell's functional programming features.