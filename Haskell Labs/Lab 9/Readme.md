# Longest Common Prefix in Haskell

## Table of Contents
- [Table of Contents](#table-of-contents)
- [Overview](#overview)
- [Commands to Run](#commands-to-run)
- [Input](#input)
- [Output](#output)
- [Example](#example)
- [Implementation Details](#implementation-details)

## Overview

This Haskell program implements a function to find the longest common prefix among a list of strings. If no common prefix is found, it returns an empty string.

## Commands to Run

1. Navigate to the directory containing the LongestCommonPrefix.hs file and compile the code:
    ```
    ghc LongestCommonPrefix.hs
    ```
   - This command will generate an executable file named LongestCommonPrefix.

2. Run the executable file by executing the following command:
    ```
    ./LongestCommonPrefix
    ```
   
## Input

- The program takes the number of strings 'n' and a list of 'n' strings as input.

## Output

- The program outputs the longest common prefix among the provided list of strings.

## Example

Suppose you have the following list of strings:
```
3
["hello", "hellbound", "hellBoy123"]
```
- Enter the strings line by line as instructed by terminal prompts.

Running the program will output:
```
hell
```
This is because "hell" is the longest common prefix among the provided strings.

## Implementation Details

- The program utilizes recursive pattern matching to find the longest common prefix among strings.
- Base cases are defined to handle scenarios where no common prefix exists or when the list is empty.