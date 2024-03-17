Dutch Flag Sorting in Prolog

## Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Commands to Run](#commands-to-run)
- [Usage](#usage)
  - [Predefined Test Cases](#predefined-test-cases)
  - [Custom Test Cases](#custom-test-cases)
- [Input](#input)
- [Output](#output)
- [Implementation Details](#implementation-details)

## Introduction

This Prolog program implements the Dutch Flag Sorting algorithm to sort a given list. The Dutch Flag Sorting algorithm rearranges elements in the list such that elements of one type come before elements of another type, following the order of "reds," "whites," and "blues."

## Commands to Run

```prolog
swipl -s DutchFlagSort.pl
```

## Usage

### Predefined Test Cases

To test the predefined test cases:

1. **Test Case 1:**
```prolog
testcase1(Sorted_list).
```
Output: `Sorted_list = [red, red, white, white, blue, blue]` (Example random testcase)

2. **Test Case 2:**
```prolog
testcase2(Sorted_list).
```
Output: `Sorted_list = []` (Example empty testcase)

3. **Test Case 3:**
```prolog
testcase3(Sorted_list).
```
Output: `false` (Testcase with wrong inputs)

### Custom Test Cases

To define and test your own test cases using the `dutch_flag_sort/2` predicate:

```prolog
dutch_flag_sort([//your input list], Sorted_list).
```

For example:
```prolog
dutch_flag_sort(['blue', 'blue', 'blue', 'white', 'white', 'red', 'red', 'red'], Sorted_list).
```

## Input

The input to the `dutch_flag_sort/2` predicate is a list containing elements that are to be sorted.

## Output

The output will be a sorted list following the Dutch Flag Sorting algorithm.

## Implementation Details

- The `partition/3` predicate divides the input list into three partitions: Reds_list, Whites_list, and Blues_list.
- The `group/3` predicate combines the partitions in the Dutch flag order using `append/3`.