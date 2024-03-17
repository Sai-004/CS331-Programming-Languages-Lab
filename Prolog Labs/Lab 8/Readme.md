Generating all possible combinations of K unique items from a list of N elements in Prolog

## Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Commands to Run](#commands-to-run)
- [Usage](#usage)
- [Input](#input)
- [Output](#output)
- [Implementation Details](#implementation-details)

## Introduction

This Prolog program generates all possible combinations of size K from a given list of N elements. It achieves this through the `combination/3` predicate, which recursively constructs combinations by selecting elements from the input list and appending them to the resulting combination list.

## Commands to Run

```prolog
swipl -s backtracking.pl
```

## Usage

> To generate combinations of size K from a list:

```prolog
combination(K, [p,q,s,r,t], L).
```

**Note:** Press `;` to get more results.

## Input

- K: The size of the combinations to generate.
- List: The input list from which combinations are to be generated.

## Output

The output will be a series of lists, each containing a unique combination of K elements from the input list.

## Implementation Details

- The `combination/3` predicate recursively selects elements from the input list and constructs combinations of size K.
- It uses the `list_tail/2` predicate to select remaining elements (excluding the current element) from the input list.
- Base cases for the `combination/3` predicate handle scenarios where K or the input list is empty. Additionally, it handles the base case of the `list_tail/2` predicate where the size of the list is 1.