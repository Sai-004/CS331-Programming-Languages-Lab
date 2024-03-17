Grouping List into Sublists of Specified Sizes in Prolog

## Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Commands to Run](#commands-to-run)
- [Usage](#usage)
- [Input](#input)
- [Output](#output)
- [Implementation Details](#implementation-details)

## Introduction

This Prolog program divides a given list of 5 elements into 3 sublists, where the 1st sublist (G1) contains 1 element, the 2nd sublist (G2) contains 2 elements, and the 3rd sublist (G3) also contains 2 elements. It achieves this through the `group3/4` predicate, utilizing backtracking and list manipulation in Prolog.

## Commands to Run

> Load the Prolog file containing the code
```prolog
swipl -s backtracking.pl
```

> Use all the groupings using the `group3/4`
```prolog
?- find_all_groupings([1, 2, 3, 4, 5], Groupings), print_groupings(Groupings).
```

## Usage

1. **Loading the Program**: Use the `swipl` command followed by the file name containing the Prolog code (`backtracking.pl`).
   
2. **Finding and Printing Groupings**: Use the `find_all_groupings/2` predicate to generate all possible groupings of the input list, and then use `print_groupings/1` to print these groupings.

## Input

The input list can be customized by replacing `[1, 2, 3, 4, 5]` with any list containing five elements.

## Output

The output will display all possible groupings of the input list, where each grouping consists of three sublists (G1, G2, and G3) such that G1 contains 1 element, G2 contains 2 elements, and G3 contains 2 elements.

## Implementation Details

- The `group3/4` predicate is responsible for generating the desired groupings.
- It utilizes the `select/3` predicate to choose elements from the input list based on the specified sizes.
- The `el/3` predicate serves as a helper to select elements from a list.
- Base cases for the `select/3` predicate handle scenarios where the number of elements to select is zero.