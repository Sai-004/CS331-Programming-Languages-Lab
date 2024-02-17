Binary Search Tree Checker in Prolog

## Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Commands to Run](#commands-to-run)
- [Usage](#usage)
  - [Predefined Test Cases](#predefined-test-cases)
  - [Custom Test Cases](#custom-test-cases)
- [Input](#input)
- [Output](#output)

## Introduction

This Prolog program checks whether a given binary tree is a Binary Search Tree (BST). 
A BST is a binary tree where the left subtree of each node contains only nodes with values less than the node's value and the right subtree contains only nodes with values greater than the node's value.

## Commands to Run

```bash
swipl -s bst.pl
```

## Usage

### Predefined Test Cases

To check if a predefined tree is a BST or not:

1. Example BST Tree:
```prolog
example_bst_tree.   
```
Output: `true`

2. Example Non-BST Tree:
```prolog
example_non_bst_tree. 
```
Output: `false`

### Custom Test Cases

Alternatively, you can define your own tree structure and directly use the `is_bst/1` predicate:

```prolog
is_bst(tree(5,
            tree(3, tree(1, empty, empty), tree(4, empty, empty)),
            tree(7, tree(6, empty, empty), tree(8, empty, empty)))).
```

## Input

The input to the `is_bst/1` predicate can be either a predefined tree structure (like in the provided examples `example_bst_tree` and `example_non_bst_tree`) or a tree structure directly passed to the `is_bst/1` predicate.

## Output

This will return `true`, indicating that the binary tree is a BST. Else `false`, indicating that the binary tree is not a BST.