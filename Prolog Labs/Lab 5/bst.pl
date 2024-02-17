% tree(Value, LeftSubtree, RightSubtree)

% Check if a tree is empty
empty(empty).

% Check if a tree is a binary search tree
is_bst(Tree) :-
    is_bst(Tree, -inf, inf).

% Helper predicate to check if a tree is a binary search tree with given minimum and maximum values
is_bst(empty, _, _).
is_bst(tree(Value, Left, Right), Min, Max) :-
    % Value should be greater than Min and less than Max
    Value > Min,
    Value < Max,
    % Check if left subtree is BST with values less than current node
    is_bst(Left, Min, Value),
    % Check if right subtree is BST with values greater than current node
    is_bst(Right, Value, Max).

% Predicate to check if all nodes in a subtree are less than a given value
is_subtree_less(empty, _).
is_subtree_less(tree(Value, Left, Right), Max) :-
    Value < Max,
    is_subtree_less(Left, Max),
    is_subtree_less(Right, Max).

% Predicate to check if all nodes in a subtree are greater than a given value
is_subtree_greater(empty, _).
is_subtree_greater(tree(Value, Left, Right), Min) :-
    Value > Min,
    is_subtree_greater(Left, Min),
    is_subtree_greater(Right, Min).



% Test cases:-

% This is a BST
example_bst_tree :-
    Tree = tree(5,
                tree(3, tree(1, empty, empty), tree(4, empty, empty)),
                tree(7, tree(6, empty, empty), tree(8, empty, empty))),
    is_bst(Tree).

% This is not a BST (2 on the left is greater than 1)
example_non_bst_tree :-
    Tree = tree(5,
                tree(3, tree(1, empty, empty), tree(2, empty, empty)),
                tree(7, tree(6, empty, empty), tree(8, empty, empty))),
    is_bst(Tree).
