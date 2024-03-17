% Grouping the list G of 5 elements into G1, G2, and G3, such that G1, G2, and G3 contain 1, 2, and 2 elements respectively.
group3(G, G1, G2, G3) :-
    select(1, G, G1),         % Select 1 element into G1
    subtract(G, G1, R1),
    select(2, R1, G2),        % Select 2 elements from R1 into G2
    subtract(R1, G2, R2),
    select(2, R2, G3),        % Select 2 elements from R2 into G3
    subtract(R2, G3, []).     % Ensure there are no remaining elements

% Base case: n is 0, resulting in an empty list.
select(0, _, []) :- !.
% Select n elements from list L, resulting in list S with elements selected in X.
select(N, L, [X|S]) :-
    N > 0,
    el(X, L, R),             % select X from list L, resulting in remaining list R
    N1 is N - 1,
    select(N1, R, S).        % remaining n-1 elements from R, resulting in S.

% Predicate to select X from a list (head).
el(X, [X|L], L).
% Predicate to select X from a list (not head).
el(X, [_|L], R) :-
    el(X, L, R).

% Find all possible groupings at once
find_all_groupings(G, Groupings) :-
    bagof((G1, G2, G3), group3(G, G1, G2, G3), Groupings).

% Print each set [G1, G2, G3] in the specified format
print_groupings([]).
print_groupings([(G1, G2, G3)|Rest]) :-
    format('[G1,G2,G3] = [~w,~w,~w]~n', [G1, G2, G3]),
    print_groupings(Rest).
