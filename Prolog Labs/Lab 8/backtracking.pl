list_tail(X,X).
list_tail([_|Xs],T):- 
    list_tail(Xs,T).

combination(0,_,[]).
combination(N,List,[X|Result]):-
    N > 0, 
    N1 is N-1,
    list_tail(List,[X|Tail]),
    combination(N1,Tail,Result).

% Note
:- writeln("!!!Give input in the format combination(3,[p,q,s,r,t],L). and keep pressing ';' to get the combinations.!!!").
