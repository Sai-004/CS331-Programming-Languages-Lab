% Predicate to partition the list into red, white, and blue elements
partition([], [], [], []).

partition([red|T], [red|Reds_list], Whites_list, Blues_list) :- 
    partition(T, Reds_list, Whites_list, Blues_list).

partition([white|T], Reds_list, [white|Whites_list], Blues_list) :- 
    partition(T, Reds_list, Whites_list, Blues_list).

partition([blue|T], Reds_list, Whites_list, [blue|Blues_list]) :- 
    partition(T, Reds_list, Whites_list, Blues_list).

% Predicate to group the partitioned lists
group(Reds, Whites, Blues, Sorted_list) :- 
    append(Reds, Whites, Temp_List),
    append(Temp_List, Blues, Sorted_list).

% Main predicate for Dutch Flag Sorting
dutch_flag_sort(List, Sorted_list) :-
    partition(List, Reds_list, Whites_list, Blues_list),
    group(Reds_list, Whites_list, Blues_list, Sorted_list).


% Example testcases:
% random testcase
testcase1(Sorted_list) :-
    dutch_flag_sort(['white', 'red', 'blue', 'red', 'white', 'blue'], Sorted_list). 
% Output: Sorted_list = [red, red, white, white, blue, blue].

% empty testcase
testcase2(Sorted_list) :-
    dutch_flag_sort([], Sorted_list).
% Output: Sorted_list = [].

% testcase with other than 'red','white','blue' as inputs
testcase3(Sorted_list) :-
    dutch_flag_sort(['grey', 'red', 'blue', 'red', 'white', 'blue'], Sorted_list).
% Output: false.

testcase4(Sorted_list) :-
    dutch_flag_sort(['blue', 'blue', 'blue', 'white', 'white', 'white', 'red', 'red', 'red'], Sorted_list).
% Output: Sorted_list = [red, red, red, white, white, white, blue, blue, blue].

testcase5(Sorted_list) :-
    dutch_flag_sort(['red', 'white', 'blue'], Sorted_list).
% Output: Sorted_list = [red, white, blue].