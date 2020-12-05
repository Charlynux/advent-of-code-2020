is_empty_string(S) :-
    S == "".

read_input(Filename, Values) :-
    read_file_to_string(Filename, String, []),
    split_string(String, "\n", "", Lines),
    exclude(is_empty_string, Lines, FixedLines),
    maplist(number_string, Values, FixedLines).

are_2020(A, B) :-
    plus(A, B, Result),
    Result == 2020.

solve_part_1(Input, Result) :-
    read_input(Input, Vals),
    member(A, Vals),
    member(B, Vals),
    are_2020(A, B),
    Result is A * B.

%?- solve_part_1("/Users/charles/git-repositories/advent-of-code-2020/day01/input", Result).
%@ Result = 618144 .


are_2020(A, B, C) :-
    plus(A, B, Tmp),
    plus(Tmp, C, Result),
    Result == 2020.

solve_part_2(Input, Result) :-
    read_input(Input, Vals),
    member(A, Vals),
    member(B, Vals),
    member(C, Vals),
    are_2020(A, B, C),
    Result is A * B * C.

%?- solve_part_2("/Users/charles/git-repositories/advent-of-code-2020/day01/input", Result).
%@ Result = 173538720.
% PRETTY LONG !!!
