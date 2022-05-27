list_length([H|T], N):-list_length([H|T], 0, N).
list_length([], N, N):-!.
list_length([_|T], CurN, N):- CurN1 is CurN+1, list_length(T, CurN1, N).

write_list([]):-!.
write_list([Head|Tail]):-write(Head), nl, write_list(Tail).

read_list(0, []):-!.
read_list(N, [X|T]):- read(X), N1 is N-1, read_list(N1, T).

rev(List, Result):-rev(List, [], Result).
rev([], Result, Result):-!.
rev([H|T],List1, Result):-rev(T, [H|List1], Result).

cut(List, N, M, Result):-M1 is M+1,cut(List, N, M1, 0, Result).
cut(List, N, M, N, Result):-list_length(List, Length),M1 is M-N,cut(List, N, M1, M1, Length,Result),!.
cut([_|T], N, M, CurIndex, Result):-CurIndex1 is CurIndex+1, cut(T, N, M, CurIndex1, Result).
cut(Result, _, _, M, M, Result):-!.
cut(List, N, M, DelIndex, Length, Result):-Length1 is Length-1, del_by_num(List, DelIndex, DelList), cut(DelList, N, M, DelIndex, Length1, Result),!.

in_list([El|_], El):-!.
in_list([_|T], El):-in_list(T, El).
%1
countElemBetweenAB([],_,_,Count):-Count is 0,!.
countElemBetweenAB([H|T],A,B,Count):-
    (
        H=<B,
        H>=A,
        countElemBetweenAB(T,A,B,Count1),
        Count is Count1+1;
        countElemBetweenAB(T,A,B,Count)
    ).

task1:- read(N),read_list(N,List),read(A),read(B),countElemBetweenAB(List,A,B,Count),write(Count),!.

%2 Дан массив чисел. Необходимо проверить, чередуются ли в нем це-
%лые и вещественные числа.

check(A):-IntA is round(A), IntA = A.


intfloat([H|T]):-check(H), intfloat(T, 1); intfloat(T, 0).
intfloat([],_):-!.
intfloat([H|T], 1):- not(check(H)),intfloat(T, 0),!;fail.
intfloat([H|T], 0):- check(H), intfloat(T, 1),!;fail.

task2:- read(Count), read_list(Count, List), intfloat(List).
