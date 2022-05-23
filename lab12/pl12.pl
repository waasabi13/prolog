%11 найти количество чисел, взаимно простых с заданным
nod(A,0,A):-!.
nod(A,B,X):-C is A mod B,nod(B,C,X).

eulerD(A,X):-eulerD(A,X,A,0).
eulerD(_,X,0,X):-!.
eulerD(A,X,I,R):-I1 is I-1, nod(A,I1,NOD),NOD is 1,!,R1 is R+1,eulerD(A,X,I1,R1).
eulerD(A,X,I,R):-I1 is I-1,eulerD(A,X,I1,R).

eulerU(A,X):-eulerU(A,X,A).
eulerU(_,0,0):-!.
eulerU(A,X,R):-R1 is R-1,nod(A,R,NOD),NOD is 1,eulerU(A,X1,R1),X is X1+1,!.
eulerU(A,X,R):-R1 is R-1,eulerU(A,X1,R1),X is X1.

%12 Найти делитель числа, являющийся взаимно простым с наибольшим
% количество цифр данного числа.

delNum(A,X):- delNum(X,A,A,0,0).

delNum(X,A,1,CMD,MD):-X is MD,!.
delNum(X,A,I,CMD,MD):-
    I1 is I-1,
    (
        0 is (A mod I),
        codwsc(COUNT,A,I),
        (
            COUNT >= CMD,
            delNum(X,A,I1,COUNT,I);
            delNum(X,A,I1,CMD,MD)
        );
        delNum(X,A,I1,CMD,MD)
    ),
    !.

codwsc(COUNT,0,DEL):- COUNT is 0,!.
codwsc(COUNT,NUM,DEL):-
    NUM1 is NUM div 10,
    (
        nod(NUM mod 10,DEL,D),
        D is 1,
        codwsc(COUNT,NUM1,DEL),
        COUNT is COUNT+1;
        codwsc(COUNT,NUM1,DEL)
    ),
    !.
%13 воспользуюсь минус заданием
%предикаты для работы со списком
write_list([]):-!.
write_list([Head|Tail]):-write(Head), write(" "), write_list(Tail).

read_list(0, []):-!.
read_list(N, [X|T]):- read(X), N1 is N-1, read_list(N1, T).
%14 длина списка

list_length([H|T], Length):-list_length([H|T], 0, Length).
list_length([], Length, Length):-!.
list_length([_|T], CurLength, Length):- CurN1 is CurLength+1, list_length(T, CurN1, Length).

%15
getMaxInd([H|T],MaxInd):-getMaxInd([H|T],H,0,0,MaxInd).
getMaxInd([],_,Answer,_,Answer):-!.
getMaxInd([H|T],Max,_,CurInd,Answer):-CurInd1 is CurInd+1,H>Max,NewMaxInd is CurInd,NewMax is H,getMaxInd(T,NewMax,NewMaxInd,CurInd1,Answer),!.
getMaxInd([_|T],Max,MaxInd,CurInd,Answer):-CurInd1 is CurInd+1,getMaxInd(T,Max,MaxInd,CurInd1,Answer).

task15:-write("Dlina spiska: "), read(Count),readList(Count,List),
    write("Kolvo elementov posle max: "),
    list_length(List,Len),getMaxInd(List,IndMax),
    X is Len-IndMax-1, write(X),!.
%17
countElemBetweenAB([],A,B,Count):-Count is 0,!.
countElemBetweenAB([H|T],A,B,Count):-
    (
        H<B,
        H>A,
        countElemBetweenAB(T,A,B,Count1),
        Count is Count1+1;
        countElemBetweenAB(T,A,B,Count)
    ).

task7:- read(N),read_list(N,List),read(A),read(B),countElemBetweenAB(List,A,B,Count),write(Count),!.



