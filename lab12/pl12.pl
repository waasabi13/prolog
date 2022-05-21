%11 ����� ���������� �����, ������� ������� � ��������
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

%12 ����� �������� �����, ���������� ������� ������� � ����������
% ���������� ���� ������� �����.

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
%13 ������������ ����� ��������
%��������� ��� ������ �� �������
write_list([]):-!.
write_list([Head|Tail]):-write(Head), write(" "), write_list(Tail).

read_list(0, []):-!.
read_list(N, [X|T]):- read(X), N1 is N-1, read_list(N1, T).
%14 ����� ������

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

%16
getMinInd([H|T],MinInd):-getMinInd([H|T],H,0,0,MinInd).
getMinInd([],_,Answer,_,Answer):-!.
getMinInd([H|T],Min,_,CurInd,Answer):-CurInd1 is CurInd+1,H<Min,NewMinInd is CurInd,NewMin is H,getMaxInd(T,NewMin,NewMinInd,CurInd1,Answer),!.
getMinInd([_|T],Min,MinInd,CurInd,Answer):-CurInd1 is CurInd+1,getMinInd(T,Min,MinInd,CurInd1,Answer).

append1([],X,X).
append1([X|T],Y,[X|T1]):- append1(T,Y,T1).

take(List,A,B,X):-take(List,A,B,X,[],0).
take([],_,_,X,X,_):-!.
take([H|T],A,B,X,L,C):-C>A,C<B,!,C1 is C+1, append(L,[H],LL),take(T,A,B,X,LL,C1).
take([_|T],A,B,X,L,C):-C1 is C+1,take(T,A,B,X,L,C1).

rev([H|T],X):-rev([H|T],X,[]).
rev([],X,X):-!.
rev([H|T],X,L):-append([H],L,LL),rev(T,X,LL).

min(X,Y,X):-X<Y,!.
min(_,Y,Y).
max(X,Y,X):-X>Y,!.
max(_,Y,Y).


task16:- read(N),read_list(N,List),
    getMinInd(List,X),getMaxInd(List,Y),A is X+1,B is Y+1,min(A,B,C),max(A,B,D),
    take(List,-1,C,L),list_length(List,K),K1 is K+2,D1 is D-2,take(List,D1,K1,LL),take(List,X,Y,R),rev(R,R1),
    append1(L,R1,T),append1(T,LL,T1),write_list(T1),!.

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



