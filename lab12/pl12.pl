%найти количество чисел, взаимно простых с заданным
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

% Ќайти делитель числа, €вл€ющийс€ взаимно простым с наибольшим
% количество цифр данного числа.

