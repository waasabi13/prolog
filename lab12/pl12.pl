%����� ���������� �����, ������� ������� � ��������
nod(A,0,A):-!.
nod(A,B,X):-C is A mod B,nod(B,C,X).

eulerD(A,X):-eulerD(A,X,A,0).
eulerD(_,X,0,X):-!.
eulerD(A,X,I,R):-I1 is I-1, nod(A,I1,NOD),NOD is 1,!,R1 is R+1,eulerD(A,X,I1,R1).
eulerD(A,X,I,R):-I1 is I-1,eulerD(A,X,I1,R).


