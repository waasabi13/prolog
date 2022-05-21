fibD(N,X):-fibD(1,1,2,N,X).
fibD(_,X,N,N,X):-!.
fibD(F1,F2,I,N,X):- C is F1+F2,I1 is I+1, fibD(F2,C,I1,N,X).

fibU(1,1):-!.
fibU(2,1):-!.
fibU(N,X):-N1 is N-1,N2 is N-2, fibU(N1,X1),fibU(N2,X2),X is X1+X2.

nod(A,0,A):-!.
nod(A,B,X):-C is A mod B,nod(B,C,X).

prime(X):-prime(X,2).
prime(X,X):-!.
prime(X,R):-0 is X mod R,!,fail;R1 is R+1,prime(X,R1).

sumcifr()
