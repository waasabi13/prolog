man(voeneg).
man(ratibor).
man(boguslav).
man(velerad).
man(duhovlad).
man(svyatoslav).
man(dobrozhir).
man(bogomil).
man(zlatomir).

woman(goluba).
woman(lubomila).
woman(bratislava).
woman(veslava).
woman(zhdana).
woman(bozhedara).
woman(broneslava).
woman(veselina).
woman(zdislava).

% parent(родитель, ребенок)

parent(voeneg,ratibor).
parent(voeneg,bratislava).
parent(voeneg,velerad).
parent(voeneg,zhdana).

parent(goluba,ratibor).
parent(goluba,bratislava).
parent(goluba,velerad).
parent(goluba,zhdana).

parent(ratibor,svyatoslav).
parent(ratibor,dobrozhir).
parent(lubomila,svyatoslav).
parent(lubomila,dobrozhir).

parent(boguslav,bogomil).
parent(boguslav,bozhedara).
parent(bratislava,bogomil).
parent(bratislava,bozhedara).

parent(velerad,broneslava).
parent(velerad,veselina).
parent(veslava,broneslava).
parent(veslava,veselina).

parent(duhovlad,zdislava).
parent(duhovlad,zlatomir).
parent(zhdana,zdislava).
parent(zhdana,zlatomir).

% 11 Является ли X сыном Y
son(X, Y) :- parent(Y, X),man(X),!.
% 11 Вывести всех сыновей X
son(X) :- parent(X, Y), man(Y), write(Y), nl, fail.
%12 Является ли X сестрой Y
sister(X,Y):-woman(X),parent(Z,X),parent(Z,Y),!.
%12 Вывести всех сестер Х
sister(X):-parent(Y,X),parent(Y,Z),man(Y),woman(Z),write(Z),nl,fail.
%13 Является ли Х бабушкой У
grand_ma(X,Y):-parent(X,Z),parent(Z,Y),woman(X),write(yes),nl,!.
%13 Всех бабушек Х
grand_mas(X):-parent(Z,X),parent(Y,Z),woman(Y),write(Y),nl.
%14 Являются Х дедушкой У внучкой или наоборот
grand_pa_and_da(X,Y):- parent(X,Z),parent(Z,Y),woman(Y),man(X),write(yes),nl,fail;parent(Y,Z),parent(Z,X),woman(X),man(Y),write(yes),nl,fail.
%15
maxU(0,0):-!.
maxU(X,M):-
	X1 is X div 10,
	maxU(X1,M1),
	M2 is X mod 10,
       (M2>M1, M is M2;M is M1),!.
%16
maxD(X,M):- maxD(X,0,M).
maxD(0,M,M):-!.
maxD(X,Y,M):-D is X mod 10, X1 is X div 10,D > Y,!,maxD(X1,D,M); X2 is X div 10,
    maxD(X2,Y,M).
