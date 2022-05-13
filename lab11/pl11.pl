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
