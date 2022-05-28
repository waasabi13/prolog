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

check(A):-IntA is round(A), IntA = A. %1.5=1


intfloat([H|T]):-check(H), intfloat(T, 1); intfloat(T, 0).% маркер чтобы понимать с какого числа начали
intfloat([],_):-!.
intfloat([H|T], 1):- not(check(H)),intfloat(T, 0),!;fail.
intfloat([H|T], 0):- check(H), intfloat(T, 1),!;fail.

task2:- read(Count), read_list(Count, List), intfloat(List).
%3
isElemUnique(List,Elem):-countOfThisElem(List,Elem,Count),1 is Count.%уникальный ли в списке

countOfThisElem([],_,Count):- Count is 0,!.%число вхождений
countOfThisElem([H|T],Elem,Count):-
    H = Elem,

    countOfThisElem(T,Elem,Count1),
    Count is 1+Count1,!;

    countOfThisElem(T,Elem,Count).

listUniqueWithIndex(List,UniList,UniIndexList):-listUniqueWithIndex(List,UniList,UniIndexList,0,List).%уникальный ли элемент и запомнить индекс
listUniqueWithIndex([],[],[],_,_):-!.
listUniqueWithIndex([H|T],UniList,UniIndexList,Index,StartList):-
    I1 is Index+1,
    (
        isElemUnique(StartList,H),

        listUniqueWithIndex(T,UniList1,UniIndexList1,I1,StartList),
        append([H],UniList1,UniList),
        append([I1],UniIndexList1,UniIndexList);

        listUniqueWithIndex(T,UniList,UniIndexList,I1,StartList)
    ),!.

listDelOnAnotherList([],[],_).
listDelOnAnotherList([UH|UT],[IH|IT],NewList):-
    0 is UH mod IH,

    listDelOnAnotherList(UT,IT,NewList1),
    append([UH],NewList1,NewList),!;

    listDelOnAnotherList(UT,IT,NewList),!.

listDelOnIndexAndUnique(List,NewList):-
    listUniqueWithIndex(List,UniList,UniIndexList),
    listDelOnAnotherList(UniList,UniIndexList,NewList).

task3:- read(N),read_list(N,List),listDelOnIndexAndUnique(List,NewList),write_list(NewList),!.
%4 Беседует трое друзей: Белокуров, Рыжов, Чернов. Брюнет
% сказал Белокурову: “Любопытно, что один из нас блондин, другой брюнет,
% третий - рыжий, но ни у кого цвет волос не соответствует фамилии”. Какой
% цвет волос у каждого из друзей?
inList([],_):-fail.
inList([X|_],X).
inList([_|T],X):-inList(T,X).

task4:-
    Hairs=[_,_,_],
    inList(Hairs,[belov,_]),
    inList(Hairs,[chernov,_]),
    inList(Hairs,[rijov,_]),
    inList(Hairs,[_,red]),
    inList(Hairs,[_,blond]),
    inList(Hairs,[_,brunet]),
    not(inList(Hairs,[belov,blond])),
    not(inList(Hairs,[chernov,brunet])),
    not(inList(Hairs,[rijov,red])),
    write(Hairs),
    !.
%5 Три подруги вышли в белом, зеленом и синем платьях и туфлях. Известно, что только у Ани цвета платья и туфлей совпадали.
% Ни туфли, ни платье Вали не были белыми. Наташа была в зеленых туфлях. Определить цвета платья и туфель на каждой из подруг.
task5:-
    Girls=[_,_,_],
    inList(Girls,[anya,X,X]),
    inList(Girls,[valya,_,_]),
    inList(Girls,[natasha,zel,_]),
    inList(Girls,[_,bel,_]),
    inList(Girls,[_,sin,_]),
    inList(Girls,[_,zel,_]),
    inList(Girls,[_,_,bel]),
    inList(Girls,[_,_,sin]),
    inList(Girls,[_,_,zel]),
    not(inList(Girls,[valya,bel,_])),
    not(inList(Girls,[valya,_,bel])),
    not(inList(Girls,[natasha,Y,Y])),
    write(Girls),
    !.
%6 На заводе работали три друга: слесарь, токарь и сварщик. Их
%фамилии Борисов, Иванов и Семенов. У слесаря нет ни братьев, ни сестер. Он
%самый младший из друзей. Семенов, женатый на сестре Борисова, старше то-
%каря. Назвать фамилии слесаря, токаря и сварщика.

task6:- Zavod=[_, _, _],
    inList(Zavod, [slesar, _, mladshiy, _, 0]),
    inList(Zavod, [tokar, _,sredniy, _, _]),
    inList(Zavod, [svarshik, _, _, _, _]),
    inList(Zavod, [_, borisov,_,_, 1]),
    inList(Zavod, [_, ivanov,_, _, _]),
    inList(Zavod, [_, semenov,starshiy,_,_]),
    inList(Zavod, [slesar, Who1, _, _, _]),
    inList(Zavod, [tokar, Who2, _, _, _]),
    inList(Zavod, [svarshik, Who3, _, _, _]),
    write(Zavod),nl,write('slesar = '),write(Who1),nl,write('tokar = '),write(Who2),nl,write('svarshick = '),write(Who3),!.

%A слева от B
left(_, _, [_]):-fail.
left(A, B, [A|[B|_]]).
left(A, B, [_|T]):-left(A, B, T).

%А справа от В
right(_, _, [_]):-fail.
right(A, B, [B|[A|_]]).
right(A, B, [_|T]):-right(A, B, T).

%около
before(A, B, List):-left(A, B, List).
before(A, B, List):-right(A, B, List).

task7:- Drinks = [_, _, _, _],
    inList(Drinks,[water, _]),
    inList(Drinks,[milk, _]),
    inList(Drinks,[lemonade, _]),
    inList(Drinks,[kvas, _]),
    inList(Drinks,[_, bottle]),
    inList(Drinks,[_, kuvshin]),
    inList(Drinks,[_, banka]),
    inList(Drinks,[_, glass]),
    not(inList(Drinks,[water, bottle])),
    not(inList(Drinks,[milk, bottle])),
    not(inList(Drinks,[water, banka])),
    not(inList(Drinks,[lemonade, banka])),
    left([_, kuvshin],[lemonade,_], Drinks),
    right([kvas, _], [lemonade,_], Drinks),
    before([_, banka], [_, glass], Drinks),
    before([milk,_], [_, glass], Drinks),
    write(Drinks).
%Воронов, Павлов, Левицкий и Сахаров – четыре талантли-
%вых молодых человека. Один из них танцор, другой художник, третий-певец,
%а четвертый-писатель. О них известно следующее: Воронов и Левицкий си-
%дели в зале консерватории в тот вечер, когда певец дебютировал в сольном
%концерте. Павлов и писатель вместе позировали художнику. Писатель написал
%биографическую повесть о Сахарове и собирается написать о Воронове. Воро-
%нов никогда не слышал о Левицком. Кто чем занимается?
task8:-Talents = [_, _, _, _],
    inList(Talents, [voronov, _, ne_slyshal_o_levitskom]),
    inList(Talents, [pavlov, _, poziroval_hudozhniku]),
    inList(Talents, [levitskiy, _, _]),
    inList(Talents, [saharov, _, _]),
    inList(Talents, [_, dancer, _]),
    inList(Talents, [_, painter, _]),
    inList(Talents, [_, singer, _]),
    inList(Talents, [_, writer, poziroval_hudozhniku]),
    not(inList(Talents, [pavlov, singer,_])),
    not(inList(Talents, [levitskiy, singer,_])),
    not(inList(Talents, [pavlov, painter,_])),
    not(inList(Talents, [saharov, writer,_])),
    not(inList(Talents, [voronov, writer,_])),
    inList(Talents, [voronov, Who1, _]),
    inList(Talents, [pavlov, Who2, _]),
    inList(Talents, [levitskiy, Who3, _]),
    inList(Talents, [saharov, Who4, _]),
    write('voronov - '), write(Who1), nl, write('pavlov - '), write(Who2), nl, write('levitskiy - '), write(Who3), nl, write('saharov - '), write(Who4).
%Задание 19 Три друга заняли первое, второе, третье места в соревнова-
% ниях универсиады. Друзья разной национальности, зовут их по-разному, и
% любят они разные виды спорта. Майкл предпочитает баскетбол и играет
% лучше,чем американец. Израильтянин Саймон играет лучше теннисиста. Игрок в
% крикет занял первое место. Кто является австралийцем? Каким спортом
% увлекается Ричард?

task9:- Sport=[_, _, _],
    inList(Sport, [michael, _, basketball, _]),
    inList(Sport, [simon, israel, _, _]),
    inList(Sport, [richard,_, _, _]),
    inList(Sport, [_, _, cricket, first]),
    inList(Sport, [_, australia, _, _]),
    inList(Sport, [_, _, tennis, _]),
    inList(Sport, [_, america, _, _]),
    in_list(Sport, [_, _, _, second]),
    in_list(Sport, [_, _, _, third]),
    not(inList(Sport,[_, america, basketball,_])),
    not(inList(Sport, [simon, tennis,_,_])),
    inList(Sport, [Who1, australia, _, _]),
    inList(Sport, [richard,_,Who2,_]),
    write('australia - '), write(Who1), nl, write('richard - '), write(Who2), nl, write(Sport).
%10
/* Пятеро детей Алик, Боря, Витя, Лена и Даша приехали в ла-
герь из 5 разных городов: Харькова, Умани, Полтавы, Славянска и Краматор-
ска. Есть 4 высказывания: 1) Если Алик не из Умани, то Боря из Краматорска.
2) Или Боря, или Витя приехали из Харькова. 3) Если Витя не из Славянска, то
Лена приехала из Харькова. 4) Или Даша приехала из Умани, или Лена из Кра-
маторска. Кто откуда приехал?
*/
task10:-
    Kortej=[_,_,_,_,_],
    inList(Kortej,[alik,_]),
    inList(Kortej,[borya,_]),
    inList(Kortej,[vitya,_]),
    inList(Kortej,[lena,_]),
    inList(Kortej,[dasha,_]),

    inList(Kortej,[_,kharkiv]),
    inList(Kortej,[_,uman]),
    inList(Kortej,[_,poltava]),
    inList(Kortej,[_,slavyansk]),
    inList(Kortej,[_,kramatorsk]),

    (
        not(inList(Kortej,[alik,uman])),%пункт а и так далее
        inList(Kortej,[borya,kramatorsk]);

        inList(Kortej,[alik,uman])
    ),
    (
        inList(Kortej,[borya,kharkiv]);
        inList(Kortej,[vitya,kharkiv])
    ),
    (
        not(inList(Kortej,[vitya,slavyansk])),
        inList(Kortej,[lena,kharkiv]);

        inList(Kortej,[vitya,slavyansk])
    ),
    (
        inList(Kortej,[dasha,uman]);
        inList(Kortej,[lena,kramatorsk])
    ),
    write(Kortej),
    !.






