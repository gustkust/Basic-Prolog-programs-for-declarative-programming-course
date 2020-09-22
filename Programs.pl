%Często używane podstawowe predykaty

%Szukanie indeksu danego elementu w liście lub elementu na danym indeksie (lista, element, indeks):
indekselementu([E|_],E,1).
indekselementu([_|T],E,N) :- indekselementu(T,E,X), N is X + 1.

%Szukanie największego elementu listy (lista, największy element):
najwiekszy([],0).
najwiekszy([H|T],C) :- najwiekszy(T,Y),H > Y,C is H.
najwiekszy([H|T],C) :- najwiekszy(T,Y),H =< Y,C is Y.

%Szukanie najmniejszego elementu listy (lista, najmniejszy element):
najmniejszy([],9999).
najmniejszy([H|T],C) :- najmniejszy(T,Y),H < Y,C is H.
najmniejszy([H|T],C) :- najmniejszy(T,Y),H >= Y,C is Y.

%Liczenie długości listy (lista, długość):
dlugosclisty([],0 ).
dlugosclisty([_|T],X) :- dlugosclisty(T,Y),X is Y + 1.

%Ostatni element listy (ostatni element, lista):
ostatni(X,[X]).
ostatni(X,[_|Y]) :- ostatni(X,Y).

%Ostatnie elementy listy (lista, liczba ostatnich elementów, ostatnie elementy):
dlugosclisty([],0 ).
dlugosclisty([_|T],X) :- dlugosclisty(T,Y),X is Y + 1.
ostatnie1(L,0,L).
ostatnie1([H1|T1],C1,L) :- C2 is C1 - 1, ostatnie1(T1,C2,L).
ostatnie(L,C1,W) :- dlugosclisty(L,C2), C3 is C2 - C1, ostatnie1(L,C3,W).

%Dodanie elementu na początek listy (oryginalna lista, element, lista z elementem):
napoczatek([],E,[E]).
napoczatek(L,E,[E|L]).

%Wstawianie konkretnego elementu na konkretną pozycję w liście (oryginalna lista, element do wstawienia, pozycja nowego elementu, lista z elementem):
wstawiaj([],_,_,[]).
wstawiaj([H|T],X,0,[H|T1]) :- wstawiaj(T,X,0,T1).
wstawiaj(T,H,1,[H|T2]) :- wstawiaj(T,H,0,T2).
wstawiaj([H|T],C,X,[H|T1]) :- X > 1,Y is X - 1,wstawiaj(T,C,Y,T1).

%Usunięcie elementu na danej pozycji (lista z elementem, pozycja, lista bez elementu):
usun([_|T],1,T) :- !.
usun([H|T1],C,[H|T2]) :- C > 1, X is C - 1, usun(T1,X,T2).

%Usuwanie danej liczby pierwszych elementów w liście (oryginalna lista, liczba pierwszych elementów do usunięcia, lista bez pierwszych elementów):
bezpierwszych(L,0,L).
bezpierwszych([_|T],C,X) :- Y is C - 1, bezpierwszych(T,Y,X).

%Usunięcie danej liczby ostatnich elementów (lista, liczba, lista bez ostatnich):
dlugosclisty([],0 ).
dlugosclisty([_|T],X) :- dlugosclisty(T,Y),X is Y + 1.
bezostatnich(L1,X,L2) :- dlugosclisty(Y,X), append(L2,Y,L1).

%Połączenie kilku zagnieżdżonych list w jedną (lista z listami, finalna lista):
polacz([],[]).
polacz([[]|T],L) :- polacz(T,L).
polacz([[H1|T2]|T],[H1|T1]):- polacz([T2|T],T1).

%Podzielenie listy na dwie na elemencie (element, oryginalna lista, lista na lewo, lista na prawo):
lewoprawo(X,[X|T],[],T).
lewoprawo(X,[H|T],[H|T1],T2) :- lewoprawo(X,T,T1,T2).

%Liczenie sumy elementów listy (lista, suma):
sumalisty([X],X).
sumalisty([H1,H2|T],S) :- X is H1 + H2,sumalisty([X|T],S).

%Liczenie średniej wartości z listy (lista, średnia):
dlugosclisty([],0 ).
dlugosclisty([_|T],X) :- dlugosclisty(T,Y),X is Y + 1.
sumalisty([X],X).
sumalisty([H1,H2|T],S) :- X is H1 + H2,sumalisty([X|T],S).
srednia(Lista,Srednia) :- dlugosclisty(Lista,Dlugosc),sumalisty(Lista,Suma),Srednia is Suma / Dlugosc.

%Różnica dwóch list (lista 1, lista 2, lista z różnicami):
odejmowanielist([],[],[]).
odejmowanielist([H1|T1],[H2|T2],[H3|T3]) :- H3 is abs(H1 - H2), odejmowanielist(T1,T2,T3).

%Odwracanie listy (lista, odwrócona lista):
odwracanie([],[]) :- !.
odwracanie([H|T],R) :- odwracanie(T,A), append(A,[H],R).

%Okręć listę w prawo daną liczbę razy (oryginalna lista, ilość akcji, okręcona lista):
dlugosclisty([],0 ).
dlugosclisty([_|T],X) :- dlugosclisty(T,Y),X is Y + 1.
ostatnie1(L,0,L).
ostatnie1([_|T1],C1,L) :- C2 is C1 - 1, ostatnie1(T1,C2,L).
ostatnie(L,C1,O) :- dlugosclisty(L,C2), C3 is C2 - C1, ostatnie1(L,C3,O).
bezostatnich(L1,X,L2) :- length(Y,X), append(L2,Y,L1).
okrec(L,C,LO) :- dlugosclisty(L,D),D > C,ostatnie(L,C,O),bezostatnich(L,C,LBO),append(O,LBO,LO).
okrec(L,C,LO) :- dlugosclisty(L,D),D =< C,W is C - D,okrec(L,W,LO).


%Predykaty zmieniające liczby w listach

%Zamiana liczby dziesiętnej na binarną (listę) (liczba dziesiętna, liczba binarna):
decnabin(0,[0]) :- !.
decnabin(1,[1]) :- !.
decnabin(C1,V) :- C2 is C1 // 2,H is mod(C1,2),append(C3,[H],V),decnabin(C2,C3).

%Zamiana liczby binarnej (listy) na dziesiętną (liczba binarna, liczba dziesiętna):
odwracanie([],[]) :- !.
odwracanie([H|T],R) :- odwracanie(T,A), append(A,[H],R).
binnadec(L,R) :- odwracanie(L,L1), binnadec1(L1,R).
binnadec1([0],0) :- !.
binnadec1([1],1) :- !.
binnadec1([H|T],R) :- binnadec1(T,W), R is W * 2 + H.

%Zamiana liczby na listę cyfr (liczba, lista cyfr):
liczbanacyfry(H,[H|[]]) :- H < 10, !.
liczbanacyfry(H,X) :- Y is H // 10,liczbanacyfry(Y,X1),Z is mod(H, 10),append(X1,[Z] ,X).

%Zamiana listy liczb na listę list z cyframi (lista liczb, lista list cyfr):
lnc([],[]).
lnc([H1|T1],[H2|T2]):- liczbanacyfry(H1,H2),lnc(T1,T2).
liczbanacyfry(H,[H|[]]) :- H < 10, !.
liczbanacyfry(H,X) :- Y is H // 10,liczbanacyfry(Y,X1),Z is mod(H, 10),append(X1,[Z] ,X).

%Zamiana listy cyfr na liczbe (lista cyfr, liczba)
cyfrynaliczbe(H,X) :- cyfrynaliczbe(H,0,X).
cyfrynaliczbe([],X,X).
cyfrynaliczbe([H|T],X,W) :- X1 is X * 10 + H, cyfrynaliczbe(T,X1,W).

%Zamiana listy list z cyframi na listę list (lista list z cyframi, lista z liczbami):
cnl([],[]).
cnl([H1|T1],[H2|T2]):- cyfrynaliczbe(H1,H2),cnl(T1,T2).
cyfrynaliczbe(H,X) :- cyfrynaliczbe(H,0,X).
cyfrynaliczbe([],X,X).
cyfrynaliczbe([H|T],X,W) :- X1 is X * 10 + H, cyfrynaliczbe(T,X1,W).

%Usuwa elementy z listy psujące monotoniczność (oryginalna lista, zmieniona lista):
monotonicznosc([],[]).
monotonicznosc([H],[H]).
monotonicznosc([H1,H2|T1],[H1|T2]) :- H2 < H1,!,monotonicznosc([H1|T1],[H1|T2]).
monotonicznosc([H1,H2|T1],[H1,H2|T2]):- H2 >= H1, monotonicznosc([H2|T1],[H2|T2]).


%Rzadziej używane predykaty

%Usunięcie ostatniego elementu listy (lista, lista bez ostatniego):
bezostatniego([_],[]).
bezostatniego([H|T1],[H|T2]) :- bezostatniego(T1,T2).

%Przedostatni element listy (lista, przedostatni element):
przedostatni(X,[X|[_]]).
przedostatni(X,[_|Y]) :- przedostatni(X,Y).

%Podwojenie każdego elementu listy (lista, podwojona lista):
podwajaj([],[]).
podwajaj([H1|T1],[H1,H1|T2]) :- podwajaj(T1,T2).

%Usunięcie elementów listy na nieparzystych pozycjach (lista, przerobiona lista):
codrugi([],[]).
codrugi([_],[]).
codrugi([_,H1|T1],[H1|T2]) :- codrugi(T1,T2).

%Podzielenie listy na listę z zagnieżdżonymi listami po dwa elementy (oryginalna lista, lista z listami):
parami([],[]).
parami([H1,H2|T1],[[H1,H2]|T2]) :- parami(T1,T2).

%Rozdzielenie listy liczb na listę z liczbami mniejszymi od danej liczby i na listę z liczbami większymi lub równymi od niej (oryginalna lista, liczba rozdzielająca, lista liczb mniejszych, lista liczb większych lub równych):
rozdziel([],_,[],[]).
rozdziel([H|T],C,[H|T1],L) :- H < C, rozdziel(T,C,T1,L).
rozdziel([H|T],C,L,[H|T1]) :- rozdziel(T,C,L,T1).

%Dzielenie listy na elementy należące do danego przydziału zamkniętego i nienależące (lista, przedział, lista elementów w przedziale, lista pozostałych elementów):
przynaleznosc([],[_,_],[],[]).
przynaleznosc([H|T],[L,P],X,[H|T1]) :- (H =< L;H >= P), przynaleznosc(T,[L,P],X,T1).
przynaleznosc([H|T],[L,P],[H|T1],X) :- (H >= L;H =< P), przynaleznosc(T,[L,P],T1,X).

%Szukanie najbliższej liczby w liście do danej liczby (dana liczba, lista, najbliższa liczba z listy do danej liczby):
najblizszy(_,[_],99999).
najblizszy(X,[H|L],H) :- najblizszy(X,L,Y),Z is abs(H - X),F is abs(Y - X),Z < F,!.
najblizszy(X,[_|L],Z) :- najblizszy(X,L,Z).

%Szukanie najdalszej liczby w liście do danej liczby (dana liczba, lista, najdalsza liczba z listy do danej liczby):
najdalszy(_,[X],X).
najdalszy(X,[H|L],H) :- najdalszy(X,L,Y),Z is abs(H - X),F is abs(Y - X),Z > F,!.
najdalszy(X,[_|L],Z) :- najdalszy(X,L,Z).

%Przemiana listy na wzór innej listy (lista wzór, długa jedna lista, ostateczna lista):
przemienliste([],_,[]) :- !.
przemienliste([[_]|T2],[H3|T3],[[H3]|T5]) :- !,przemienliste(T2,T3,T5).
przemienliste([[_|T1]|T2],[H3|T3],[[H3|T4]|T5]) :- przemienliste([T1|T2],T3,[T4|T5]).

%Powielanie każdego elementu listy tyle razy ile przypisane jest dla odpowiadającej mu pozycji w innej liście (oryginalna lista, lista z liczbami powieleń, lista z powielonymi elementami):
powiel([],[],[]).
powiel([_|T],[0|T1],L):- powiel(T,T1,L).
powiel([H|T],[C|T1],[H|T2]):- Y is C - 1,powiel([H|T],[Y|T1],T2).

%Splątanie dwóch list ([a1, b1, a2, b2…]) (lista 1, lista 2, splatana lista):
splataj([H1|T1],[H2|T2],[H1,H2|T3]) :- splataj(T1,T2,T3).
splataj([],X,X).
splataj(X,[],X).

%Sprawdzenie, czy lista jest symetryczna (lista):
symetryczna([]).
symetryczna([H|T]) :- append(X,[H],T),symetryczna(X).

%Sprawdzanie, czy lista jest liniowa np. [1, 3, 5, 7…] (lista):
liniowa([_,_]).
liniowa([H1,H2,H3|T]) :- H2 - H1 =:= H3 - H2,liniowa([H2,H3|T]).

%Liczenie danego elementu w liście (lista, liczba wystąpień):
ilerazy([],0,_).
ilerazy([H|T],Y,W) :- H =:= W, ilerazy(T,X,W), Y is X + 1.
ilerazy([_|T],Y,W) :- ilerazy(T,Y,W).

%Wymiana każdego wystąpienia danego elementu listy na inny (element wymieniany, element wstawiany, lista przed wymianą, przerobiona lista):
wymiana(_,_,[],[]).
wymiana(X,Y,[X|T1],[Y|T2]) :- !,wymiana(X,Y,T1,T2).
wymiana(X,Y,[H|T1],[H|T2]) :- wymiana(X,Y,T1,T2).

%Liczenie liczb dodatnich w liście (lista, liczba liczb dodatnich):
ile_dodatnich([],0).
ile_dodatnich([H|T],X) :- ile_dodatnich(T,Y),H > 0,X is Y + 1.
ile_dodatnich([_|T],X) :- !,ile_dodatnich(T,X).

%Sortowanie listy metodą quick sort (oryginalna lista, posortowana lista):
szybkisort([],[]).
szybkisort([H|T],W):-
	piwot(H,T,L1,L2),szybkisort(L1,W1),szybkisort(L2,W2),
	append(W1,[H|W2]).
piwot(H,[],[],[]).
piwot(H,[X|T],[X|L],G):-X=<H,piwot(H,T,L,G).
piwot(H,[X|T],L,[X|G]):-X>H,piwot(H,T,L,G).

%Zamiana listy przedziałów w jeden przedział zawierający wszystkie przedziały (lista przedziałów, największy możliwy przydział):
rozszerzaj([],[1000,-1000]).
rozszerzaj([[L,P]|T],[LM,PM]) :- rozszerzaj(T,[X,Y]),L =< X, P >= Y,LM is L,PM is P,!.
rozszerzaj([[L,P]|T],[LM,PM]) :- rozszerzaj(T,[X,Y]),L =< X, P =< Y,LM is L,PM is Y,!.
rozszerzaj([[L,P]|T],[LM,PM]) :- rozszerzaj(T,[X,Y]),L >= X, P =< Y,LM is X,PM is Y,!.
rozszerzaj([[L,P]|T],[LM,PM]) :- rozszerzaj(T,[X,Y]),L >= X, P >= Y,LM is X,PM is P,!.
rozszerzaj([[_,_]|T],[LM,PM]) :- rozszerzaj(T,[LM,PM]).

%Z posortowanej listy liczb wybiera dwie sąsiednie (przedział), do których należy dana liczba (posortowana lista, liczba, przedział):
nalezy([H1,H2],_,[H1,H2]) :- !.
nalezy([H1,H2,_|_],S,[H1,H2]) :- H1 >= S, H2 =< S,!.
nalezy([_,H2,H3|T],S,[L,P]) :- nalezy([H2,H3|T],S,[L,P]).

%Rozkład liczby na kwadraty innych liczb (liczba, lista liczb):
rozbij(0,[],0) :- !.
rozbij(Liczba,[H|T],H) :- X is (H + 1) * (H + 1), X > Liczba, Minus is H * H, W is Liczba - Minus, rozbij(W,T,0).
rozbij(L,W,Z) :- H is Z + 1, rozbij(L,W,H).
rozkład(L,LS) :- robij(L,LS,0).

%Sortowanie listy i przestawianie elementów w drugiej na jej wzór (lista do posortowania, posortowana lista, lista do przestawienia, przestawiona lista):
wstaw2(X,[],[X],Y,[],[Y]):- !.
wstaw2(X,[H|T],[X,H|T],Y,[H2|T2],[Y,H2|T2]):- H >= X, !.
wstaw2(X,[H|T],[H|T1],Y,[H2|T2],[H2|T3]):- wstaw2(X,T,T1,Y,T2,T3).
wstawsort2([],[],[],[]).
wstawsort2([H|T],X,[H2|T2],W) :- wstawsort2(T,Y,T2,Z),wstaw2(H,Y,X,H2,Z,W).

%Wypisuje w liście wszystkie liczby naturalne do danej liczby (liczba, lista):
doliczby(L,DL) :- doliczby1(L,1,DL).
doliczby1(L,H,[]) :- Z is H + 1,L =:= Z.
doliczby1(L,H,[H|T]) :- L > H, W is H + 1, doliczby1(L,W,T).

%Daje listę dzielników danej liczby (liczba, lista z jej dzielnikami):
doliczby(L,DL) :- doliczby1(L,1,DL).
doliczby1(L,H,[]) :- Z is H + 1,L =:= Z.
doliczby1(L,H,[H|T]) :- L > H, W is H + 1, doliczby1(L,W,T).
dzielniki1(_,[],[]).
dzielniki1(L,[H|T],[H|T1]) :- X is mod(L,H), X =:= 0,!,dzielniki1(L,T,T1).
dzielniki1(L,[_|T],T1) :- dzielniki1(L,T,T1).
dzielniki(L,D) :- doliczby(L,X),dzielniki1(L,X,D).

%Zostawia tylko parzyste liczby z listy (oryginalna lista, lista bez nieparzystych elementów):
parzyste([],[]).
parzyste([H|T],[H|T1]) :- X is mod(H,2), X =:= 0,!, parzyste(T,T1).
parzyste([_|T],T1) :- parzyste(T,T1).

%Zostawia tylko nieparzyste liczby z listy (oryginalna lista, lista bez parzystych elementów):
nieparzyste([],[]).
nieparzyste([H|T],[H|T1]) :- X is mod(H,2), X =:= 1,!, nieparzyste(T,T1).
nieparzyste([_|T],T1) :- nieparzyste(T,T1).

%Daje listy z odległościami wszystkich elementów z jednej listy do wszystkich elementów z drugiej listy w alfabecie (lista, dla której szukać odległości, lista z odległościami, lista, do której szukać odległości):
lo([],[],_).
lo([H1|T1],[H2|T2],OL) :- listaodleglosci(H1,OL,H2),lo(T1,T2,OL).
listaodleglosci(_,[],[]).
listaodleglosci(E,[H1|T1],[H2|T2]) :- alfabet(A), E \= H1,!,
    indekselementu(A,E,INDE),
    indekselementu(A,H1,INDH1), H2 is abs(INDE - INDH1),
    listaodleglosci(E,T1,T2).
listaodleglosci(E,[H1|T1],T2) :- E == H1,listaodleglosci(E,T1,T2).

%Składa listę, której suma daje daną liczbę z elementów innej (malejącej) listy (liczba, malejąca lista, lista z sumą liczby):
rozloz(0,_,[]).
rozloz(L,[H|T],[H|T1]) :- L >= H,!,X is L - H, rozloz(X,[H|T],T1).
rozloz(L,[H|T],T1) :- L < H, rozloz(L,T,T1).

%Zamienia liczbę dziesiętną na szesnastkową (liczba dziesiętna, liczba szesnastkowa):
whex(0,0).
whex(1,1).
whex(2,2).
whex(3,3).
whex(4,4).
whex(5,5).
whex(6,6).
whex(7,7).
whex(8,8).
whex(9,9).
whex(10,a).
whex(11,b).
whex(12,c).
whex(13,d).
whex(14,e).
whex(15,f).
decnahex(0,[0]) :- !.
decnahex(1,[1]) :- !.
decnahex(C1,V) :- C2 is C1 // 16,H is mod(C1,16),
    whex(H,Z),append(C3,[Z],V),decnahex(C2,C3).

%Rozbija liczba na czynniki pierwsze (liczba, lista czynników pierwszych):
czynnikipierwsze(X,Y) :- czyn(X,2,Y).
czyn(A,_,[]):- A =< 1, !.
czyn(A,A,[A]):-!.
czyn(A,B,[B|T]):- 0 =:= mod(A,B), C is div(A,B), czyn(C, B,T).
czyn(A,B, T):- 0 =\= mod(A,B), C is B + 1, czyn(A,C,T).

%Sortowanie listy przez wstawiania(lista do posortowania, posortowana lista):
wstaw(X,[],[X]):- !.
wstaw(X,[H|T],[X,H|T]):- H >= X, !.
wstaw(X,[H|T],[H|T1]):- wstaw(X,T,T1).
wstawsort([],[]).
wstawsort([H|T],X) :- wstawsort(T,Y),wstaw(H,Y,X).

%Usuwa powtórzenia z posortowanej listy (lista, lista bez powtórzeń):
bezpowtorzen([],[]) :- !.
bezpowtorzen([X],[X]) :- !.
bezpowtorzen([H1|T1],[H1|T2]) :- \+member(H1,T1), bezpowtorzen(T1,T2).
bezpowtorzen([H1|T1],T2) :- member(H1,T1), bezpowtorzen(T1,T2).


%Dodatkowe predykaty

%Sprawdza czy lista jest ma parzystą długość (lista):
parzysta([_,_]).
parzysta([X,Y|T]) :- parzysta(T).

%Sprawdza czy lista ma nieparzystą długość (lista):
nieparzysta([X|[]]).
nieparzysta([X,Y|T]) :- nieparzysta(T).

%Wyznacza elementy listy o konkretnej sumie (lista, suma):
znajdzsume([],0,[]).
znajdzsume([H|T],S,[H1|T1]) :- N is S - H1, znajdzsume(T,N,T1).
znajdzsume([H|T],S,T1) :- znajdzsume(T,S,T1).

%Wyznacza silnie (liczba, jej silnia):
silnia(0,1).
silnia(L,R) :- L > 0,L1 is L - 1,silnia(L1,L2), R is L2 * L.

%Wyznacza wartość elementu ciągu Fibonacciego o danym indeksie (indeks, wartość):
fibon(0,1) :- !.
fibon(1,1) :- !.
fibon(X,F) :- Y is X - 1, Z is Y - 1, fibon(Y,F1),fibon(Z,F2),F is F1 + F2).

%Liczy ile razy pod rząd w liście wystąpiły kolejne elementy(lista, lista z ilościami wystąpień elementów pod rząd):
ilepodrzad([],[]).
ilepodrzad([_],[1|[]]).
ilepodrzad([H1,H2|T1],[X|T2]):- H1 =:= H2,ilepodrzad([H2|T1],[Y|T2]), X is Y + 1.
ilepodrzad([H1,H2|T1],[1|T2]):- H1 \= H2,ilepodrzad([H2|T1],T2).
