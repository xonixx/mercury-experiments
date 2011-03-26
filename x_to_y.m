:- module x_to_y.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, char.

:- pred replaceEvenElementList(list(char), list(char)).
:- mode replaceEvenElementList(in, out) is det.

%~ replaceEvenElementList([A,'x'|T1],[A,'y'|T2]):-replaceEvenElementList(T1,T2).
%~ replaceEvenElementList(L,L).

%~ replaceEvenElementList([A,'x'|T1],[A,'y'|T2]):-replaceEvenElementList(T1,T2).
%~ replaceEvenElementList([],[]).

%~ replaceEvenElementList([A,E|T1],[A,E1|T2]):-E1 = (E='x'->'y'; E),replaceEvenElementList(T1,T2).
%~ replaceEvenElementList([],[]).

replaceEvenElementList([A,E|T1],[A,E1|T2]):-E1 = (E='x'->'y'; E),replaceEvenElementList(T1,T2).
replaceEvenElementList([E],[E]).
replaceEvenElementList([],[]).

main --> {replaceEvenElementList(['1','x','3','p','5','x','7'], L)}, print(L).