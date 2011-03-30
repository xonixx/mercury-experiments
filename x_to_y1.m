:- module x_to_y.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, char, int, require.

:- pred replaceEvenElementList(list(char), list(char)).
:- mode replaceEvenElementList(in, out) is det.

%~ replaceEvenElementList([A,'x'|T1],[A,'y'|T2]):-replaceEvenElementList(T1,T2).
%~ replaceEvenElementList(L,L).

%~ replaceEvenElementList([A,'x'|T1],[A,'y'|T2]):-replaceEvenElementList(T1,T2).
%~ replaceEvenElementList([],[]).

%~ replaceEvenElementList([A,E|T1],[A,E1|T2]):-E1 = (E='x'->'y'; E),replaceEvenElementList(T1,T2).
%~ replaceEvenElementList([],[]).

%~ replaceEvenElementList([A,E|T1],[A,E1|T2]):-E1 = (E='x'->'y'; E),replaceEvenElementList(T1,T2).
%~ replaceEvenElementList([E],[E]).
%~ replaceEvenElementList([],[]).


replaceEvenElementList([A,E|T1],[A,E1|T2]):- replaceEvenElementList(T1,T2), E1 = (E='x'->'y'; E).
%replaceEvenElementList([A,E|T1],[A,E1|T2]):- E1 = (E='x'->'y'; E), replaceEvenElementList(T1,T2).
replaceEvenElementList([E],[E]).
replaceEvenElementList([],[]).

:- mode gen(in, in, out, in) is det.
gen(N, E) --> 
	(	{N>0} ->
		[E],
		gen(N-1, E)
	;
		[]
	).
	
l = L :- gen(10000000, 'x', L, []).

main --> {replaceEvenElementList(l, L1), (take(10,L1,L2) -> L = L2; error("Not enough!"))}, print(L).