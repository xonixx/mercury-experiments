:- module zebra.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list, solutions.


:- typeclass unifiable(T) where [
	func unify(T, T) = T is semidet
].

:- instance unifiable(maybe(T)) where [
	func(unify/2) is unify_maybe
].

:- instance unifiable(list(T)) <= unifiable(T) where [
	func(unify/2) is unify_lists 
].

:- func unify_maybe(maybe(T), maybe(T)) = maybe(T) is semidet.
unify_maybe(no, yes(E)) = yes(E).
unify_maybe(yes(E), no) = yes(E).
unify_maybe(no, no) = no.
unify_maybe(yes(E), yes(E)) = yes(E).

:- type maybe_list(T) == list(maybe(T)).
:- type maybe_list_list(T) == list(maybe_list(T)).

:- func unify_lists(list(T), list(T)) = list(T) is semidet <= unifiable(T).
unify_lists([], []) = [].
unify_lists([H|T], [H1|T1]) = [unify(H, H1) | unify_lists(T, T1)].

:- pred unify_lists(list(T), list(T), list(T)) <= unifiable(T).
:- mode unify_lists(in, in, out) is semidet.
unify_lists(L1, L2, unify_lists(L1, L2)).

:- pred member(T, T, list(T), list(T)) <= unifiable(T).
:- mode member(in, out, in, out) is nondet.
member(E, E, [], []) :- fail.
member(E0, E1, [H | T], [H1 | T1]) :- 
	(	H0 = unify(E0, H),
		H1 = H0,
		(	E1 = H0, T=T1
		%;
		%	member(E0, E1, T, T1)
		)
	;
		H1 = H,
		member(E0, E1, T, T1)
	).
	
:- pred member(T, list(T), list(T)) <= unifiable(T).
:- mode member(in, in, out) is nondet.	
member(E, !L) :- member(E,_,!L).	
	
%
% solution
%

:- pred neigh(maybe_list(T), maybe_list(T), maybe_list_list(T), maybe_list_list(T)).
neigh(Left, Right, !List) :- 
        (	unify_lists([Left, Right, [no, no, no]], !List)
	;
		unify_lists([[no, no, no], Left, Right], !List)
	).

%:- type people ---> englishman; spanish; japanese.
%:- type animals ---> jaguar; zebra; snail.
%:- type color ---> blue; green; red.

:- type data ---> englishman; spanish; japanese;
			jaguar; zebra; snail;
			blue; green; red.

zebraowner(!Houses, ZebraOwner) :-
	zebra.member([yes(englishman), no, yes(red)], !Houses),
        zebra.member([yes(spanish), yes(jaguar), no], !Houses),
        %neigh([no, yes(snail), no], [yes(japanese), no, no], !Houses),
        %neigh([no, yes(snail), no], [no, no, yes(blue)], !Houses),
        zebra.member([no, yes(zebra), no], H, !Houses), H = [ZebraOwner,_,_],
        zebra.member([no, no, yes(green)], !Houses).

:- mode zebra(out) is nondet.
zebra({Houses, X}) :- 
	EmptyHouse=[no, no, no], 
	zebraowner([EmptyHouse, EmptyHouse, EmptyHouse], Houses, X).
	
main -->
	{ solutions(zebra, Solutions)},
	%print(Solutions),
	
	{solutions((pred(HH1::out) is nondet :-
		EmptyHouse=[no, no, no], HH=[EmptyHouse, EmptyHouse, EmptyHouse],
		zebra.member([yes(englishman), no, yes(red)], HH, HH1)%,
		%zebra.member([yes(spanish), yes(jaguar), no], HH1, HH2)
		), 
		L)},
	nl, print(L).