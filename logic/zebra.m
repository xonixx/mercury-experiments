:- module zebra.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list.


:- func unify(maybe(T), maybe(T)) = maybe(T) is semidet.
unify(no, yes(E)) = yes(E).
unify(yes(E), no) = yes(E).
unify(no, no) = no.
unify(yes(E), yes(E)) = yes(E).

:- type maybe_list(T) == list(maybe(T)).

:- func unify_lists(maybe_list(T), maybe_list(T)) = maybe_list(T) is semidet.
unify_lists([], []) = [].
unify_lists([H|T], [H1|T1]) = [unify(H, H1) | unify_lists(T, T1)].

:- pred unify_lists(maybe_list(T), maybe_list(T), maybe_list(T)).
:- mode unify_lists(in, in, out) is semidet.
unify_lists(L1, L2, unify_lists(L1, L2)).

:- pred member(maybe(T), maybe(T), maybe_list(T), maybe_list(T)).
:- mode member(in, out, in, out) is semidet.
member(E, E, [], []) :- fail.
member(E0, E1, [H | T], [H1 | T1]) :- 
	(	H0 = unify(E0, H) ->
		H1 = H0
	;
		H1 = H,
		member(E0, E1, T, T1)
	).
	
:- pred member(maybe(T), maybe_list(T), maybe_list(T)).
:- mode member(in, in, out) is semidet.	
member(E, !L) :- member(E,_,!L).	
	
%
% solution
%

neigh(Left, Right, !List) :- 
        (	unify_lists([Left, Right, no], !List)
	;
		unify_lists([no, Left, Right], !List)
	).

:- type people ---> englishman; spanish; japanese.
:- type animals ---> jaguar; zebra; snail.
:- type color ---> blue; green; red.

zebraowner(!Houses, ZebraOwner) :-
	member([yes(englishman), no, yes(red)], !Houses),
        member([yes(spanish), yes(jaguar), no], !Houses),
        neigh([no, yes(snail), no], [yes(japanese), no, no], !Houses),
        neigh([no, yes(snail), no], [no, no, yes(blue)], !Houses),
        member([no, yes(zebra), no], [ZebraOwner,_,_], !Houses),
        member([no, no, yes(green)], !Houses).


zebra({Houses, X}) :- 
	EmptyHouse=[no, no, no], 
	zebraowner([EmptyHouse, EmptyHouse, EmptyHouse], Houses, X).
	
main -->
	(	{ solutions(zebra, Solutions)} ->
		print(Solutions)
	;
		print("no solutions")
	).