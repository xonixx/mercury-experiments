:- module zebra.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list, solutions, logic.
	
:- type data ---> englishman; spanish; japanese;
			jaguar; zebra; snail;
			blue; green; red.

neigh(Left, Right, !List) :- 
        (	unify([Left, Right, [no, no, no]], !List)
	;
		unify([[no, no, no], Left, Right], !List)
	).

zebraowner(!Houses, ZebraOwner) :-
	logic.member([yes(englishman), no, yes(red)], !Houses),
        logic.member([yes(spanish), yes(jaguar), no], !Houses),
        neigh([no, yes(snail), no], [yes(japanese), no, no], !Houses),
        neigh([no, yes(snail), no], [no, no, yes(blue)], !Houses),
        logic.member([no, yes(zebra), no], [ZebraOwner|_], !Houses),
        logic.member([no, no, yes(green)], !Houses).

:- mode zebra(out) is nondet.
zebra({Houses, X}) :- 
	EmptyHouse=[no, no, no], 
	zebraowner([EmptyHouse, EmptyHouse, EmptyHouse], Houses, X).
	
main -->
	(	{ solutions(zebra, [{Houses,yes(P)}|_])} ->
		print(Houses),
		nl, nl,
		print(P)
	;
		print("noway!")
	).