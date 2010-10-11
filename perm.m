:- module perm.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, solutions.

appendl([]) = [].
appendl([H]) = H.
appendl([H1,H2|T]) = appendl([append(H1, H2)|T]). 

:- mode permut(in, out) is multi.
permut([], []).
permut([H | T], appendl([A, [H], B])) :-
	permut(T, Pt),
	append(A, B, Pt).

main -->
	{ solutions(permut(1..4), Perms) },
	print(Perms).
	
