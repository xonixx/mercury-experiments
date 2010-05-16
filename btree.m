:- module btree.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- type btree(T) ---> btree(T, btree(T), btree(T)); leaf(T).

:- func min(T, T) = T.
min(A, B) = (ordering(A, B) = (<) -> A; B).

:- func btmin(btree(T)) = T.
btmin(leaf(A)) = A.
btmin(btree(A, B1, B2)) = min(A, min(M1, M2)) :-
	M1 = btmin(B1),
	M2 = btmin(B2).

main --> print(btmin(
		     btree(6,
			   btree(4,
				 btree(5,
				       leaf(7),
				       leaf(10)),
				 btree(2,
				       leaf(100),
				       btree(200,
					     leaf(300),
					     leaf(4000)))),
			   leaf(3)))).
