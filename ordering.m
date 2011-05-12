:- module ordering.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list, int.

:- type order 
	--->	eq
	;	up
	;	down
	.
	
:- type maybe_order == maybe(order).	

list_ordering(L) = list_ordering(yes(eq), L).

list_ordering(no, _) = no.
list_ordering(O @ yes(_), []) = O.
list_ordering(O @ yes(_), [_]) = O.
list_ordering(O @ yes(_), [A,B|C]) = list_ordering(new_ordering(O, yes(order_num(A,B))), [B|C]).

order_num(A, B) = (A = B -> eq; A < B -> up; down).

%:- mode new_ordering(in, in(bound(yes(eq)); bound(yes(up)); bound(yes(down)))) = out.
new_ordering(no, no) = no.
new_ordering(no, yes(_)) = no.
new_ordering(yes(_), no) = no.
new_ordering(yes(O1), yes(O2)) = 
	(	O1 = eq
	->	yes(O2)
	;	O2 = eq
	->	yes(O1)
	;	O1 = O2 
	->	yes(O1)
	;	no
	).

test(L, !IO) :- 
	write(L,!IO), 
	write_string(" : ", !IO), 
	write(list_ordering(L):maybe_order, !IO), 
	nl(!IO).

main -->
	test([1,2]),
	test([2,1]),
	test([1,2,3,4,5]),
	test([5,4,3,2,1]),
	test([1,3,2,4,5]),
	test([7,7,7,7,7,7,7]).