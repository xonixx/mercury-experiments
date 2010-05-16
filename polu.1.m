:- module poly.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, int.

:- type poly == list(comp). % polynome
:- type comp == list(item). % component
:- type item ---> num(int); var(var, pwr).
:- type pwr == int.
:- type var ---> x; y; z.

:- func null_c = comp.
null_c = [].

:- func null = poly.
null = [null_c].

:- func sum(poly, poly) = poly.
sum(P1, P2) = P :- append(P1, P2, P).

:- func mul(poly, poly) = poly.
mul(P1, P2) = mul1(null, P1, P2).

:- func mul1(poly, poly, poly) = poly.
mul1(P0, [C1 | CC], P2) = mul1(sum(P0, mul_c_p(C1, P2)), CC, P2).
mul1(P, [], _) = P.

:- func mul_c_p(comp, poly) = poly.
mul_c_p(C, P) = mul_c_p1(null, C, P).

:- func mul_c_p1(poly, comp, poly) = poly.
mul_c_p1(P0, C, [C1 | CC]) = mul_c_p1([mul_c_c(C, C1) | P0], C, CC).
mul_c_p1(P, _, []) = P.

:- func mul_c_c(comp, comp) = comp.
mul_c_c(C1, C2) = norm_c(append(C1, C2)).

:- func compare_i(item, item) = comparison_result.
compare_i(num(_),var(_,_)) = (<).
compare_i(var(_,_),num(_)) = (>).
compare_i(num(A),num(B)) = ordering(A, B).
compare_i(var(A,_),var(B,_)) = ordering(A, B).

:- func sort_c(comp) = comp. 
sort_c(C) = sort(compare_i, C).

:- func compact_c(comp) = comp.
compact_c(C) = R :-
	( C = [num(A), num(B) | T] ->
	  R = compact_c([num(A*B) | T])
	;
	  C = [var(A,P1),var(A, P2) | T] ->
	  R = compact_c([var(A,P1+P2) | T])
	;
	  C = [A | T] ->
	  R = [A | compact_c(T)]
	;
	  R = []
	).
/*
compact_c([num(A), num(B) | T]) = compact_c([num(A*B) | T]).
compact_c([var(A,P1),var(A, P2) | T]) = compact_c([var(A,P1+P2) | T]).
compact_c([A | T]) = [A | compact_c(T)].
compact_c([]) = [].
*/
:- func norm_c(comp) = comp.
norm_c(C) = compact_c(sort_c(C)).

main -->
	{
	 P = mul([[num(1),var(x,1)], [num(2),var(y,1),var(z,1)]], [[num(3),var(z,1)]])
	},
	print(P).


