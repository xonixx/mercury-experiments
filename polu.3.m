:- module poly.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, int.

:- type poly == list(comp). % polynome
:- type comp ---> comp(koef, lvp). % component
:- type lvp == list(vp).
:- type vp ---> var(var, pwr).
:- type pwr == int.
:- type koef == int.
:- type var ---> x; y; z.

:- func null = poly.
null = [].

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
mul_c_c(comp(K1, L1), comp(K2, L2)) = comp(K1 * K2, (norm_lvp(append(L1, L2)))).

:- func compare_vp(vp, vp) = comparison_result.
compare_vp(var(A,_),var(B,_)) = ordering(A, B).

:- func sort_lvp(lvp) = lvp. 
sort_lvp(L) = sort(compare_vp, L).

:- func compact_lvp(lvp) = lvp.
compact_lvp(L) = R :-
	( L = [var(A,P1),var(A, P2) | T] ->
	  R = compact_lvp([var(A, P1 + P2) | T])
	;
	  L = [A | T] ->
	  R = [A | compact_lvp(T)]
	;
	  R = []
	).

:- func norm_lvp(lvp) = lvp.
norm_lvp(L) = compact_lvp(sort_lvp(L)).

/*
:- func compare_c(comp, comp) = comparison_result.
compare_c([_|V1], [_|V2]) = compare_c_vars(V1, V2).

:- func compare_c_vars(comp, comp) = comparison_result.
compare_c_vars([], [_|_]) = (<).
compare_c_vars([_|_], []) = (>).
compare_c_vars([V1|T1], [V2,T2])

:- func sort_p(poly) = poly.
sort_p(P) = sort(compare_c, P).
*/
main -->
	{
	 P = mul( [ comp(1, [var(x,1)]), comp(2,[var(y,1),var(z,1)])],
		  [ comp(3, [var(z,1)])])
	},
	print(P).


