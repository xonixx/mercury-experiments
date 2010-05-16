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
mul(P1, P2) = norm_p(mul1(null, P1, P2)).

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


:- func compare_c(comp, comp) = comparison_result.
compare_c(comp(_,L1), comp(_,L2)) = compare_lvp(L1, L2).

:- func compare_lvp(lvp, lvp) = comparison_result.
compare_lvp([], []) = (=).
compare_lvp([], [_|_]) = (<).
compare_lvp([_|_], []) = (>).
compare_lvp([var(V1, P1) | T1], [var(V2, P2) | T2]) = R :-
	( V1 = V2 ->
	  ( P1 = P2 ->
	    R = compare_lvp(T1, T2)
	  ;
	    R = ordering(P1, P2)
	  )
	;
	  R = ordering(V1, V2)
	).

:- func sort_p(poly) = poly.
sort_p(P) = sort(compare_c, P).

:- func norm_p(poly) = poly.
norm_p(P) = compact_p(sort_p(P)).

:- func compact_p(poly) = poly.
compact_p([]) = [].
compact_p([C]) = [C].
compact_p([C1, C2 | CC]) = R :-
	C1 = comp(K1, L1),
	C2 = comp(K2, L2),
	( L1 = L2 ->
	  R = compact_p([comp(K1 + K2, L1) | CC])
	;
	  R = [C1 | compact_p([C2 | CC])]
	).

:- func one = comp.
one = comp(1, []).

:- func one_p = poly.
one_p = [one].

:- func simple(var, pwr) = comp.
simple(X, P) = comp(1, [var(X, P)]).

:- func pwr(poly, int) = poly.
pwr(P, I) = pwr1(one_p, P, I).

:- func pwr1(poly, poly, int) = poly.
pwr1(P0, P, I) = ( I > 0 -> pwr1(mul(P0, P), P, I-1); P0).

main -->
	{
	 P = mul( [ comp(1, [var(x,1)]), comp(2,[var(y,1),var(z,1)])],
		  [ comp(3, [var(z,1)])]),
	 Q = mul( [comp(1,[])], [
		     comp(1,[var(x,3)]),
		     comp(2,[var(y,2),var(z,4)]),
		     comp(3,[var(x,3)]),
		     comp(4,[var(z,4),var(y,2)])
		     ]),
	 %R = pwr([one, comp(1,[var(x,1)])],40)
	 R = pwr([one, simple(x,1), simple(y, 1), simple(z, 1)], 20)
	},
	print(P),nl,
	print(Q),nl,
	print(R).


