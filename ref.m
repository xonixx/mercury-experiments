:- module ref.

:- interface.

:- import_module io.
%:- import_module list.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.


:- type a_type ---> a(int, int).

:- inst a_inst ---> a(ground, free).
:- mode a_mode == a_inst >> ground.
/*
:- pred f(a_type).
:- mode f(a_mode) is semidet.

%f(A) :-
%	A = a(B,B).
f(a(B,B)).
*/
:- pred g(a_type).
:- mode g(in) is semidet.
g(a(B, B)).

main -->
	 {
	 (
	  g(a(7,Q1))
	 ->
	  Q = Q1
	 ;
	  Q = -1
	 )
	 },
	io.write_int(Q),
	io.nl.
