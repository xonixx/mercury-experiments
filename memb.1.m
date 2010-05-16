:- module memb.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, solutions, int.

:- pred member_semidet(T, list(T)).
:- mode member_semidet(in, in) is semidet.
:- mode member_semidet(out, in) is nondet.
member_semidet(X, [Y|L]) :-
	( X = Y
	; member_semidet(X, L)
	).

:- pred member_nondet(T, list(T)).
:- mode member_nondet(in, in) is semidet.
:- mode member_nondet(out, in) is nondet.
member_nondet(X,[X|_]).
member_nondet(X,[_|L]):-member_nondet(X,L).


:- pred memb(T, list(T)).
:- mode memb(in, in) is semidet.
:- mode memb(out, in) is nondet.
memb(X,[X|_]).
memb(X,[_|L]):-memb(X,L).

:- type yes_no ---> yes; no.

main -->
	{
	 ( member_semidet(2, [1,2,3]) ->
	   A = yes
	 ; A = no
	 ),
	 ( member_nondet("qqq", ["q", "qq", "bbb"]) ->
	   B = yes
	 ; B = no
	 ),
	 ( solutions((pred(Elem::out) is nondet :-
		     member(Elem, 0..20),
		      Elem mod 7 = 0
		     ), Elems) 
	 )
	},
	print(A), % yes
	nl,
	print(B), % no
	nl,
	print(Elems), % [0, 7, 14]
	nl
	.
