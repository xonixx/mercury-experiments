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


:- pred a(int).
:- mode a(out) is multi.

a(1).
a(2).
a(3).

main --> { solutions(a, AA) }, print(AA).

main0 -->
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
	nl,
	print("=========="),nl,
	( { memb(6,[2,3,7,8]) } ->
	  print("Угу")
	; { true }
	), print("..."), nl,
	( { memb(7,[2,3,7,7]) } ->
	  print("Угу")
	; { true }
	), print("..."), nl,
	( {solutions((pred(X::out) is nondet :-
		    memb(X, [2,3,7,7,7]), X = 7
		    ), Ugus
		   )
	  }, write_list(Ugus, ":", print), nl

	 ),
/*
	{promise_equivalent_solutions [] ( unsorted_aggregate(( pred(X::out) is nondet :-
		    memb(X, [2,3,7,7,6])),
		    ( pred(X::in, di, uo) is det -->
		      print(X),
		      ({X > 2} ->
		      print("yes ");
		      {true}))
		    )

	 )
	}
*/
	{ X=Y, Z=Y, Z=Q, 123=Q }, print(X)
	.

/*
run():-init(),
    (member1(6,[2,3,7,7]),write("Угу "),fail;write("...")),nl,
    (member1(7,[2,3,7,7]),write("Угу "),fail;write("...")),nl,
    (member2(6,[2,3,7,7]),write("Угу "),fail;write("...")),nl,
    (member2(7,[2,3,7,7]),write("Угу "),fail;write("...")),nl,
    (member2(X,[2,3,7,7]),write(X),fail;write("...")).
Вот результат:
Цитата
...
Угу ...
...
Угу Угу ...
2377...
*/