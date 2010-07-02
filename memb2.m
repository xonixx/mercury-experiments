:- module memb2.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, solutions, int.

:- pred memb(T, list(T)).
:- mode memb(in, in) is semidet.
:- mode memb(out, in) is nondet.
memb(X,[X|_]).
memb(X,[_|L]):-memb(X,L).

:- pred print_ugu_on_every_N(T::in, list(T)::in, io::di, io::uo) is det.
print_ugu_on_every_N(N, [H | T]) -->
	( {H = N} ->
	  print("Ugu")
	;
	  []
	),
	print_ugu_on_every_N(N, T).
print_ugu_on_every_N(_, []) --> [].

main -->
	( { memb(6,[2,3,7,8]) } ->  print("Ugu") ; [] ), print("..."), nl,

	( { memb(7,[2,3,7,7]) } ->  print("Ugu") ; [] ), print("..."), nl,

	( { memb(6,[2,3,7,7]) } ->  print("Ugu") ; [] ), print("..."), nl,

	print_ugu_on_every_N(7, [2,3,7,7]), print("..."), nl,

	io.write_list([2,3,7,7],"",io.write_int), print("...")

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