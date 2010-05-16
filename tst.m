:- module tst.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.

%:- type a_type ---> a.

:- func unif(K,list({K,V})) = V.
:- mode unif(in, in) = out is semidet.

unif(K,[H|T]) = V :-
	(H = {K, V1}
	-> V = V1
	; V = unif(K, T)
	).

main -->
	{
	 (A1 = unif(2, [{1,"111"}, {2, "222"}, {3, "333"}])
	 -> A = A1
	  ; A = "000"
	  )
	},
	io.print(A),
%	io.write_list(["a", "b", "c"], "\n", (pred(S::in, in, out) --> write_string(S))).
	%{P = (pred(S::in, in, out) --> write_string(S))},
	%io.write_list(["a", "b", "c"], "\n", P).