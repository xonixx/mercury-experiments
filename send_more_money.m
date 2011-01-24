:- module send_more_money.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, int, solutions.

:- type li == list(int).
:- type lt == list({string, int}).

:- pred problem(li, int, lt,lt, string, string, string).
:- mode problem(in, in, in,out, in, in, in) is nondet.
problem(!.Nums,!.Carry,!Unif,!.Word1,!.Word2,!.Word3):-
	(!.Word3 \= ""
	->
	 take_num(!Nums, !Unif, !Word1, N1),
	 take_num(!Nums, !Unif, !Word2, N2),
	 take_num(!Nums, !Unif, !Word3, N3),
	 sum(!.Carry,N1,N2,N3) = !:Carry,
	 problem(!.Nums,!.Carry,!Unif,!.Word1,!.Word2,!.Word3)
	;
	 !.Carry = 0,
	 !:Unif = !.Unif
	).

:- pred take_num(li,li, lt, lt, string, string, int).
:- mode take_num(in,out, in,out, in,out, out) is nondet.
take_num(!NN, !Unif, !Word, N) :-
	(B = last(!.Word, !:Word)
	->
	 (N0 = unif(B, !.Unif)
	 ->
	  N = N0,
	  !:Unif = !.Unif
	 ;
	  oneof(!.NN, N, !:NN),
	  !.Word = "" => N \= 0, % first digit not 0
	  !:Unif = [{B,N} | !.Unif]
	 )
	; 
	N = 0, !:Word = "", !:Unif = !.Unif
	).

:- func last(string,string) = string.
:- mode last(in, out) = out is semidet.
last(W,W1) = B:-
	length(W) = L,
	L>0,
	right(W, 1) = B,
	left(W, L-1) = W1. 

:- func unif(K,list({K,V})) = V.
:- mode unif(in, in) = out is semidet.
unif(K,[H|T]) = V :-
	(H = {K, V1}
	-> V = V1
	; V = unif(K, T)
	).

:- pred oneof(li, int, li).
:- mode oneof(in, out, out) is nondet.
oneof([N|NN], N, NN).
oneof([N1|NN], N, [N1|Rest]) :- oneof(NN,N,Rest).

:- func sum(int,int,int,int) = int.
:- mode sum(in, in, in, in) = out is semidet.
sum(Carry,N1,N2,N3) = Out :-
	(Carry+N1+N2=N3
	-> Out = 0
	; Carry+N1+N2=N3+10
	-> Out = 1
	; fail
	).

:- func word_to_digits(string, lt) = string.
:- mode word_to_digits(in, in) = out is det.
word_to_digits(W, U) = Res :-
	(first_char(W,C,W1)
	->
	 char_to_string(C,K),
	 (D0 = unif(K, U)
	 -> D = int_to_string(D0)
	 ; D = "?"
	 ),
	 Res = D ++ word_to_digits(W1,U)
	; Res = "" 
	).

:- pred write_sols(list(lt), string, string, string, io, io).
:- mode write_sols(in, in, in, in, di, uo) is det.
write_sols([S | SS], W1,W2,W3) -->
 	format("Solved:  %s+%s=%s\n", [
 					  s(word_to_digits(W1, S)),
 					  s(word_to_digits(W2, S)),
 					  s(word_to_digits(W3, S))
 					 ]),
	write_sols(SS, W1,W2,W3).
write_sols([],_,_,_) --> [].

:- pred solve(string,string,string,io,io).
:- mode solve(in,in,in,di,uo) is det.
solve(W1, W2, W3) -->
	format("Solving: %s+%s=%s\n", [s(W1), s(W2), s(W3)]),
	{
	 solutions((pred(Unif::out) is nondet :-
			problem([1,2,3,4,5,6,7,8,9,0], 0, [], Unif, W1,W2,W3))
		      ,L
		      )
	 },
	write_sols(L, W1, W2, W3).

main -->
	solve("SEND", "MORE", "MONEY"),
	solve("DONALD","GERALD","ROBERT"),
	solve("GREA", "TYOU", "DIDIT").