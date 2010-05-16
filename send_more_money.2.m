:- module send_more_money.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module int.
:- import_module solutions.
:- import_module set.

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
	; !:Unif = !.Unif
	).

%problem(_, 0, Unif,Unif, _,_,"").

:- pred take_num(li,li, lt, lt, string, string, int).
:- mode take_num(in,out, in,out, in,out, out).
take_num(!NN, !Unif, !Word, N) :-
	(B = last(!.Word, !:Word)
	->
	 (N0 = unif(B, !.Unif)
	 ->
	  N = N0,
	  !:Unif = !.Unif
	 ;
	  oneof(!.NN, N, !:NN),
	  !:Unif = [{B,N} | !.Unif]
	 )
	;N = 0, !:Word = "", !:Unif = !.Unif
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

main -->
	{
	solutions_set((pred(Unif::out) is nondet :-
		       %problem([1,2,3,4,5,6,7,8,9,0],0,[], Unif,"A","A","B"))
			problem([1,2,3,4,5,6,7,8,9,0],0,[], Unif,"SEND","MORE","MONEY"))
		      ,Set
		      )

/*	
	 solutions_set((pred(Rest::out) is nondet :-
		      digit([1,2,3,4],_,Rest)
		      ),
		      Set)*/
	 },
	io.write_list(to_sorted_list(Set), "\n", io.write)
	.