:- module send_more_money.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.
/*
Domains
IL = Integer*
Пара = п(String,Integer)
Пары = Пара*
predicates
nondeterm шарада(IL,Integer,Пары,String,String,String)
nondeterm взять(IL,Пары,String,Integer,String,Пары,IL)
послед(String,String,String)
nondeterm униф(String,Пары,Integer)
nondeterm цифра(IL,Integer,IL)
сумма(Integer,Integer,Integer,Integer,Integer)
  
шарада(_,0,Униф,"","",""):- write(Униф),nl.
шарада(Цифры,Перенос,Униф,Слово1,Слово2,Слово3):-Слово3<>"",
    взять(Цифры,Униф,Слово1,Ц1,С1,Униф1,Цифры1),
    взять(Цифры1,Униф1,Слово2,Ц2,С2,Униф2,Цифры2),
    взять(Цифры2,Униф2,Слово3,Ц3,С3,Униф3,Цифры3),
    сумма(Перенос,Ц1,Ц2,Ц3,Перенос1),
    шарада(Цифры3,Перенос1,Униф3,С1,С2,С3).

взять(Цифры,Униф,Слово,Ц,"",Униф,Цифры):-  % Слово=Б
    str_char(Слово,_),униф(Слово,Униф,Ц),Ц>0,!.
взять(Цифры,Униф,Слово,Ц,Слово1,Униф,Цифры):-
    послед(Слово,Б,Слово1),not(Слово1=""),униф(Б,Униф,Ц),!.
взять(Цифры,Униф,Слово,Ц,Слово1,[п(Б,Ц)|Униф],Цифры1):-
    послед(Слово,Б,Слово1),цифра(Цифры,Ц,Цифры1).
взять(Цифры,Униф,"",0,"",Униф,Цифры):- !.

послед(Слово,Б,Слово1):-str_len(Слово,Длина),Длина>0,
    substring(Слово,Длина,1,Б),concat(Слово1,Б,Слово).

униф(Б,[п(Б,Ц)|_],Ц):- !.
униф(Б,[_|Униф],Ц):-униф(Б,Униф,Ц).

цифра([Цифра|Цифры],Цифра,Цифры).
цифра([Цифра1|Цифры],Цифра,[Цифра1|Остаток]):- цифра(Цифры,Цифра,Остаток).

сумма(Перенос,Ц1,Ц2,Ц3,0):- Перенос+Ц1+Ц2=Ц3,!.
сумма(Перенос,Ц1,Ц2,Ц3,1):- Перенос+Ц1+Ц2=Ц3+10.
 */
:- import_module list.
:- import_module string.
:- import_module int.
:- import_module solutions.

:- type listi == list(int).
:- type listt == list({string, int}).

:- pred problem(listi, int, listt,listt, string, string, string).
:- mode problem(in, in, in,out, in, in, in) is nondet.

problem(!.Nums,!.Carry,!Unif,!.Word1,!.Word2,!.Word3):-
	!.Word3 \= "",
	take_num(!Nums, !Unif, !Word1, N1),
	take_num(!Nums, !Unif, !Word2, N2),
	take_num(!Nums, !Unif, !Word3, N3),
	sum(!.Carry,N1,N2,N3) = !:Carry,
	problem(!.Nums,!.Carry,!Unif,!.Word1,!.Word2,!.Word3).

problem(_, 0, Unif,Unif, _,_,_).

  /*
problem(Nums0,Carry0,Unif0, Unif,Word1,Word2,Word3):-Word3 \= "",
    take_(Nums0,Unif0,Word1,N1,W1,Unif1,Nums1),
    take_(Nums1,Unif1,Word2,N2,W2,Unif2,Nums2),
    take_(Nums2,Unif2,Word3,N3,W3,Unif3,Nums3),
    sum(Carry0,N1,N2,N3,Carry1),
    problem(Nums3,Carry1,Unif3,Unif,W1,W2,W3).*/
/*
take_(NN,Unif,Word,N,"",Unif,NN):-  % Слово=Б
    length(Word)=1,unif(Word,Unif,N),N>0.
take_(NN,Unif,Word,N,Word1,Unif,NN):-
    last(Word,B,Word1),not(Word1=""),unif(B,Unif,N).
take_(NN,Unif,Word,N,Word1,[{B,N}|Unif],NN1):-
    last(Word,B,Word1),digit(NN,N,NN1).
take_(NN,Unif,"",0,"",Unif,NN).
*/



:- pred take_num(listi,listi, listt, listt, string, string, int).
:- mode take_num(in,out, in,out, in,out, out).
take_num(!NN, !Unif, !Word, N) :-
	B = last(!.Word, !:Word),
	(N0 = unif(B, !.Unif)
	->
	 N = N0,
	 !:Unif = !.Unif
	;
	 oneof(!.NN, N, !:NN),
	 !:Unif = [{B,N} | !.Unif]
	 ).
	


:- func last(string,string) = string.
:- mode last(in, out) = out is semidet.
last(W,W1) = B:-
	length(W)=L,
	L>0,
	right(W, 1)=B,
	left(W, L-1) = W1. 
/*
unif(B,[{B,N}|_],N).
unif(B,[_|Unif],N):-unif(B,Unif,N).
*/

:- func unif(K,list({K,V})) = V.
:- mode unif(in, in) = out is semidet.

unif(K,[H|T]) = V :-
	(H = {K, V1}
	-> V = V1
	; V = unif(K, T)
	).
/*
:- mode unif(in,in,out) is semidet.
unif(B, [H | T], N) :-
	(H={B,N1}
	-> N1 = N
	; unif(B, T, N)
	).
*/

:- pred oneof(listi, int, listi).
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
%sum(Carry,N1,N2,N3,1) :- Carry+N1+N2=N3+10.

main -->
	{
	 %list.append([1], [2,3], [A,B,C])
	%; B = -1
	% "Переменная" = 123
	% _A = 1

	solutions_set((pred(Unif::out) is nondet :-
		       problem([1,2,3,4,5,6,7,8,9,0],0,[], Unif,"A","A","B"))
				%problem([1,2,3,4,5,6,7,8,9,0],0,[], Unif,"SEND","MORE","MONEY"))
		      ,Set
		      )

/*	
	 solutions_set((pred(Rest::out) is nondet :-
		      digit([1,2,3,4],_,Rest)
		      ),
		      Set)*/
	 },
	io.print(Set)%,
	% {
	%  last("ABCD",B,W1); B="",W1=""
	%  },
	% io.print([B, W1])
	%io.write_string("123")
	%io.write_string(list.list_to_doc([1,2,3]))
	.