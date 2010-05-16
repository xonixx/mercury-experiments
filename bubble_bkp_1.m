
:- module bubble.
:- interface.
:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module int.
:- import_module list.


bubble(L,X):- 
	(	up(L,L1,0)
	->	bubble(L1,X)
	;	X = L
	).

/*
up([X,Y|L],[Y|L1],_):-X>Y,!,up([X|L],L1,1).
up([X,Y|L],[X|L1],B):-!,up([Y|L],L1,B).
up(L,L,1).
*/

%:- mode up(in, out, in).
up(M1, M2, Inp) :-
	(	M1 = [X,Y|L] 
	->	(	X>Y
		->	up([X|L],L1,1), M2=[Y|L1]
		;	up([Y|L],L1,Inp), M2=[X|L1]
		)
	;	M1 = M2, Inp=1
	).

gen(N,[N|L]):- 
	(	N=1
	->	L = []
	;	M=N-1,
		gen(M,L)
	).

main -->
	{N = 1000
	,gen(N, L1), bubble(L1, L)
	},
	io.write_list(L,", ", io.write_int),
	io.nl.