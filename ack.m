:- module ack.

:- interface.

:- import_module io.
:- import_module int.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- pred ack_p(int, int, int).
:- mode ack_p(in, in, out) is det.

:- func ack_f(int, int) = int.
:- mode ack_f(in, in) = out is det.

:- implementation.

/*
ack0(0,X,X + 1).
ack0(X,0,A) :- ack0(X-1,1,A).
ack0(X,Y,A) :- ack0(X,Y-1,B), ack0(X-1,B,A).
*/

ack_p(X, Y, A) :-
	( X = 0
	-> A = Y + 1
	; Y = 0
	-> ack_p(X-1, 1, A)
	; ack_p(X,Y-1,B), ack_p(X-1,B,A)
	).
	
/*	
ack_f(0,X)=X+1.
ack_f(X,0)=ack_f(X-1,1).
ack_f(X,Y)=ack_f(X-1,ack_f(X,Y-1)).	
*/

ack_f(X, Y) = R :-
	( X = 0
	-> R = Y + 1
	; Y = 0
	-> R = ack_f(X-1,1)
	; R = ack_f(X-1,ack_f(X,Y-1))
	).
	
/*
class facts
tr : tree{tuple{unsigned, unsigned}, unsigned} := emptyUnique.

class predicates
ackerman: (unsigned, unsigned) -> unsigned.

clauses
ackerman(0, Y)= Y + 1:- !.
ackerman(X, Y)= R:- 
    R = tryLookUp(tr, tuple(X, Y)), !.
ackerman(X, 0) = R :- !,
    R = ackerman(X - 1, 1), 
    tr := insert(tr, tuple(X - 1, 1), R). 
ackerman(X, Y) = R1:- 
    R = ackerman(X, Y - 1),
    R1 = ackerman(X - 1, R),
    tr := insert(tr, tuple(X - 1, R), R1).
*/

:- import_module map.

:- type ack_map == map({int, int}, int).

:- func ack_memoized_map(int, int, ack_map, ack_map) = int.
:- mode ack_memoized_map(in, in, in, out) = out is det.

ack_memoized_map(X, Y, MapIn, MapOut) = R :-
	T = {X, Y},
	(	E = map.elem(T, MapIn)
	->	R = E, MapIn = MapOut
	;	(	X = 0
		-> 	R = Y + 1, M = MapIn
		; 	Y = 0
		-> 	R = ack_memoized_map(X-1, 1, MapIn, M)
		; 	R = ack_memoized_map(X-1,
						ack_memoized_map(X, Y-1, MapIn, M1),
						M1, M)
		),	map.det_insert(M, T, R, MapOut)
	).


:- type ack_mm == map(int, map(int, int)).

:- pred ack_memoized_mm(int, int, ack_mm, ack_mm, int).
:- mode ack_memoized_mm(in, in, in, out, out) is det.

ack_memoized_mm(X, Y, !Map, R) :-
	(	E = !.Map ^ elem(X) ^ elem(Y)
	->	R = E, !:Map = !.Map
	;	(	X = 0
		-> 	R = Y + 1, !:Map = !.Map
		; 	Y = 0
		-> 	ack_memoized_mm(X-1, 1, !Map, R)
		;
			ack_memoized_mm(X, Y-1, !Map, R1),
			ack_memoized_mm(X-1, R1, !Map, R)
		),
		map_ins_2(X, Y, R, !Map)
	).

:- pred map_ins_2(int, int, int, ack_mm, ack_mm).
:- mode map_ins_2(in, in, in, in, out) is det.
map_ins_2(K1, K2, V, !M) :-
 	(   M1 = !.M ^ elem(K1)
	->
	    !:M = map.det_update(!.M, K1, map.det_insert(M1, K2, V))
	;
	    !:M = det_insert(!.M, K1,map.det_insert(map.init, K2, V)) 
	).

:- func ack_memoized(int, int) = int.
:- mode ack_memoized(in, in) = out is det.
ack_memoized(X, Y) = ack_memoized_map(X, Y, M0, _) :- map.init(M0).

:- pred ack_p_memo(int, int, int).
:- mode ack_p_memo(in, in, out) is det.
:- pragma memo(ack_p_memo/3). % <--- крутая штука
ack_p_memo(X, Y, A) :-
	( X = 0
	-> A = Y + 1
	; Y = 0
	-> ack_p_memo(X-1, 1, A)
	; ack_p_memo(X,Y-1,B), ack_p_memo(X-1,B,A)
	).


main -->
	{
	%ack_f(4,1)=R			% 0m9.516s
	%ack_memoized(4,1)=R	% 0m1.078s
	%ack_p_memo(4,1,R)		% 0m0.078s
	map.init(M0), ack_memoized_mm(4, 1, M0, _, R)
	},
	io.write_int(R),
	io.nl.