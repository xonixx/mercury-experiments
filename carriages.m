:- module carriages.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, require, string.

:- type carriage ---> a; b.
:- type carriages == list(carriage).
:- type state == {carriages,carriages,carriages}.
:- type move ---> 
	left_to_middle;
	left_to_right;
	middle_to_right;
	done.


step({[], [], R}, {[], [], R}, done).
step({[A|AA], [], R}, {AA, B1, R1}, M) :- 
	( R \= [A|_] -> 
	  B1 = [], R1 =  [A|R], M = left_to_right
	; B1 = [A], R1 = R, M = left_to_middle
	).
step({[], [B|BB], R}, {[], BB, [B|R]}, middle_to_right) :- R \= [B|_].
step({A0 @ [A|AA], B0 @ [B|BB], C0 @ [C|_]}, {A1, B1, C1}, M) :-
	( A = C	->
	  ( B = C ->
  	    A1 = AA, B1 = [A|B0], C1 = C0, M = left_to_middle
	  ; A1 = A0, B1 = BB, C1 = [B|C0], M = middle_to_right
	  )
	; A1 = AA, B1=B0, C1=[A|C0], M = left_to_right
       ).

%:- mode solve(in, out, di, uo) is det.
solve(Inp, Out) -->
	( { step(Inp, S1, Movement) }
	  ;
	  {error("No way! " ++ string(Inp))}
       	),
	print(Movement), 
	nl,
	( { Movement = done } -> 
	  { Out = S1 }
	  ; 
	  solve(S1, Out)
	).

main -->
	solve({[a:carriage,a,a,a,b,b,b,b], [], []}, {_,_,R}),
	print(R), nl.
	
