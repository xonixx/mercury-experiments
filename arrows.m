:- module arrows.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module list, io, int.

:- type arrow ---> (->); (<-).

:- mode solution(in, in, in, in, out, in) is cc_nondet.
solution(State0, Target, MaxDepth, CurrentDepth) -->
	{CurrentDepth =< MaxDepth},
    (    { State0 = Target } -> []
    ;    { step(State0, State1) },
         [State1],
         solution(State1, Target, MaxDepth, CurrentDepth + 1)
    ).

flip(->, <-).
flip(<-, ->).

step([], []).
step([A|Rest0], [A|Rest]) :- step(Rest0, Rest).
step([A0,A1|Rest], [B0,B1|Rest]) :- flip(A0, B0), flip(A1, B1).

main -->
	(({
	member(N, 1..10),
       	solution([->,<-,->,<-,->,<-], [<-,<-,<-,->,->,->], N, 0, Solution, [])
	}) 
	-> print(Solution)
	; print("No solutions")
	).	
