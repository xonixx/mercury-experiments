:- module queens.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list, solutions, logic, int.

:- type pos ---> pos(int, maybe(int)).

:- instance unifiable(pos) where [
	func(unify/2) is unify_pos
].

:- func unify_pos(pos, pos)=pos is semidet.
unify_pos(pos(X, Y), pos(X, Y1)) = pos(X,unify(Y, Y1)).

solve(!:S):-
	!:S=[pos(1,no),pos(2,no),pos(3,no),pos(4,no),pos(5,no),pos(6,no),pos(7,no),pos(8,no)],
	solution(!S).

solution([], []).
solution([pos(!.X,!.Y) | !.Others ], [ pos(!:X, !:Y) | !:Others ]) :-
        solution(!Others),
        logic.member(!Y, [yes(1), yes(2), yes(3), yes(4), yes(5), yes(6), yes(7), yes(8)],_),
        nokill(pos(!.X, !.Y), !.Others).
                  
nokill(_, []).                                 
nokill(pos(X, yes(Y)), [pos(X1, yes(Y1)) | Others]) :-
	Y \= Y1, % на разных горизонталях     
        Y1-Y \= X1-X, % на разных диагоналях
        Y1-Y \= X-X1,
        nokill( pos(X, yes(Y)), Others).

main -->
	{ solutions(pred(S::out) is nondet :- solve(S), L)},
	print(L),
	print("\nTotal solutions: "),
	print(length(L):int).