:- module y1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- type mu(A) ---> roll(unroll :: (func(mu(A)) = A)).

y(F) = F1(roll(F1)) :- F1 = (func(X) = (func(A) = F(unroll(X)(X))(A))).

y_fac = y(func(F) = (func(N) = (N =< 0 -> 1; N * F(N-1)))).
y_fib = y(func(F) = (func(N) = (N < 2 -> N; F(N-1) + F(N-2)))).

%% let fix f g = (fun x a -> f (x x) a) (fun x a -> f (x x) a) g

%%%fix(F) = F1(F1) :- F1 = (func(X) = (func(A) = F((X)(X))(A))).
%%%fix_fac =  fix(func(F) = (func(N) = (N =< 0 -> 1; N * F(N-1)))).

main --> 
	write_int((y_fac)(10)), % 3628800
	nl,
	write_int((y_fib)(15)).
	
%main --> write_int((fix_fac)(10)). % 3628800