:- module eul118.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, float, math, list, string, bool, solutions, set.

:- pragma memo(prime/1).

:- mode prime(in) is semidet.
prime(N) :- 
	(	N = 2
	;
		N \= 1,
		Up = ceiling_to_int(math.sqrt(float(N))),
		not_divides(2, Up, N)
	).
	
not_divides(D, Up, N) :- 
	(	D > Up
	;	
		N mod D \= 0,
		not_divides(D+1, Up, N)
	).


test_prime(N, Res) -->
	print(N), print(Res=yes->" (prime)"; " (not prime)"), print(" - "), 
	(	{prime(N)} ->
		(	{Res = yes} ->
			print("ok")
		;
			print("fail")
		)
	;
		(	{Res = yes} ->
			print("fail")
		;	
			print("ok")
		)
	),
	nl.

test_primes --> 
	test_prime(1,no),
	test_prime(2,yes),
	test_prime(7,yes),
	test_prime(29,yes),
	test_prime(6,no),
	test_prime(111,no),
	test_prime(111,no),
	test_prime(49,no)
	.
	
	
digits_to_num([]) = 0.	
digits_to_num([D]) = D.
digits_to_num([H1, H2|T]) = digits_to_num([H1 * 10 + H2|T]).


%~ count_cases([], 1, S, S).
%~ count_cases([H|T], R, S0, S) :- count_cases([H], T, R, S0, S).

%~ count_cases(L, V, R, S0:set(int), S:set(int)) :-
	%~ D = digits_to_num(L),
	%~ (	prime(D) ->
		%~ (	contains(S0, D) ->
			%~ R0 = 0,
			%~ S1 = S0
		%~ ;
			%~ count_cases(V, R0, insert(S0, D), S1)
		%~ )
	%~ ;
		%~ R0 = 0,
		%~ S1 = S0
	%~ ),
	%~ (	V = [],
		%~ R = R0,
		%~ S = S1
	%~ ;
		%~ V = [H|T],
		%~ count_cases(L ++ [H], T, R1, S1, S),
		%~ R = R0 + R1
	%~ ).
	
digits = [1,2,3,4,5,6,7,8,9].	
% digits = [3,2].	

empty_set = set.init:set(int).

prime_sets([], empty_set).
prime_sets([H|T], Set) :- prime_sets([H], T, empty_set, Set).

prime_sets(L0, L, S0, S) :-
	D = digits_to_num(L0),
	(	prime(D),
		S1 = insert(S0, D),
		prime_sets(L, S2),
		union(S1, S2, S)
	)
	;
	(	L=[H|T],
		prime_sets(L0 ++ [H], T, S0, S)
	%;
	%	L=[],
	%	S = S0
	).

%(pred(V::in, Acc0::in, Acc::out) is det :-
%	Acc = Acc0 + 1),	

:- mode all_prime_sets(in, out) is nondet.
all_prime_sets(Digits, Set) :-
	list.perm(Digits, Permutated),
	prime_sets(Permutated, Set).

solve -->
	{solutions(all_prime_sets(digits), Sets)},
	%print(Sets),
	write_int(length(Sets)).
	%~ {promise_equivalent_solutions [Res] unsorted_aggregate(list.perm(digits), 
		%~ (pred(V::in, {S0, Acc0}::in, {S, Acc}::out) is det :-
			%~ prime_sets(V, Set),
			%~ Acc = Acc0 + R
			%~ ),
			%~ {set.init, 0},
			%~ {_,Res})},
	%~ print(Res).
	
main -->
	% test_primes
	% print(digits_to_num([1,2,3,4,5]))
	solve
	.
	