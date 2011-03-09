:- module eul118.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, float, math, list, string, bool.

:- pragma memo(prime/1).

:- mode prime(in) is semidet.
prime(N) :- 
	(	N = 2
	;
		Up = float.ceiling_to_int(math.sqrt(float.float(N))),
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
	test_prime(2,yes),
	test_prime(7,yes),
	test_prime(29,yes),
	test_prime(6,no),
	test_prime(111,no),
	test_prime(111,no),
	test_prime(49,no)
	.
	
main -->
	test_primes
	.
	