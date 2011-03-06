:- module fibbig.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module integer, list, string.

:- pragma memo(fib/1).

:- mode fib(in) = out is det.
fib(N) = Res :-
	(	N=zero ->
		Res = one
	;	
		N=one ->
		Res = one
	;	
		Prev1 = N - one,
		Prev2 = Prev1 - one,
		Res = fib(Prev1) + fib(Prev2)
	).

main --> command_line_arguments(Args), 
	(	{Args=[Arg|_]},	
		print(to_string(fib(det_from_string(Arg))))
	;
		{Args=[]},
		write_string("Usage: fib N")
	).