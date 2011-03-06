:- module fib.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list, string.

%:- pragma memo(fib/1).

:- mode fib(in) = out is det.
fib(N) = 
	(	N=0->1
	;	N=1->1
	;	fib(N-1) + fib(N-2)
	).

main --> command_line_arguments(Args), 
	(	{Args=[Arg|_]},	
		print(fib(det_to_int(Arg)))
	;
		{Args=[]},
		write_string("Usage: fib N")
	).