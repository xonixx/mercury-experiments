:- module fibotest.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module fibonacci_tr.

main -->
	{N = 50},
	io.write_string("fibonacci(N) = "),
	io.write_int(fibonacci(N)),
	io.nl.
