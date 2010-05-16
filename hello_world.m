:- module hello_world.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

main(!IO) :-
	io.write_string("Hello, World!\n", !IO).
