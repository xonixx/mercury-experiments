:- module io1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(IO0, IO) :-
	write_string("4", IO3, IO4),
	write_string("1", IO0, IO1),
	write_string("3", IO2, IO3),
	write_string("2", IO1, IO2),
	write_string("5", IO4, IO).
