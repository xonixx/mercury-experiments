:- module a.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, char, float, list, require, math, string.

number_chars(N, [D|DD]) :- number_chars(N, D, DD).
number_chars(0, []).

number_chars(N, N, []).
number_chars(N, N1, [D1|DD]) :- number_chars(N, N1 * 10 + D1, DD).

:- mode integer(out, in, out) is nondet.
integer(I) -->
        digit(D0),
        digits(D),
        { number_chars(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D),
        digits(T).
digits([]) -->
        [].

:- mode digit(out, in, out) is nondet.
digit(N) -->
        [D],
        { char.int_to_digit(N, D)
        }.

:- pred float(float::out, list(char)::in, list(char)::out) is nondet.
float(F) -->
	(	
		['-'], {Sign = -1.0}
	;   
		[], {Sign = 1.0}
	),
	integer(N),
	[','],
	integer(D),
	{F = Sign * (float(N + D) / math.pow(10.0, math.ceiling(log10(float(D)))))
	}.

:- mode sum(out, in, out) is nondet.
sum(S) -->
	float(F1:float),
	[' '],
	(   sum(S1:float)
	;   {S1 = 0.0}
	),
	{ S = F1 + S1}.

:- mode sum(in, out, in, out) is nondet.
sum(S, Total) -->
	float(F1),
	[' '],
	{ S1 = S + F1},
	sum(S1, Total).
sum(Total, Total) -->
	[].

/*
go :-
	read_file_to_codes('numbers_large.txt',Codes,[]),
	writeln('Processing...'),
	sum(S,Codes,[]),
	writeln(S).

go1 :-
	phrase_from_file(sum(0, S),'numbers_large.txt', [buffer_size(16384)]),
	writeln(S).
*/

get_chars_from_current_stream(Chars) -->
	read_file(Result),
	{	Result = ok(Chars)
	;	
		Result = error(_,Error),
		error(error_message(Error))
	}.

main(!IO) :-
	see("D:/numbers_large.txt", Result, !IO),
	(	Result = ok,
		get_chars_from_current_stream(Chars, !IO),
		seen(!IO),

		promise_equivalent_solutions [S1]
		(	sum(0.0, S, Chars, [])
		->	
			S1 = S
		;	
			error("WTF")
		),
		write(S1, !IO),
		nl(!IO)
	;	
		Result = error(Error),
		write_string(error_message(Error), !IO)	
	).
	