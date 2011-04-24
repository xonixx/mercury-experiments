:- module aag11.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, solutions, list.

%~ p(1):- !, a....
%~ p(2):- !, b....
%~ p(3):- !, c....

:- mode a(in, out) is nondet.
a(N, R) :- (N mod 2 = 0), member(R, 1 .. N).
%a(N, R) :- (N mod 2 = 1), member(R, 1 .. N).

:- mode b(in, out) is nondet.
b(N, R) :- a(N+1, R).
%b(N, R) :- a(N+2, R).

:- mode c(in, out) is nondet.
c(N, R) :- a(N * 3, R).

:- mode p(in, out) is nondet.
p(N, R) :- 
	(	N = 1, a(N, R)
	; 
		N = 2, b(N, R)
	; 
		N = 3, c(N, R)
	).
	

test(N) --> {/*unsorted_*/solutions(p(N), L)}, 
		write_int(N),
		write_string("->"),
		print(L),
		nl.

main --> test(1), test(2), test(3).%.	