:- module reverse.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- func reverse2(list(int)) = list(int).
%:- mode reverse2(in) = out.
:- mode reverse2(in(non_empty_list)) = out(non_empty_list).
reverse2(A) = reverse2_aux(A, []).

:- func reverse2_aux(list(int), list(int)) = list(int).
:- mode reverse2_aux(in, in) = out.
%:- mode reverse2_aux(in(non_empty_list), in) = out(non_empty_list).
%:- mode reverse2_aux(in, in(non_empty_list)) = out(non_empty_list).
reverse2_aux([], Acc) = Acc.
reverse2_aux([A | As], Acc) = reverse2_aux(As, [A | Acc]).

main(!IO) :-
	Y = 1, Ys = [2,3,4,5],
	[X | Xs] = reverse2([Y | Ys]),
	print({X, Xs}, !IO), nl(!IO).