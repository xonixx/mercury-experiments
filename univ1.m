:- module univ1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module univ, list, solutions.

:-pred aa(univ::out) is multi.
aa(L) :-
	L = univ(123)
	;
	L = univ("Hello").
	
	
main --> 
	{solutions(aa, L)}, 
	print_univs(L).


:- pred print_univs(list(univ)::in, io::di, io::uo).
print_univs([U | T]) --> print(univ_value(U)), print_univs(T).
print_univs([])-->[].
	