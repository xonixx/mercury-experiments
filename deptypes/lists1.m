:- module lists1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

%:- inst even_list ---> [] ; [ground|even_list]. % XXX
%:- inst even_list ---> [] ; [ground,ground|even_list]. % YYY

%:- inst even_list ---> []; [ground | bound([ground | even_list])].
:- inst even_list == bound([]; [ground | bound([ground | even_list])]).

%:- type even_list(T) ---> []; [T,T|even_list(T)].

%:- pred t(even_list(int)).
:- pred t(list(int)).
:- mode t(in(even_list)) is semidet.
t([1,2,3,4]).

main(!IO) :-
	%L=[1,2,3,4,5,6],
	L=[1,2,3,4,5,6,7,8,9],
	(	t(L) ->
		print("1",!IO)
	;
		print("2",!IO)
	).