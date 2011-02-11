:- module typeclasses1.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.

:- type decomposed(T) ---> 
	one(T);
       	lst(list(T)).

:- typeclass decomposeable(T) where [
	func decompose(T) = decomposed(T)
].

:- instance decomposeable(list(E)) where [
	func(decompose)
].



