:- module tc1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- typeclass a(T) where [
	func aaa(T) = T
].

:- instance a(int) where [
	aaa(E) = E 
].

:- type q ---> some [T] q(T) => a(T).

z(q(T)) = aaa(T).

main --> print(z('new q'(123))).
