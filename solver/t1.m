:- module t1.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.


:- typeclass qqqable(T) where [
	func qqq(T) = T
].

:- type aaa ---> aaa(int).

:- type bbb(T) ---> bbb(T).
:- type bbb == bbb(int).

:- instance qqqable(aaa) where [ func(qqq/1) is qqq_aaa ]. % XXX
:- instance qqqable(bbb) where [ func(qqq/1) is qqq_aaa ]. % YYY

qqq_aaa(A) = A.

main(!IO) :- print(qqq(aaa(7)),!IO).