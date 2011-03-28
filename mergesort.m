:- module mergesort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool, list, int.

:- mode m_split(in, out, out) is det.
m_split(L, L1, L2) :- split(yes, L, [], L1, [], L2).

:- mode split(in, in, in, out, in, out) is det.
split(yes, [H|T], L1, L11, L2, L22) :- split(no, T, [H|L1], L11, L2, L22).
split(no, [H|T], L1, L11, L2, L22) :- split(yes, T, L1, L11, [H|L2], L22).
split(_, [], L1, L1, L2, L2).

:- func m_sort(list(int)) = list(int).
m_sort(L @ [_,_|_]) = R :- m_split(L, L1, L2), m_merge(m_sort(L1), m_sort(L2), R).
m_sort(L @ [_]) = L.
m_sort([]) = [].

m_merge([H1|T1] @ L1, [H2|T2] @ L2, [H|T]) :- 
	(	H1 < H2 -> 
		H = H1,
		m_merge(T1, L2, T)
	;
		H = H2,
		m_merge(L1, T2, T)
	).
m_merge([], L @ [_|_], L).
m_merge(L @ [_|_], [], L).
m_merge([], [], []).


main --> {L=[5,2,3,4,1,7,0], m_split(L, L1, L2)}, 
	print(L1), nl, 
	print(L2), nl, 
	print(m_sort(L))%,
	%{m_merge([1,3,5], [2,4,7,9], R)},
	%print(R),
	%print("hello")
	.