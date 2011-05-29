:- module array_sort.
:- interface.
:- import_module array.

:- func sort(array(T)) = array(T).
:- mode sort(array_di) = array_uo is det.

:- implementation.
:- import_module int, string, random, list, std_util, require.

%------------------------------------------------------------------------------%

    % array__sort/1 has type specialised versions for arrays of
    % ints and strings on the expectation that these constitute
    % the common case and are hence worth providing a fast-path.
    %
    % Experiments indicate that type specialisation improves
    % array__sort/1 by a factor of 30-40%.
    %
:- pragma type_spec(array_sort__sort/1, T = int).
:- pragma type_spec(array_sort__sort/1, T = string).

array_sort__sort(A) = msort_subarray(A, array__min(A), array__max(A)).

% array_sort__sort(A) = hsort_subarray(A, array__min(A), array__max(A)).

% array_sort__sort(A) = qsort_subarray(P, array__min(A), array__max(A)) :-
    % random__init(2450987, RS),
    % random_permutation(A, P, RS, _).

%------------------------------------------------------------------------------%

	% array__random_permutation(A0, A, RS0, RS) permutes the elements in
	% A0 given random seed RS0 and returns the permuted array in A
	% and the next random seed in RS.
	%
:- pred random_permutation(array(T), array(T),
		random__supply, random__supply).
:- mode random_permutation(array_di, array_uo, mdi, muo) is det.

random_permutation(A0, A, RS0, RS) :-
	Lo = array__min(A0),
	Hi = array__max(A0),
	Sz = array__size(A0),
	permutation_2(Lo, Lo, Hi, Sz, A0, A, RS0, RS).



:- pred permutation_2(int, int, int, int, array(T), array(T),
		random__supply, random__supply).
:- mode permutation_2(in, in, in, in, array_di, array_uo, mdi, muo) is det.

permutation_2(I, Lo, Hi, Sz, A0, A, RS0, RS) :-
	( if I > Hi then
		A  = A0,
		RS = RS0
	  else
	  	random__random(R, RS0, RS1),
	  	J  = Lo + (R `rem` Sz),
		A1 = swap_elems(A0, I, J),
		permutation_2(I + 1, Lo, Hi, Sz, A1, A, RS1, RS)
	).

%------------------------------------------------------------------------------%

    % qsort_subarray(A, Lo, Hi) sorts into ascending order
    % the elements of array A with indices Lo, Lo + 1, ..., Hi.
    %
    % This procedure uses quicksort choosing the pivot as
    % median of three where candidate pivots have indices
    % Lo, (Lo + Hi) //2, and Hi.
    %
:- func qsort_subarray(array(T), int, int) = array(T).
:- mode qsort_subarray(array_di, in, in) = array_uo is det.

:- pragma type_spec(qsort_subarray/3, T = int).
:- pragma type_spec(qsort_subarray/3, T = string).

qsort_subarray(A0, Lo, Hi) = A :-
    ( if Lo >= Hi then
        A     = A0
      else
        Mid   = (Lo + Hi) // 2,
        Pivot = median_of_three(A0 ^ elem(Lo), A0 ^ elem(Mid), A0 ^ elem(Hi)),
        partition(Pivot, Lo, Hi, I, A0, A1),
        A2    = qsort_subarray(A1, Lo, I - 1),
        A     = qsort_subarray(A2, I,  Hi)
    ).

%------------------------------------------------------------------------------%

    % partition(Pivot, Lo, Hi, I, A0, A) reorders the subarray from
    % Lo to Hi in A0 into A such that all elements less than Pivot come
    % before all other elements, returning the index of the dividing
    % point in I.
    %
:- pred partition(T, int, int, int, array(T), array(T)).
:- mode partition(in, in, in, out, array_di, array_uo) is det.

:- pragma type_spec(partition/6, T = int).
:- pragma type_spec(partition/6, T = string).

partition(Pivot, Lo, Hi, I, A0, A) :-
    ( if Lo > Hi then
        A = A0,
        I = Hi + 1                      % Elts above Hi are > Pivot
      else
        compare(Result, A0 ^ elem(Lo), Pivot),
        (
            Result = (<),
            partition(Pivot, Lo + 1, Hi, I, A0, A)
        ;
            Result = (=),
            partition(Pivot, Lo + 1, Hi, I, A0, A)
        ;
            Result = (>),
            partition(Pivot, Lo, Hi - 1, I, swap_elems(A0, Lo, Hi), A)
        )
    ).

%------------------------------------------------------------------------------%

    % Computes the middle of three values, or the least value if
    % two are the same.
    %
:- func median_of_three(T, T, T) = T.

:- pragma type_spec(median_of_three/3, T = int).
:- pragma type_spec(median_of_three/3, T = string).

median_of_three(X, Y, Z) = M :-
    compare(RXY, X, Y),
    (
        RXY = (<),
        compare(RYZ, Y, Z),
        (   RYZ = (<),      M = Y
        ;   RYZ = (=),      M = X
        ;   RYZ = (>),      M = generic_max(X, Z)
        )
    ;
        RXY = (=),          M = generic_min(Y, Z)
    ;
        RXY = (>),
        compare(RXZ, X, Z),
        (   RXZ = (<),      M = X
        ;   RXZ = (=),      M = Y
        ;   RXZ = (>),      M = generic_max(Y, Z)
        )
    ).

%------------------------------------------------------------------------------%

:- func generic_min(T, T) = T.

:- pragma type_spec(generic_min/2, T = int).
:- pragma type_spec(generic_min/2, T = string).

generic_min(X, Y) = ( if compare((<), X, Y) then X else Y ).

%------------------------------------------------------------------------------%

:- func generic_max(T, T) = T.

:- pragma type_spec(generic_max/2, T = int).
:- pragma type_spec(generic_max/2, T = string).

generic_max(X, Y) = ( if compare((>), X, Y) then X else Y ).

%------------------------------------------------------------------------------%

:- func swap_elems(array(T), int, int) = array(T).
:- mode swap_elems(array_di, in, in) = array_uo is det.

swap_elems(A0, I, J) = A :-
    XI = A0 ^ elem(I),
    XJ = A0 ^ elem(J),
    A  = ((A0   ^ elem(I) := XJ)
                ^ elem(J) := XI).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

    % Heap sort a subarray.
    %
:- func hsort_subarray(array(T), int, int) = array(T).
:- mode hsort_subarray(array_di, in, in) = array_uo is det.

:- pragma type_spec(hsort_subarray/3, T = int).
:- pragma type_spec(hsort_subarray/3, T = string).

hsort_subarray(A, Lo, Hi) = hsort_subarray_2(make_heap(A, Lo, Lo, Hi), Lo, Hi).



:- func hsort_subarray_2(array(T), int, int) = array(T).
:- mode hsort_subarray_2(array_di, in, in) = array_uo is det.

:- pragma type_spec(hsort_subarray_2/3, T = int).
:- pragma type_spec(hsort_subarray_2/3, T = string).

hsort_subarray_2(A0, Lo, Hi) = A :-
    ( if Lo =< Hi then
        XLo = A0 ^ elem(Lo),
        XHi = A0 ^ elem(Hi),
        A1  = A0 ^ elem(Hi) := XLo,
        A2  = down_heap(A1, XHi, Lo, Hi - 1),
        A   = hsort_subarray_2(A2, Lo, Hi - 1)
      else
        A = A0
    ).

%------------------------------------------------------------------------------%

:- func make_heap(array(T), int, int, int) = array(T).
:- mode make_heap(array_di, in, in, in) = array_uo is det.

:- pragma type_spec(make_heap/4, T = int).
:- pragma type_spec(make_heap/4, T = string).

make_heap(A, I, Lo, Hi) =
    ( if I =< Hi then make_heap(up_heap(A, A ^ elem(I), Lo, I), I + 1, Lo, Hi)
                 else A
    ).

%------------------------------------------------------------------------------%

:- func up_heap(array(T), T, int, int) = array(T).
:- mode up_heap(array_di, in, in, in) = array_uo is det.

:- pragma type_spec(up_heap/4, T = int).
:- pragma type_spec(up_heap/4, T = string).

up_heap(A, X, Lo, I) =
    ( if Lo < I, compare((<), A ^ elem(J), X) then
        up_heap(A ^ elem(I) := A ^ elem(J), X, Lo, J)
      else
        A ^ elem(I) := X
    )
 :-
    J = (I - 1) // 2.

%------------------------------------------------------------------------------%

:- func down_heap(array(T), T, int, int) = array(T).
:- mode down_heap(array_di, in, in, in) = array_uo is det.

:- pragma type_spec(down_heap/4, T = int).
:- pragma type_spec(down_heap/4, T = string).

down_heap(A0, X, I, Hi) = A :-
    J1 = I + I + 1,
    J2 = J1 + 1,
    ( if J1 =< Hi, XJ1 = A0 ^ elem(J1), compare((<),  X, XJ1)
      then K0 = J1,    X0 = XJ1
      else K0 = I,     X0 = X
    ),
    ( if J2 =< Hi, XJ2 = A0 ^ elem(J2), compare((<), X0, XJ2)
      then K1 = J2,    X1 = XJ2
      else K1 = K0,    X1 = X0
    ),
    ( if I = K1 then
        A = A0 ^ elem(I) := X
      else
        A = down_heap(A0 ^ elem(I) := X1, X, K1, Hi)
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

    % Merge sort an array.
    %
:- func msort_subarray(array(T), int, int) = array(T).
:- mode msort_subarray(array_di, in, in) = array_uo is det.

:- pragma type_spec(msort_subarray/3, T = int).
:- pragma type_spec(msort_subarray/3, T = string).

msort_subarray(A, Lo, Hi) =
    msort_subarray_2(A, array__copy(A), 1, Lo, Hi).



:- func msort_subarray_2(array(T), array(T), int, int, int) = array(T).
:- mode msort_subarray_2(array_ui, array_di, in, in, in) = array_uo is det.

:- pragma type_spec(msort_subarray_2/5, T = int).
:- pragma type_spec(msort_subarray_2/5, T = string).

msort_subarray_2(A, B, N, Lo, Hi) =
    ( if N >= Hi
      then A
      else msort_subarray_2(msort_subarray_3(A, B, Lo, N, Hi), A, N + N, Lo, Hi)
    ).



:- func msort_subarray_3(array(T), array(T), int, int, int) = array(T).
:- mode msort_subarray_3(array_ui, array_di, in, in, in) = array_uo is det.

:- pragma type_spec(msort_subarray_3/5, T = int).
:- pragma type_spec(msort_subarray_3/5, T = string).

msort_subarray_3(A, B0, I, N, Hi) = B :-
    ( if I + N > Hi then
        B  = copy_subarray(A, B0, I, Hi, I)
      else if I + N + N > Hi then
        B  = merge_subarrays(A, B0, I, I + N - 1, I + N, Hi, I)
      else
        B1 = merge_subarrays(A, B0, I, I + N - 1, I + N, I + N + N - 1, I),
        B  = msort_subarray_3(A, B1, I + N + N, N, Hi)
    ).

%------------------------------------------------------------------------------%

    % merges the two sorted consecutive subarrays Lo1 .. Hi1 and
    % Lo2 .. Hi2 from A into the subarray starting at I in B.
    % 
:- func merge_subarrays(array(T), array(T), int, int, int, int, int) = array(T).
:- mode merge_subarrays(array_ui, array_di, in, in, in, in, in) = array_uo
            is det.

:- pragma type_spec(merge_subarrays/7, T = int).
:- pragma type_spec(merge_subarrays/7, T = string).

merge_subarrays(A, B0, Lo1, Hi1, Lo2, Hi2, I) = B :-
    (      if Lo1 > Hi1 then B = copy_subarray(A, B0, Lo2, Hi2, I)
      else if Lo2 > Hi2 then B = copy_subarray(A, B0, Lo1, Hi1, I)
      else
        X1 = A ^ elem(Lo1),
        X2 = A ^ elem(Lo2),
        compare(R, X1, X2),
        (
            R = (<),
            B = merge_subarrays(A, B0^elem(I) := X1, Lo1+1, Hi1, Lo2, Hi2, I+1)
        ;
            R = (=),
            B = merge_subarrays(A, B0^elem(I) := X1, Lo1+1, Hi1, Lo2, Hi2, I+1)
        ;
            R = (>),
            B = merge_subarrays(A, B0^elem(I) := X2, Lo1, Hi1, Lo2+1, Hi2, I+1)
        )
    ).

%------------------------------------------------------------------------------%

:- func copy_subarray(array(T), array(T), int, int, int) = array(T).
:- mode copy_subarray(array_ui, array_di, in, in, in) = array_uo is det.

:- pragma type_spec(copy_subarray/5, T = int).
:- pragma type_spec(copy_subarray/5, T = string).

copy_subarray(A, B, Lo, Hi, I) =
    ( if Lo > Hi
      then B
      else copy_subarray(A, B ^ elem(I) := A ^ elem(Lo), Lo + 1, Hi, I + 1)
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pred subarray_is_sorted(array(T), int, int).
:- mode subarray_is_sorted(array_ui, in, in) is semidet.

subarray_is_sorted(A, Lo, Hi) :-
    ( if Lo + 1 >= Hi then
    	true
      else
        compare((<), A ^ elem(Lo), A ^ elem(Lo + 1)),
        subarray_is_sorted(A, Lo + 1, Hi)
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
