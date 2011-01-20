:- module life.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, require.

:- type row == list(int).
:- type grid == list(row).
:- type sign ---> sum_; mul_; or_.

:- type lr ---> left; right; no.
:- type ud ---> up; down; no.

eq(M, N) = map(map(func(E)=(E=N->1;0)),M).

sum(M1, M2) = agg(sum_, M1, M2).
or(M1, M2) = agg(or_, M1, M2).
mul(M1, M2) = agg(mul_, M1, M2).

sum_lst(L) = foldl(sum, det_tail(L), det_head(L)).

agg(Sign, M1, M2) = map_corresponding(map_corresponding(agg_elts(Sign)),M1,M2).

agg_elts(sum_, E1, E2) = E1 + E2. 
agg_elts(mul_, E1, E2) = E1 * E2. 
agg_elts(or_, E1, E2) = E1 \/ E2. 

hor(M, LR) = map(hor_row(LR), M).

gen(T, N) = R :- (
	N=0 -> R = []
	;
	R = [T|gen(T,N-1)]
	).

vert(M, up) = [zeros(M) | without_last(M)].
vert(M, down) = det_tail(M) ++ [zeros(M)].
vert(M, no) = M.

zeros(M) = gen(0, length(det_head(M))).

without_last(L) = R :- det_split_last(L,R,_).

hor_row(left, L) = [0 | without_last(L)].
hor_row(right, L) = det_tail(L) ++ [0].
hor_row(no, L) = L.

:- func move(grid, ud, lr) = grid.
move(M, UD, LR) = hor(vert(M, UD), LR).

neighbours(M) = sum_lst([
	move(M, up, left),
	move(M, up, no),
	move(M, up, right),
	
	move(M, no, left),
	move(M, no, no),
	move(M, no, right),

	move(M, down, left),
	move(M, down, no),
	move(M, down, right)
	]).


%% this is GoL algorithm
%%
next(M) = or(eq(MN,3), eq(mul(M,MN),4)) :- MN = neighbours(M).


%% grid pretty-print
%%
print_m([H|T]) --> print_r(H), nl, print_m(T).
print_m([]) --> [].

print_r([H | T]) --> print_el(H), print_r(T).
print_r([]) --> [].

print_el(H) --> print(H=0->".";"#").

trace(M, N) --> (
	{N = 0} -> []
	;
	print_m(M),
	nl,
	trace(next(M), N-1)
	).

m1 = [
	[0,1,0,0,0,0,0,0,0,0],
	[0,0,1,0,0,0,0,0,0,0],
	[1,1,1,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0],
	[0,0,0,0,0,0,0,0,0,0]
	].

main -->
	trace(m1,25).