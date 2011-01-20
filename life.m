:- module life.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, require.

:- type row == list(int).
:- type grid == list(row).
:- type sign ---> sum; mul; or_.

:- type lr ---> left; right; no.
:- type ud ---> up; down; no.

eq([R | RR], N) = [eq_row(R, N) | eq(RR, N)].
eq([], _) = [].

eq_row([H|T], N) = [(H=N->1;0) | eq_row(T,N)].
eq_row([],_) = [].

sum(M1, M2) = R :- R1 = agg(M1, M2, sum) -> R = R1 ; error("can't sum").
or(M1, M2) = R :- R1 = agg(M1, M2, or_) -> R = R1 ; error("can't or").
mul(M1, M2) = R :- R1 = agg(M1, M2, mul) -> R = R1 ; error("can't mul").

sum_lst(L) = foldl(sum, det_tail(L), det_head(L)).

:-func agg(grid, grid, sign) = grid is semidet.
agg([R1 | RR1], [R2 | RR2], Sign) = [agg_rows(R1, R2, Sign) | agg(RR1, RR2, Sign)].
agg([], [], _) = [].

:-func agg_rows(row, row, sign) = row is semidet.
agg_rows([E1 | EE1], [E2 | EE2], Sign) = [agg_elts(E1, E2, Sign) | agg_rows(EE1, EE2, Sign)].
agg_rows([], [], _) = [].

agg_elts(E1, E2, sum) = E1 + E2. 
agg_elts(E1, E2, mul) = E1 * E2. 
agg_elts(E1, E2, or_) = E1 \/ E2. 

hor([H | T], LR) = [hor_row(H, LR) | hor(T, LR)].
hor([], _) = [].

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

hor_row(L, left) = [0 | without_last(L)].
hor_row(L, right) = det_tail(L) ++ [0].
hor_row(L, no) = L.

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