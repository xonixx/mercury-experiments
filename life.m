:- module life.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, require.

:- type row == list(int).
:- type grid == list(row).
:- type sign ---> (+); (*); (\/).

:- type lr ---> left; right; no.
:- type ud ---> up; down; no.

eq([R | RR], N) = [eq_row(R, N) | eq(RR, N)].
eq([], _) = [].

eq_row([H|T], N) = [(H=N->1;0)|eq_row(T,N)].
eq_row([],_) = [].

sum(M1, M2) = R :- R1 = agg(M1, M2, (+)) -> R = R1 ; error("can't sum").
or(M1, M2) = R :- R1 = agg(M1, M2, (\/)) -> R = R1 ; error("can't or").
mul(M1, M2) = R :- R1 = agg(M1, M2, (*)) -> R = R1 ; error("can't mul").

sum_lst(L) = R :- (
	L = [M1,M2|MM] -> R = sum_lst([sum(M1,M2)|MM])
	;
	L=[M] -> R = M
	;
	error("sum_lst")
	).

:-func agg(grid, grid, sign) = grid is semidet.
agg([R1 | RR1], [R2 | RR2], Sign) = [agg_rows(R1, R2, Sign) | agg(RR1, RR2, Sign)].
agg([], [], _) = [].

:-func agg_rows(row, row, sign) = row is semidet.
agg_rows([E1 | EE1], [E2 | EE2], Sign) = [agg_elts(E1, E2, Sign) | agg_rows(EE1, EE2, Sign)].
agg_rows([], [], _) = [].

agg_elts(E1, E2, (+):sign) = E1 + E2. 
agg_elts(E1, E2, (*)) = E1 * E2. 
agg_elts(E1, E2, (\/)) = E1 \/ E2. 

hor([H | T], LR) = [ hor_row(H, LR) | hor(T, LR) ].
hor([], _) = [].

head_det(L) = E :- (
	L = [], error("empty list")
	;
	L=[E1|_], E = E1
	).
	
gen(T, N) = R :- (
	N=0 -> R = []
	;
	R = [T|gen(T,N-1)]).

%:- func vert(grid, ud) = grid.
vert(M, up) = [zeros(M)|without_last(M)].
vert(M, down) = without_first(M) ++ [zeros(M)].
vert(M, no) = M.

zeros(M) = gen(0, length(head_det(M))).

without_first(L) = R :- (
	L = [], error("without_first fail")
	;
	L=[_ | T], R=T
	).

%:- func without_last(list(T)) = list(T).
without_last(L) = R :- ( 
	L=[], error("without_last fail")
	;
	L=[_], R=[]
	;
	L=[H,H1|T], R=[H|without_last([H1|T])]
	).

hor_row(L, left) = [0 | without_last(L)].
hor_row(L, right) = without_first(L) ++ [0].
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
	move(M, down, right)]).
	
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

print_m([H|T]) --> print_r(H), nl, print_m(T).
print_m([]) --> [].

print_r([H | T]) --> print(H=0->".";"#"), print_r(T).
print_r([]) --> [].

next(M) = or(eq(MN,3), eq(mul(M,MN),4)) :- MN = neighbours(M).

trace(M, N) --> (
	{N = 0} -> []
	;
	print_m(M),
	nl,
	trace(next(M), N-1)).

main -->
	trace(m1,25).
	%print_m(m1),nl,
	%print_m(neighbours(m1)),nl,
	%print_m(next(m1)).
	%print_m(move(m1,down, left)).