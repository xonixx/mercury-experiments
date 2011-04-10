:- module solver3.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, bool, string, map, solutions, int, require.

:- type problem 
	--->	problem(name, domains, rules).
	
:- type name == string.	
:- type domains == list(domain).
:- type rules == list(t_rule).

:- type domain == list(string).

:- type t_rule
	--->	eq(var,var)
	;	neq(var, var)
	
	;	lt(var, var)
	;	gt(var, var)
	
	;	near(var, var)
	;	not_near(var, var).	
	
:- type var
	--->	num(int)
	;	str(string).
	
:- type solution_result 
	--->	solution_ok(solution_ok)
	;	solution_error(list(string)).
	
:- type solution_ok == list(list(string)).	
	
:- typeclass extractable(T) where [
	func extract(T) = list(string)
].

:- instance extractable(t_rule) where [
	func(extract/1) is extract_rule_vars
].

extract_rule_vars(eq(A, B)) = extract_vars([A, B]).
extract_rule_vars(neq(A, B)) = extract_vars([A, B]).
extract_rule_vars(lt(A, B)) = extract_vars([A, B]).
extract_rule_vars(gt(A, B)) = extract_vars([A, B]).
extract_rule_vars(near(A, B)) = extract_vars([A, B]).
extract_rule_vars(not_near(A, B)) = extract_vars([A, B]).

:- typeclass showable(T) where [
	func show(T) = string
].

:- instance showable(t_rule) where [
	func(show/1) is show_rule
]. 

:- instance showable(var) where [
	func(show/1) is show_var
]. 

show_rule(eq(A,B)) = show_rule("=",A,B).
show_rule(neq(A,B)) = show_rule("<>",A,B).
show_rule(lt(A,B)) = show_rule("<",A,B).
show_rule(gt(A,B)) = show_rule(">",A,B).
show_rule(near(A,B)) = show_rule("|",A,B).
show_rule(not_near(A,B)) = show_rule("#",A,B).

show_rule(S, A, B) = show(A) ++ S ++ show(B). 

show_var(str(S)) = S.
show_var(num(N)) = string.from_int(N).

:- func extract_vars(list(var)) = list(string).
extract_vars([]) = [].
extract_vars([num(_)|T]) = extract_vars(T).
extract_vars([str(S)|T]) = [S|extract_vars(T)].

:- mode append_lists(in, out) is det.
append_lists([], []).	
append_lists([L], L).	
append_lists([L1,L2|LL], L) :- append_lists([L1++L2|LL], L).	

:- mode solve(in, out) is det.
solve(Problem, Solution) :-
	verify(Problem, VerRes),
	(	VerRes = [],
		solve_verified(Problem, Solution)
	;
		VerRes = [_|_],
		Solution = solution_error(VerRes)
	).

solve_verified(Problem, Solution) :-
	solutions(solution_nd(Problem), SolutionMaps),
	prepare_solution(SolutionMaps, Solution).
	
prepare_solution(SolutionMaps, Solution) :-
	t("Found: ", SolutionMaps),
	Solution = solution_ok([[]]).
	
:- mode solution_nd(in, out) is nondet.	
solution_nd(problem(_Name, Domains, Rules), SolutionMap) :-
	length(det_head(Domains), L),
	Numbers = 1 .. L,
	
	some [!HelperMap] (
	map.init(!:HelperMap),
	prepare_helper_map(Rules, !HelperMap),
	optimize_helper_map(!HelperMap),
	
	map.init(SolutionMap0),
	generate_solution_map_nd(SolutionMap0, SolutionMap, Domains, Numbers)
	),
	%trace [io(!IO)] (print(SolutionMap, !IO), nl(!IO)),
	
	check_solution_map(SolutionMap, Rules).
	
take_element(E, L, L1) :- list.delete(L, E, L1).	
	
:- type var_map == map(var, list(var)).	
	
:- pred prepare_helper_map(rules, var_map, var_map).	
prepare_helper_map([], !M).	
prepare_helper_map([R|RR], !M) :-	
	prepare_helper_map_rule(R, !M),
	prepare_helper_map(RR, !M).		

helper_map_add(!M, S1, S2) :-
	(	map.search(!.M, S1, L) ->
		L1 = [S2 | L]
	;
		L1 = [S2]
	),
	map.set(!.M, S1, L1, !:M).

prepare_helper_map_rule(R, !M) :-
	(	R = eq(V1, V2) ->
		(	(V1 = num(_), V2 = num(_)) -> 
			error("prepare_helper_map_rule both are nums")
		;
			helper_map_add(!M, V1, V2),
			helper_map_add(!M, V2, V1)
		)
	;
		true
	).

optimize_helper_map(M_in, M_out) :-
	map.init(M0),
	map.foldl((pred(K::in, L::in, M_cur::in, M::out) is det:-
		(	K = num(_) ->
			list.foldl((pred(SK::in, M_cur1::in, M1::out) is det:- 
				map.set(M_cur1, SK, [K], M1)
				), L, M_cur, M)
		;
			map.set(M_cur, K, L, M)
		)), M_in, M0, M_out).

generate_solution_map_nd(!M, [], _).
generate_solution_map_nd(!M, [D|DD], Numbers) :-
	process_domain(!M, D, Numbers, []),
	generate_solution_map_nd(!M, DD, Numbers).
	
process_domain(!M, [], !Numbers).	
process_domain(M0, M, [E|EE], !Numbers) :-
	take_element(N, !Numbers),
	map.det_insert(M0, E, N, M1),
	process_domain(M1, M, EE, !Numbers).

t(S, T) :- 
	trace [io(!IO)] (
		write_string(S, !IO), 
		write_string(" ", !IO),
		print(T, !IO), 
		nl(!IO)
	).
t(S) :- t(S, "").	

check_solution_map(_, []).
check_solution_map(M, [R|RR]) :-
	%t("Checking", {R, M}),
	check_rule(M, R),
	%t("ok\n"),
	check_solution_map(M, RR).
	
check_rule(M, eq(A, B)) :- get_values(M, A, B, AV, BV), AV = BV.	
check_rule(M, neq(A, B)) :- get_values(M, A, B, AV, BV), AV \= BV.	

check_rule(M, lt(A, B)) :- get_values(M, A, B, AV, BV), AV < BV.	
check_rule(M, gt(A, B)) :- get_values(M, A, B, AV, BV), AV > BV.

check_rule(M, near(A, B)) :- get_values(M, A, B, AV, BV), abs(AV - BV) = 1.	
check_rule(M, not_near(A, B)) :- get_values(M, A, B, AV, BV), abs(AV - BV) \=1 .

	
get_values(M, A, B, get_value(M, A), get_value(M, B)).

:- func get_value(map(string, int), var) = int.
get_value(_, num(N)) = N.
get_value(M, str(K)) = map.lookup(M, K).
	

verify(problem(_, Domains, Rules), VerRes) :-
	some [!VerRes] (
		!:VerRes = [],
		verify_domains_equal_len(Domains, !VerRes),
		verify_domains(Domains, !VerRes),
		verify_rules(Rules, all_vars(Domains), !VerRes),
		VerRes = !.VerRes
	).
	

all_vars(Domains) = AllVars :- append_lists(Domains, AllVars).
	
verify_domains_equal_len([], !VerRes) :- add_error("There is no domains defined", !VerRes).
verify_domains_equal_len([D|DD], !VerRes) :- 
	list.length(D, L),
	verify_domains_len(L, DD, !VerRes).
		
verify_domains_len(_, [], !VerRes).
verify_domains_len(L, [D|DD], !VerRes) :-
	(	list.length(D, L) ->
		ok(!VerRes)
	;
		add_error("Invalid domain length", !VerRes)
	),
	verify_domains_len(L, DD, !VerRes).
	

verify_domains([], !VerRes).
verify_domains([D | DD], !VerRes) :-
	verify_domain(D, !VerRes),
	verify_domain(D, DD, !VerRes),
	verify_domains(DD, !VerRes).
	
% inner	
verify_domain([], !VerRes).
verify_domain([E | EE], !VerRes) :-
	verify_not_in_domain(E, EE, yes, !VerRes),
	verify_domain(EE, !VerRes).

:- type verify_result == list(string).

:- pred verify_not_in_domain(string, list(string), bool, verify_result,verify_result).
verify_not_in_domain(E, EE, MyDomain, !VerRes) :-	
	(	member(E, EE) ->
		add_error(E ++ ": duplication in " ++
			(MyDomain = yes -> "my"; "outer") ++ 
			" domain", !VerRes)
	;
		ok(!VerRes)
	).	

verify_domain([], _, !VerRes).
verify_domain([_|_], [], !VerRes).
verify_domain([E|EE], [D|DD] @ DDD, !VerRes) :- 
	verify_not_in_domain(E, D, no, !VerRes),
	verify_domain([E], DD, !VerRes),
	verify_domain(EE, DDD, !VerRes).

add_error(S, EE, [S|EE]).	
ok(EE,EE).	
	
verify_rules([], _, !VerRes).
verify_rules([R | RR], AllVars, !VerRes) :-
	verify_rule(R, AllVars, !VerRes),
	verify_rules(RR, AllVars, !VerRes).

verify_rule(Rule, AllVars, !VerRes) :-
	extract(Rule) = VarNames,
	%trace [io(!IO)] print({VarNames, AllVars}, !IO),
	(	VarNames = [],
		add_error("Rule is invalid: " ++ show(Rule), !VerRes)
	;
		VarNames = [_|_],
		verify_rule_var_names(VarNames, AllVars, !VerRes)
	).
	
verify_rule_var_names([], _, !VerRes).	
verify_rule_var_names([V|VV], AllVars, !VerRes) :-
	(	member(V, AllVars) ->
		ok(!VerRes)
	;
		add_error(V ++ ": unknown var in rule", !VerRes)
	),
	verify_rule_var_names(VV, AllVars, !VerRes).
	

write_errors(Errors, !IO) :- write_list(Errors, "\n", write_string, !IO).  


problem1 = problem(
	"Einstain problem",
	[
		["Englishman", "Swedish", "Danish", "Norwegian", "German"],
		["red", "green", "white", "yellow", "blue"],
		["dog", "cat", "horse", "bird", "fish"],
		["tea", "coffee", "milk", "beer", "water"],
		["PallMall", "Dunhill", "Marlboro", "Winfield", "Rothmans"]
	],
	[
		eq(str("Englishman"),str("red")),
		eq(str("Swedish"),str("dog")),
		eq(str("Danish"),str("tea")),
		lt(str("green"),str("white")),
		eq(str("green"),str("coffee")),
		eq(str("PallMall"),str("bird")),
		eq(str("milk"),num(3)),
		eq(str("yellow"),str("Dunhill")),
		eq(str("Norwegian"),num(1)),
		near(str("Marlboro"),str("cat")),
		near(str("horse"),str("Dunhill")),
		eq(str("Winfield"),str("beer")),
		near(str("Norwegian"),str("blue")),
		eq(str("German"),str("Rothmans")),
		near(str("Marlboro"),str("water"))
	]).
	
problem2 = problem("test",
	[
	["a", "b", "c"]
	],
	[
		eq(str("a"), num(2)),
		lt(str("c"), str("b"))
	]
).	

problem3 = problem("test3",
	[
		["triangle", "diamond", "circle", "square"],
		["green", "yellow", "blue", "red"]
	],
	[
		near(str("red"), str("green")),
		near(str("red"), str("blue")),
		gt(str("diamond"), str("yellow")),
		near(str("diamond"), str("yellow")),
		gt(str("circle"), str("triangle")),
		gt(str("circle"), str("diamond")),
		gt(str("triangle"), num(1)),
		lt(str("triangle"), num(4)),
		not_near(str("blue"), str("yellow"))
	]
).	

main(!IO) :-
	%solve(problem1, Solution),
	solve(problem3, Solution),
	(	Solution = solution_ok(SolutionOk),
		print(SolutionOk, !IO)
	;
		Solution = solution_error(Errors),
		write_errors(Errors, !IO)
	).
	
	
	



