:- module solver1.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, bool, string, map, solutions, int.

:- type problem 
	--->	problem(name, domains, rules).
	
:- type name == string.	
:- type domains == list(domain).
:- type rules == list(t_rule).

:- type domain == list(string).
:- type t_rule
	--->	eq(var, var)
	;	neq(var, var)
	
	;	lt(var, var)
	;	gt(var, var)
	
	;	near(var, var)
	;	not_near(var, var).
	
:- type var
	--->	i(int)
	;	s(string).
	
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

:- func extract_vars(list(var)) = list(string).
extract_vars([]) = [].
extract_vars([i(_)|T]) = extract_vars(T).
extract_vars([s(S)|T]) = [S|extract_vars(T)].

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

%:- mode solve_verified(in, out) is det.
solve_verified(Problem, Solution) :-
	solutions(solution_nd(Problem), SolutionMaps),
	prepare_solution(SolutionMaps, Solution).
	
prepare_solution(SolutionMaps, Solution) :-
	t("Found: ", SolutionMaps),
	Solution = solution_ok([[]]).
	
:- mode solution_nd(in, out) is nondet.	
solution_nd(problem(Name, Domains, Rules), GeneratedSolutionMap) :-
	map.init(SolutionMap),
	
	length(det_head(Domains), L),
	Numbers = 1 .. L,
	
	generate_solution_map_nd(SolutionMap, GeneratedSolutionMap, Domains, Numbers),
	%trace [io(!IO)] (print(GeneratedSolutionMap, !IO), nl(!IO)),
	check_solution_map(GeneratedSolutionMap, Rules).
	
take_element(E, L, L1) :- list.delete(L, E, L1).	
	
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
	t("ok\n"),
	check_solution_map(M, RR).
	
check_rule(M, eq(A, B)) :- get_values(M, A, B, AV, BV), AV = BV.	
check_rule(M, neq(A, B)) :- get_values(M, A, B, AV, BV), AV \= BV.	

check_rule(M, lt(A, B)) :- get_values(M, A, B, AV, BV), AV < BV.	
check_rule(M, gt(A, B)) :- get_values(M, A, B, AV, BV), AV > BV.

check_rule(M, near(A, B)) :- get_values(M, A, B, AV, BV), abs(AV - BV) = 1.	
check_rule(M, not_near(A, B)) :- get_values(M, A, B, AV, BV), abs(AV - BV) \=1 .

	
get_values(M, A, B, get_value(M, A), get_value(M, B)).

:- func get_value(map(string, int), var) = int.
get_value(_, i(N)) = N.
get_value(M, s(K)) = map.lookup(M, K).
	

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
	verify_rule_var_names(VarNames, AllVars, !VerRes).
	
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
		eq(s("Englishman"),s("red")),
		eq(s("Swedish"),s("dog")),
		eq(s("Danish"),s("tea")),
		lt(s("green"),s("white")),
		eq(s("green"),s("coffee")),
		eq(s("PallMall"),s("bird")),
		eq(s("milk"),i(3)),
		eq(s("yellow"),s("Dunhill")),
		eq(s("Norwegian"),i(1)),
		near(s("Marlboro"),s("cat")),
		near(s("horse"),s("Dunhill")),
		eq(s("Winfield"),s("beer")),
		near(s("Norwegian"),s("blue")),
		eq(s("German"),s("Rothmans")),
		near(s("Marlboro"),s("water"))
	]).
	
problem2 = problem("test",
	[
	["a", "b", "c"]
	],
	[
		eq(s("a"), i(2)),
		lt(s("c"), s("b"))
	]
).	

main(!IO) :-
	solve(problem1, Solution),
	%solve(problem2, Solution),
	(	Solution = solution_ok(SolutionOk),
		print(SolutionOk, !IO)
	;
		Solution = solution_error(Errors),
		write_errors(Errors, !IO)
	).
	
	
	



