:- module bf.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, char, solutions, require, int.

:- type side ---> to_left; to_right.

:- type bf_cmd ---> plus; minus; step; back; print; read; cycle(list(bf_cmd));
				% optimized commands
				plus(int); zero;
				
				% [<++++++>-] => move(left, 1, 5)
				% [>>>>+<<<<-] => move(right, 4, 1)
				move(side :: side, steps :: int, multiplier :: int);
				step(int); back(int). 

:- type bf_state ---> bf_state(
	left :: list(int),
	cell :: int,
	right :: list(int)
).

one_solution(Pred, Solution) :-
	solutions(Pred, [Solution|_]).

chars_to_ast(Chars) = Ast :-
	CharsClean = clean_chars(Chars),
	(	one_solution(pred(Ast_::out) is nondet :- ast(Ast_, CharsClean, []:list(char)), Ast0) ->
		Ast = Ast0
	;
		error("Program invalid (parse error)!")
	).
	
bf('+'). bf('-').
bf('>'). bf('<').
bf('['). bf(']').
bf('.'). bf(',').

clean_chars([]) = [].
clean_chars([H|T]) = R :-
	(	bf(H)	->
		R = [H|clean_chars(T)]
	;	
		R = clean_chars(T)
	).

:- mode ast(out, in, out) is multi.
ast([plus|Cmds]) --> ['+'], ast(Cmds).
ast([minus|Cmds]) --> ['-'], ast(Cmds).
ast([step|Cmds]) --> ['>'], ast(Cmds).
ast([back|Cmds]) --> ['<'], ast(Cmds).
ast([print|Cmds]) --> ['.'], ast(Cmds).
ast([read|Cmds]) --> [','], ast(Cmds).
ast([cycle(Cycle)|Cmds]) --> ['['], ast(Cycle), [']'], ast(Cmds).
ast([]) --> [].

execute_ast([], !State) --> [].
execute_ast([Cmd|Cmds], !State) --> execute_cmd(Cmd, !State), execute_ast(Cmds, !State).

execute_cmd(plus, bf_state(L,C,R), bf_state(L, C+1, R)) --> [].
execute_cmd(minus, bf_state(L,C,R), bf_state(L, C-1, R)) --> [].
execute_cmd(plus(N), bf_state(L,C,R), bf_state(L, C+N, R)) --> [].
execute_cmd(zero, bf_state(L,_,R), bf_state(L, 0, R)) --> [].
execute_cmd(step, bf_state(L,C,R), bf_state([C|L], H, T)) --> {R = [], H=0, T=[]; R = [H|T]}.
execute_cmd(back, bf_state(L,C,R), bf_state(T, H, [C|R])) --> {L = [], H=0, T=[]; L = [H|T]}.
execute_cmd(print, S @ bf_state(_,C,_), S) --> print(char.det_from_int(C):char).
execute_cmd(read, bf_state(L,_,R), bf_state(L, char.to_int(Char), R)) --> 
	read_char(Res),
	{	Res = ok(Char)
	;	
		Res = eof,
		error("eof")
	;
		Res = error(Error),
		error(error_message(Error))	
	}.
execute_cmd(Cmd @ cycle(Cmds), !.S @ bf_state(_,C,_), !:S) --> 
	(	{C \= 0} -> 
		execute_ast(Cmds, !S), 
		execute_cmd(Cmd, !S)
	;
		[]
	).
execute_cmd(step(N), !S) --> 
	(	{N > 0} ->
		execute_cmd(step,!S),
		execute_cmd(step(N-1),!S)
	;
		[]
	).
execute_cmd(back(N), !S) --> 
	(	{N > 0} ->
		execute_cmd(back,!S),
		execute_cmd(back(N-1),!S)
	;
		[]
	).
execute_cmd(move(Side, Steps, Multiplier), !.S @ bf_state(_,C,_), !:S) -->
	{	Side = to_left,
		A = back(Steps),
		B = step(Steps)
	;
		Side = to_right,
		A = step(Steps),
		B = back(Steps)
	},
	execute_ast([A, plus(C * Multiplier), B, zero], !S).
	

optimize_ast(InAst) = OutAst :-
	(	(InAst = [cycle([plus])|T]; InAst = [cycle([minus])|T]) ->
		OutAst = [zero|optimize_ast(T)]
	;
		InAst = [plus,plus|T] ->
		OutAst = optimize_ast([plus(2)|T])
	;	
		InAst = [minus,minus|T] ->
		OutAst = optimize_ast([plus(-2)|T])
	;
		InAst = [plus(N),plus|T] ->
		OutAst = optimize_ast([plus(N+1)|T])
	;
		InAst = [plus(N),minus|T] ->
		OutAst = optimize_ast([plus(N-1)|T])
	;	
		InAst = [cycle(Ast1)|T] ->
		OutAst = [cycle(optimize_ast(Ast1))|optimize_ast(T)]
	;
		InAst = [H|T] ->
		OutAst = [H|optimize_ast(T)]
	;
		OutAst = InAst
	).
		

execute_chars(Chars) --> 
	{	Ast = chars_to_ast(Chars),
		AstOpt = optimize_ast(Ast)
	},
	%print(Ast),nl,nl,
	%print(AstOpt),nl,nl,
	execute_ast(AstOpt, bf_state([], 0, []), _).

get_chars_from_current_stream(Chars) -->
	read_file(Result),
	{	Result = ok(Chars)
	;	Result = error(_,Error),
		error(error_message(Error))
	}.

launch(Filename, !IO) :-
	see(Filename, Result, !IO),
	(	Result = ok,
		get_chars_from_current_stream(Chars, !IO),
		seen(!IO),
		execute_chars(Chars, !IO)	
	;	
		Result = error(Error),
		write_string(Filename ++ " : ", !IO), 
		write_string(error_message(Error), !IO)	
	).
	
usage -->
	write_string("Usage: \n\n\tbf program_name.bf").

main(!IO) :-
	command_line_arguments(Args, !IO),
	(	Args = [Filename|_], 
		launch(Filename, !IO)
	;	Args = [],
		usage(!IO)
	).