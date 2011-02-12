:- module bf.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, char, solutions, require, int, bool, getopt.

:- type side ---> to_left; to_right.

:- type bf_cmd ---> plus; minus; step; back; print; read; cycle(list(bf_cmd));
				% optimized commands
				plus(int); zero;
				
				% [<++++++>-] => move(left, 1, 5)
				% [>>>>+<<<<-] => move(right, 4, 1)
				move(side :: side, steps :: int, multiplier :: int);
				step(int); back(int). 

:- type bf_ast == list(bf_cmd).

:- type bf_state ---> bf_state(
	left :: list(int),
	cell :: int,
	right :: list(int)
).

description = "Brainfuck interpreter written on Mercury programming language \
(http://www.mercury.csse.unimelb.edu.au/) by Vladimir Gubarkov (xonixx@gmail.com)".

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
execute_cmd(read, bf_state(L,_,R), bf_state(L, C, R)) --> 
	read_char(Res),
	{	Res = ok(Char),
		C = char.to_int(Char)
	;	
		Res = eof,
		C = 0	% see http://en.wikipedia.org/wiki/Brainfuck#End-of-file_behavior
		%error("eof")
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
	
optimize_cycle(CycleAst) = Res :-
	(	(CycleAst = [bf.plus]; CycleAst = [bf.minus]) ->
		Res = zero
	;
		one_solution(pred(P_::out) is nondet :- move_pattern(P_, CycleAst, []:bf_ast), MoveCmd) ->
		Res = MoveCmd
	;
		Res = cycle(optimize_ast(CycleAst))
	).		
		
optimize_ast(InAst) = OutAst :-
	(	InAst = [cycle(CycleAst)|T] ->
		OutAst = [optimize_cycle(CycleAst)|optimize_ast(T)]
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
		InAst = [H|T] ->
		OutAst = [H|optimize_ast(T)]
	;
		OutAst = InAst
	).


take(E, N) --> take(E, 0, N), {N > 0}.

take(E, N0, N1) --> 
	(	[E] -> 
		take(E, N0+1, N1)
	;
		{N0 = N1}
	).

% [-<++++++>]
% [<++++++>-]
% [->>>>+<<<<]
% [>>>>+<<<<-]
one_minus --> [bf.minus].
move_pattern(move(to_left, Steps, Multiplier)) --> one_minus, take(back, Steps), take(bf.plus, Multiplier), take(step, Steps).
move_pattern(move(to_left, Steps, Multiplier)) --> take(back, Steps), take(bf.plus, Multiplier), take(step, Steps), one_minus.
move_pattern(move(to_right, Steps, Multiplier)) --> one_minus, take(step, Steps), take(bf.plus, Multiplier), take(back, Steps).
move_pattern(move(to_right, Steps, Multiplier)) --> take(step, Steps), take(bf.plus, Multiplier), take(back, Steps), one_minus.

execute_chars(Chars, Options, !IO) :- 
	Ast = chars_to_ast(Chars),
	AstOpt = optimize_ast(Ast),
	lookup_bool_option(Options, print_ast, PrintAst),
	(	PrintAst = bool.yes,
		write_string("AST:\n", !IO),
		print(Ast, !IO),
		write_string("\n\nOptimized AST:\n", !IO),
		print(AstOpt, !IO)
	;
		PrintAst = bool.no,
		execute_ast(AstOpt, bf_state([], 0, []), _, !IO)
	).

get_chars_from_current_stream(Chars) -->
	read_file(Result),
	{	Result = ok(Chars)
	;	
		Result = error(_,Error),
		error(error_message(Error))
	}.

launch(Filename, Options, !IO) :-
	see(Filename, Result, !IO),
	(	Result = ok,
		get_chars_from_current_stream(Chars, !IO),
		seen(!IO),
		execute_chars(Chars, Options, !IO)	
	;	
		Result = error(Error),
		write_string(Filename ++ " : ", !IO), 
		write_string(error_message(Error), !IO)	
	).
	
usage -->
	write_strings([description,
	"\nUsage: bf [options] <file.bf>",
	"\nOptions:",
	"\n\t-A, --ast", 
		"\n\t\tPrint AST & optimized AST of bf program.",
	"\n\t--help",
		"\n\t\tPring this help."
	]).
	
:- type bf_option ---> print_ast; help.

:- mode opt_short(in, out) is semidet.
:- mode opt_long(in, out) is semidet.
:- mode opt_defaults(out, out) is nondet.

opt_short('A', print_ast).
opt_long("ast", print_ast).
opt_long("help", help).
opt_defaults(print_ast, bool(bool.no):option_data).
opt_defaults(help, bool(bool.no):option_data).

main(!IO) :-
	command_line_arguments(Args0, !IO),
	
	process_options(
		option_ops(opt_short,opt_long,opt_defaults),
		Args0, Args,
		MaybeOptions),
		
	(	MaybeOptions = error(String),
		write_string(String, !IO)
	;	
		MaybeOptions = ok(Options),
		(	Args = [Filename|_], 
			launch(Filename, Options, !IO)
		;	
			Args = [],
			usage(!IO)
		)
	).