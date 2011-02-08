:- module bf.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, char, solutions, require, int.

:- type bf_cmd ---> plus; minus; step; back; print; read; cycle(list(bf_cmd));
				plus(int); zero. % optimized commands

:- type bf_state ---> bf_state(
	left :: list(int),
	cell :: int,
	right :: list(int)
).

prog_to_ast(Prog) = Ast :-
	to_char_list(Prog, Chars),
	Ast = chars_to_ast(Chars).
	
chars_to_ast(Chars) = Ast :-
	CharsClean = clean_chars(Chars),
	solutions(pred(Ast_::out) is nondet :- ast(Ast_, CharsClean, []:list(char)), Asts),
	(	Asts = [], error("Program invalid (parse error)!")
	;	Asts = [Ast|_]
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
execute_cmd(read, S /* @ bf_state(_,C,_) */, S) --> { error("Sorry, can't read yet") }.
execute_cmd(Cmd @ cycle(Cmds), !.S @ bf_state(_,C,_), !:S) --> 
	(	{C \= 0} -> 
		execute_ast(Cmds, !S), 
		execute_cmd(Cmd, !S)
	;
		[]
	).

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