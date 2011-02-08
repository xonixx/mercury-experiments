:- module bf.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, string, char, solutions, require, int.

:- type bf_cmd ---> plus; minus; step; back; print; cycle(list(bf_cmd)).

:- type bf_state ---> bf_state(
	left :: list(int),
	cell :: int,
	right :: list(int)
).

hello_world = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++\
.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.\
------.--------.>+.>.".
	
squares_1_to_1000 = "++++[>+++++<-]>[<+++++>-]+<+[\
>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+\
>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]\
<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-\
]".

fib_1_100 = "+++++++++++\
>+>>>>++++++++++++++++++++++++++++++++++++++++++++\
>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>\
+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-\
<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<\
-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]\
>[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++\
+++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++\
++++++++++++++++++++++++++++++++++++++++++++.[-]<<\
<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<\
[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]".
	
prog_to_ast(Prog) = Ast :-
	to_char_list(Prog, Chars), 
	solutions(pred(Ast_::out) is nondet :- ast(Ast_, Chars, []:list(char)), Asts),
	(	Asts = [], error("Program invalid (parse error)!")
	;	Asts = [Ast|_]
	).

:- mode ast(out, in, out) is multi.
ast([plus|Cmds]) --> ['+'], ast(Cmds).	
ast([minus|Cmds]) --> ['-'], ast(Cmds).	
ast([step|Cmds]) --> ['>'], ast(Cmds).	
ast([back|Cmds]) --> ['<'], ast(Cmds).	
ast([print|Cmds]) --> ['.'], ast(Cmds).	
ast([cycle(Cycle)|Cmds]) --> ['['], ast(Cycle), [']'], ast(Cmds).
ast([]) --> [].

execute([], !State) --> [].
execute([Cmd|Cmds], !State) --> execute_cmd(Cmd, !State), execute(Cmds, !State).

execute_cmd(plus, S @ bf_state(L,C,R), bf_state(L, C+1, R)) --> [], p(S).
execute_cmd(minus, S @ bf_state(L,C,R), bf_state(L, C-1, R)) --> [], p(S).
execute_cmd(step, S @ bf_state(L,C,R), bf_state([C|L], H, T)) --> [], p(S), {R = [], H=0, T=[]; R = [H|T]}.
execute_cmd(back, S @ bf_state(L,C,R), bf_state(T, H, [C|R])) --> [], p(S), {L = [], H=0, T=[]; L = [H|T]}.
execute_cmd(print, S @ bf_state(_,C,_), S) --> print(char.det_from_int(C):char).
execute_cmd(Cmd @ cycle(Cmds), !.S @ bf_state(_,C,_), !:S) --> p(!.S), 
	(	{C \= 0} -> 
		execute(Cmds, !S), 
		execute_cmd(Cmd, !S)
	;
		[]
	).

%p(T) --> print(T), nl.
p(_) --> [].


exec(Prog) --> {Ast = prog_to_ast(Prog)}, /* print(Ast), nl, nl, */ execute(Ast, bf_state([], 0, []), _).

main --> 
	exec(hello_world), nl, 
	exec(squares_1_to_1000), nl, 
	exec(fib_1_100).