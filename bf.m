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

execute_cmd(plus, bf_state(L,C,R), bf_state(L, C+1, R)) --> [].
execute_cmd(minus, bf_state(L,C,R), bf_state(L, C-1, R)) --> [].
execute_cmd(step, bf_state(L,C,R), bf_state([C|L], H, T)) --> {R = [], H=0, T=[]; R = [H|T]}.
execute_cmd(back, bf_state(L,C,R), bf_state(T, H, [C|R])) --> {L = [], H=0, T=[]; L = [H|T]}.
execute_cmd(print, S @ bf_state(_,C,_), S) --> print(char.det_from_int(C):char).
execute_cmd(Cmd @ cycle(Cmds), !.S @ bf_state(_,C,_), !:S) --> 
	(	{C \= 0} -> 
		execute(Cmds, !S), 
		execute_cmd(Cmd, !S)
	;
		[]
	).

execute_str(Prog) --> {Ast = prog_to_ast(Prog)}, execute(Ast, bf_state([], 0, []), _).

main --> 
	execute_str(hello_world), nl, 
	execute_str(squares_1_to_1000), nl, 
	execute_str(fib_1_100).