:- module gramm.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, string, int, parsing_utils.


:- type op ---> (+); (-); (*); (/); (**).

:- type lexem --->
	open; close;
	op(op);
	comma;
	num(int);
	error.
:- type lexems == list(lexem).

:- type op_term ---> op_term(op, expr).
:- type expr --->
	apply_op(op, expr, expr);
	unary_op(op, expr);
	num(int).
		
% helpful

true --> {true}.
fail --> {fail}.
empty([]) --> [].

take(What, Sin, Sout) :-
	string.append(What, Sout, Sin).

:- pred take_one_of(T, list(T), list(T), list(T)).
:- mode take_one_of(out, in, in, out) is semidet.
take_one_of(A, [H | T]) -->
	( [H] -> {A = H}
	; take_one_of(A, T)
	).

:- pred take_one_of_f(T1, list(T), func(T) = T1, list(T), list(T)).
:- mode take_one_of_f(out, in, in, in, out) is semidet.
take_one_of_f(A, [H | T], F) -->
	( [H] -> {A = F(H)}
	; take_one_of_f(A, T, F)
	).

substr(N, Sin, Sout) :-
	Sout:string = string.right(Sin, string.length(Sin) - N).

:- pred skip_while(string, string, string).
:- mode skip_while(in, in, out) is semidet.
skip_while(WhileS) -->
	( take(WhileS) -> true
	; substr(1),
	  skip_while(WhileS)
	).

% lexing

:- pred lex(lexems, string, string).
:- mode lex(out, in, out) is semidet.
lex(Lexems) -->
	( lexem_t(H) ->
	  lex(T), {Lexems=[H | T]}
	; empty(Lexems)
	).

lexem_t(L) --> trashes, lexem(L), trashes.

lexem(L) -->
	( take("(") -> {L = open}
	; take(")") -> {L = close}
	; take("+") -> {L = op(+)}
	; take("-") -> {L = op(-)}
	; take("^") -> {L = op(**)}
	; num(N) -> {L = num(N)}
	; fail
	).

trashes -->
	( trash ->
	  trashes
	; []
	).

:- mode trash(in, out) is semidet.
trash -->
	( comment_marker(End)->
	  skip_while(End)
	; white
	).

:- mode comment_marker(out, in, out) is semidet.
comment_marker(Out) -->
	( take("(*") -> {Out = "*)"}
	; take("/*") -> {Out = "*/"}
	; fail    
	).

% one or more whitespaces
white --> white_one, whites.

whites -->
	( white_one ->
	  whites
	; []
	).

white_one -->
	( take(" ") -> true
	; take("\t")
	).

:- mode p(in, out, in, out) is semidet.
p(Src, {Int, Pos}) -->
	int_literal(Src, Int),
	current_offset(Src, Pos).

num(N, !S) :-
	promise_equivalent_solutions [Res] parse(!.S, p, Res),
	Res = ok({N, Pos}),
	substr(Pos, !S).

% aggregate
add_all(T1, [op_term(Op, T2) | T], R) :-
	add_all(apply_op(Op, T1, T2), T, R).
add_all(R, [], R).

add_all_right(T1, [op_term(Op, T2) | T], apply_op(Op, T1, R1)) :-
	add_all_right(T2, T, R1).
add_all_right(R, [], R).
% end aggregate

% parsing

expr(E) --> term(T1), plus_minus_terms(Terms), {add_all(T1, Terms, E)}.

plus_minus_terms(L) -->
	( plus_minus_op(Op), term(T) ->
	  plus_minus_terms(L1), {L = [op_term(Op, T) | L1]}
	; empty(L)
	).

plus_minus_op(Op) --> take_one_of(op(Op), [op(+), op(-)]).

term(T) --> factor(F1), mul_div_factors(Factors), {add_all(F1, Factors, T)}.

mul_div_factors(L) -->
	( mul_div_op(Op), factor(T) ->
	  mul_div_factors(L1), {L = [op_term(Op, T) | L1]}
	; empty(L)
	).

mul_div_op(Op) --> take_one_of(op(Op), [op(*), op(/)]).

factor(F) --> pwr(P), pwrs(PP), {add_all_right(P, PP, F)}.

pwrs(L) -->
	( pwr_op(Op), pwr(T) ->
	  pwrs(L1), {L = [op_term(Op, T) | L1]}
	; empty(L)
	).

pwr_op(**) --> [op(**)].

pwr(P) --> a(P).

:- pred a(expr, lexems, lexems).
:- mode a(out, in, out) is semidet.
a(E) -->
	( [open] -> expr(E), [close]
	; [num(N)] -> {E = num(N)}
	; plus_minus_op(Op), factor(F) -> {E = unary_op(Op, F)}
	; fail
	).


main -->
	{	 
	(lex(Lexems0,"(  123456( \t \t\t\t ) )---++++++", "")
	-> Lexems = Lexems0
	; Lexems = [error] )/*,
	 (trashes("     ",R0)
	 -> R = R0
	 ; R = ""
	 )*/
	},
	%io.print(R),
	io.print(Lexems)
	.