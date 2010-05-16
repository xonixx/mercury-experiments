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
	num(int);
	error.
		
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

:- mode calc(in) = out is det. 
calc(unary_op(Op, Expr)) = R :-
	( E = calc(Expr),
	  ( Op = (-) -> R = -E
	  ; R = E
	  )
	).
calc(apply_op(Op, E1, E2)) = R :-
	R1 = calc(E1),
	R2 = calc(E2),
	( Op = (+), R = R1 + R2
	; Op = (-), R = R1 - R2
	; Op = (*), R = R1 * R2
	; Op = (/), R = R1 / R2
	; Op = (**), R = pow(R1, R2)
	).
calc(num(N)) = N.
calc(error) = -37707.

build_large_l(N) = L :-
	( N = 0
	-> L = []
	; L = ["+1" | build_large_l(N - 1) ]
	).

build_large_s(N) = append_list(build_large_l(N)):string.
	
main -->
	{
	 %S = "(  123456( \t \t\t\t ) )---++++++",
	 %S = "+1+22+333",
	 S = build_large_s(30000)
	},
	 io.print("generated"),
	({lex(Lexems0,S, "")}
	-> {Lexems = Lexems0:lexems}, io.write(" lexed"),
	 ( {expr(E1, Lexems, [])}
	 -> {E = E1}
	 ; {E = error:expr}
	 )
	; {Lexems = [error], E = error} ),

	/*,
	 (trashes("     ",R0)
	 -> R = R0
	 ; R = ""
	 )*/

	
	%io.print(R),
	%io.print(Lexems),
	io.nl
	%,io.print(E)
	,io.nl
	,io.print(calc(E))
	.