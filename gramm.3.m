:- module gramm.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, string, int.


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
		

:- pred lex(lexems, string, string).
:- mode lex(out, in, out) is semidet.
lex(Lexems) -->
	( lexem_t(H) ->
	  lex(T), {Lexems=[H | T]}
	; {Lexems = []}
	).

lexem_t(L) --> trashes, lexem(L), trashes.

lexem(L) -->
	( take("(") -> {L = open}
	; take(")") -> {L = close}
	; take("+") -> {L = op(+)}
	; take("-") -> {L = op(-)}
	; take("^") -> {L = op(**)}
	; {fail}
	).

trashes -->
	(trash ->
	 trashes
	;
	 []
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
    ; {fail}    
    ).

% one or more whitespaces
white --> white_one, whites.

whites -->
	( white_one ->
	  whites
	; []
	).

white_one -->
	( take(" ") -> {true}
	; take("\t")
	).

take(What, Sin, Sout) :-
	string.append(What, Sout, Sin).

:- pred skip_while(string, string, string).
:- mode skip_while(in, in, out) is semidet.
skip_while(WhileS,!S) :-
	(
	 take(WhileS,!S)
	-> true
	;
	 skip_while(WhileS, string.right(!.S, string.length(!.S)-1), !:S)
	).

% aggregate
add_all(T1, [op_term(Op, T2) | T], R) :-
	add_all(apply_op(Op, T1, T2), T, R).
add_all(R, [], R).

add_all_right(T1, [op_term(Op, T2) | T], apply_op(Op, T1, R1)) :-
	add_all_right(T2, T, R1).
add_all_right(R, [], R).
% end aggregate

% parsing

:- pred take_one_of(T, list(T), list(T), list(T)).
take_one_of(A, [H | T]) -->
	( [H] -> {A = H}
	; take_one_of(A, T)
	).

empty(L) --> [], {L=[]}.	
	
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
	(
	 [open]
	-> expr(E), [close]
	;
	 [num(N)]
	-> {E = num(N)}
	;
	 plus_minus_op(Op), factor(F)
	-> {E = unary_op(Op, F)}
	; {fail}
	).


main -->
	{	 
	(lex(Lexems0,"(    ( \t \t\t\t ))---++++++", "")
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