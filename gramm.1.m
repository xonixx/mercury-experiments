:- module gramm.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, string, int.


:- type op ---> (+); (-); (*); (/); (^).
:- type lexem --->
	open; close;
	op(op);
	comma;
	num(int).

:- pred lex(list(lexem), string, string).
:- mode lex(out, in, out) is semidet.
lex(Lexems) -->
	(lexem_t(H) ->
	 lex(T), {Lexems=[H | T]}
	;
	 {Lexems = []}
	).

lexem_t(L) --> trashes, lexem(L), trashes.

lexem(L) -->
	(take("(") -> {L = open}
	;take(")") -> {L = close}
	;take("+") -> {L = op(+)}
	;take("-") -> {L = op(-)}
	;{fail}
	).

trashes -->
	(trash ->
	 trashes
	;
	 []
	).

:- mode trash(in, out) is semidet.
trash -->
	(comment_marker(End)->
	 skip_while(End)
	; white
	).

:- mode comment_marker(out, in, out) is semidet.
comment_marker(Out) -->
    (take("(*") -> {Out = "*)"}
    ;take("/*") -> {Out = "*/"}
    ;{fail}    
    ).

white -->
	(white0 ->
	 white
	;
	 white0
	).

white0 -->
	(take(" ")
	-> {true}
	 ; take("\t")).

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

main -->
	{	 
	(lex(Lexems0,"(()-+)", "")
	-> Lexems = Lexems0
	; Lexems = [] ),
	 (trashes("     ",R0)
	 -> R = R0
	 ; R = ""
	 )
	},
	io.print(R)
	,io.print(Lexems)
	.