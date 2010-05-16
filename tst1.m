:- module tst1.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.
:- import_module int.
:- import_module parsing_utils.

%:- pred p(src, int, ps, ps).
/*:- mode p(in, out, in, out) is semidet.
p(Src, {Int, Pos}) -->
	int_literal(Src, Int),
	current_offset(Src, Pos).
*/
make_list(N) = (if N = 0 then [] else [1 | make_list(N-1)]).

main -->
	{
	 %promise_equivalent_solutions [Res] parse("123123123asdasda", p, Res)
	 %N = 50000000,
	 N = 70000000,
	 Res = length(make_list(N)):int
	},
	io.print(Res).