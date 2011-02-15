:- module sudoku.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module maybe, int, list.

problem1 = [
  0, 0, 3, 0, 0, 8, 2, 0, 4,
  0, 2, 0, 0, 6, 4, 0, 1, 0,
  9, 0, 0, 0, 0, 0, 0, 0, 8,
  0, 8, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 6, 9, 8, 0,
  0, 0, 0, 0, 0, 0, 5, 0, 0,
  0, 0, 4, 9, 0, 7, 0, 3, 0,
  8, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 7, 0, 0, 5, 0, 4, 0, 0
].

problem_to_maybe([]) = [].
problem_to_maybe([H|T]) = [elt_to_maybe(H)|problem_to_maybe(T)].

elt_to_maybe(E) = (E = 0 -> no; yes(E)).

solve_sudoku(
	!A00,!A01,!A02,	!A03,!A04,!A05,	!A06,!A07,!A08,
	!A10,!A11,!A12,	!A13,!A14,!A15,	!A16,!A17,!A18,
	!A20,!A21,!A22,	!A23,!A24,!A25,	!A26,!A27,!A28,
	
	!A30,!A31,!A32,	!A33,!A34,!A35,	!A36,!A37,!A38,
	!A40,!A41,!A42,	!A43,!A44,!A45,	!A46,!A47,!A48,
	!A50,!A51,!A52,	!A53,!A54,!A55,	!A56,!A57,!A58,
	
	!A60,!A61,!A62,	!A63,!A64,!A65,	!A66,!A67,!A68,
	!A70,!A71,!A72,	!A73,!A74,!A75,	!A76,!A77,!A78,
	!A80,!A81,!A82,	!A83,!A84,!A85,	!A86,!A87,!A88
	) 
	
	:-
	
	solve9(!A00,!A01,!A02,!A03,!A04,!A05,!A06,!A07,!A08),
	solve9(!A10,!A11,!A12,!A13,!A14,!A15,!A16,!A17,!A18),
	solve9(!A20,!A21,!A22,!A23,!A24,!A25,!A26,!A27,!A28),
	
	%--
	solve9(!A00,!A01,!A02,!A10,!A11,!A12,!A20,!A21,!A22),
	solve9(!A03,!A04,!A05,!A13,!A14,!A15,!A23,!A24,!A25),
	solve9(!A06,!A07,!A08,!A26,!A27,!A28,!A26,!A27,!A28),	
	%--
	
	solve9(!A30,!A31,!A32,!A33,!A34,!A35,!A36,!A37,!A38),
	solve9(!A40,!A41,!A42,!A43,!A44,!A45,!A46,!A47,!A48),
	solve9(!A50,!A51,!A52,!A53,!A54,!A55,!A56,!A57,!A58),
	
	%
	solve9(!A30,!A31,!A32,!A40,!A41,!A42,!A50,!A51,!A52),
	solve9(!A33,!A34,!A35,!A43,!A44,!A45,!A53,!A54,!A55),
	solve9(!A36,!A37,!A38,!A46,!A47,!A48,!A56,!A57,!A58),
	%

	solve9(!A00,!A10,!A20,!A30,!A40,!A50,!A60,!A70,!A80),
	solve9(!A01,!A11,!A21,!A31,!A41,!A51,!A61,!A71,!A81),
	solve9(!A02,!A12,!A22,!A32,!A42,!A52,!A62,!A72,!A82),

	solve9(!A60,!A61,!A62,!A63,!A64,!A65,!A66,!A67,!A68),
	solve9(!A70,!A71,!A72,!A73,!A74,!A75,!A76,!A77,!A78),
	solve9(!A80,!A81,!A82,!A83,!A84,!A85,!A86,!A87,!A88),
	
	solve9(!A03,!A13,!A23,!A33,!A43,!A53,!A63,!A73,!A83),
	solve9(!A04,!A14,!A24,!A34,!A44,!A54,!A64,!A74,!A84),
	solve9(!A05,!A15,!A25,!A35,!A45,!A55,!A65,!A75,!A85),
	solve9(!A06,!A16,!A26,!A36,!A46,!A56,!A66,!A76,!A86),
	solve9(!A07,!A17,!A27,!A37,!A47,!A57,!A67,!A77,!A87),
	solve9(!A08,!A18,!A28,!A38,!A48,!A58,!A68,!A78,!A88),

	%--
	solve9(!A60,!A61,!A62,!A70,!A71,!A72,!A80,!A81,!A82),
	solve9(!A63,!A64,!A65,!A73,!A74,!A75,!A83,!A84,!A85),
	solve9(!A66,!A67,!A68,!A76,!A77,!A78,!A86,!A87,!A88).
	
solve_call(
	[
		In00,In01,In02,In03,In04,In05,In06,In07,In08,
		In10,In11,In12,In13,In14,In15,In16,In17,In18,
		In20,In21,In22,In23,In24,In25,In26,In27,In28,
		In30,In31,In32,In33,In34,In35,In36,In37,In38,
		In40,In41,In42,In43,In44,In45,In46,In47,In48,
		In50,In51,In52,In53,In54,In55,In56,In57,In58,
		In60,In61,In62,In63,In64,In65,In66,In67,In68,
		In70,In71,In72,In73,In74,In75,In76,In77,In78,
		In80,In81,In82,In83,In84,In85,In86,In87,In88
	],
	[
		Out00,Out01,Out02,Out03,Out04,Out05,Out06,Out07,Out08,
		Out10,Out11,Out12,Out13,Out14,Out15,Out16,Out17,Out18,
		Out20,Out21,Out22,Out23,Out24,Out25,Out26,Out27,Out28,
		Out30,Out31,Out32,Out33,Out34,Out35,Out36,Out37,Out38,
		Out40,Out41,Out42,Out43,Out44,Out45,Out46,Out47,Out48,
		Out50,Out51,Out52,Out53,Out54,Out55,Out56,Out57,Out58,
		Out60,Out61,Out62,Out63,Out64,Out65,Out66,Out67,Out68,
		Out70,Out71,Out72,Out73,Out74,Out75,Out76,Out77,Out78,
		Out80,Out81,Out82,Out83,Out84,Out85,Out86,Out87,Out88
	]
	)
	
	:-
	
	solve_sudoku(
		In00,Out00,In01,Out01,In02,Out02,In03,Out03,In04,Out04,In05,Out05,In06,Out06,In07,Out07,In08,Out08,
		In10,Out10,In11,Out11,In12,Out12,In13,Out13,In14,Out14,In15,Out15,In16,Out16,In17,Out17,In18,Out18,
		In20,Out20,In21,Out21,In22,Out22,In23,Out23,In24,Out24,In25,Out25,In26,Out26,In27,Out27,In28,Out28,
		In30,Out30,In31,Out31,In32,Out32,In33,Out33,In34,Out34,In35,Out35,In36,Out36,In37,Out37,In38,Out38,
		In40,Out40,In41,Out41,In42,Out42,In43,Out43,In44,Out44,In45,Out45,In46,Out46,In47,Out47,In48,Out48,
		In50,Out50,In51,Out51,In52,Out52,In53,Out53,In54,Out54,In55,Out55,In56,Out56,In57,Out57,In58,Out58,
		In60,Out60,In61,Out61,In62,Out62,In63,Out63,In64,Out64,In65,Out65,In66,Out66,In67,Out67,In68,Out68,
		In70,Out70,In71,Out71,In72,Out72,In73,Out73,In74,Out74,In75,Out75,In76,Out76,In77,Out77,In78,Out78,
		In80,Out80,In81,Out81,In82,Out82,In83,Out83,In84,Out84,In85,Out85,In86,Out86,In87,Out87,In88,Out88
	).
	
	

:- pragma inline(digits/0).
:- func digits = list(int).
digits = [1,2,3,4,5,6,7,8,9].	
	
solve9(!A0,!A1,!A2,!A3,!A4,!A5,!A6,!A7,!A8) :-
	some [!D] (
		!:D = digits,
		take_one(!A0, !D),
		take_one(!A1, !D),
		take_one(!A2, !D),
		take_one(!A3, !D),
		take_one(!A4, !D),
		take_one(!A5, !D),
		take_one(!A6, !D),
		take_one(!A7, !D),
		take_one(!A8, !.D, [])
	).

take_one(yes(N), yes(N), L, L1) :- delete(L, N, L1).
take_one(no, yes(N), L, L1) :- delete(L, N, L1).

main --> 
	{P = problem_to_maybe(problem1)},
	print(P), nl,nl,
	(	{solve_call(P,Solution)} ->
		print(Solution)
	;
		[]
	).