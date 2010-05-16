:- module fast_string_test.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list, string, int, parsing_utils.

to_char_list_(Str) = ( first_char(Str, C, Rest) ->
		      [C | to_char_list_(Rest)]
		    ; []
		    ).

make_list(Len, C) = (if Len = 0 then [] else [C | make_list(Len-1, C)]).

make_long_string(Len, Char) = S :-
	CharLst = make_list(Len, Char),
	string.to_char_list(S, CharLst).


:- type pos == int.
:- type fast_string --->
	fast_string(string, pos).

:- func to_fast_string(string) = fs.
:- func from_fast_string(fs) = string.
:- func fs_length(fs) = int.

:- pred front_char(string, fs, fs). % string of len 1
:- mode front_char(out, in, out) is semidet.

:- pred front(string, int, fs, fs).
:- mode front(out, in, in, out) is semidet.

:- type fs == fast_string.
	
to_fast_string(Str) = fast_string(Str, 0).
from_fast_string(FS @ fast_string(Str, Pos)) = string.unsafe_substring(Str, Pos, fs_length(FS)).

fs_length(fast_string(Str,Pos)) = string.length(Str) - Pos.


front_char(C, !FS) :- front(C, 1, !FS).

front(FrontStr, Count, F0 @ fast_string(Str, Pos), fast_string(Str, NewPos)) :-
	fs_length(F0) >= Count,
	NewPos = Pos + Count,
	FrontStr = string.unsafe_substring(Str, Pos, Count).
	
%%% fast_string tst

to_char_list_fs(FS) = ( front_char(C, FS, FS1) ->
		      [C | to_char_list_fs(FS1)]
		    ; []
		    ).

to_char_list_s_fs(Str) = Lst :-
	Fs = to_fast_string(Str),
	Lst = to_char_list_fs(Fs).

main(!IO) :-
	
	N = 50000000,
	S = make_long_string(N, 'A'),
	print("made\n", !IO),
	%CL = to_char_list_(S):list(character),
	%CL = to_char_list_s_fs(S):list(string),
	CL = string.to_char_list(S),
	print("to_c_l_\n", !IO),
	print(length(CL):int, !IO)
	 
	%print(from_fast_string(to_fast_string("12345")), !IO)
	%print(to_char_list_s_fs("ABCDEFGHIJK"), !IO)
	.
	