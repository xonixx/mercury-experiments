%---------------------------------------------------------------------------%
% Copyright (C) 2000 Peter Ross
% This file may only be copied under the terms of the GNU General Public
% License - see the file COPYING
%-----------------------------------------------------------------------------%
%
% Main author: petdr
% A program that just listens to a socket echoing whatever it reads to stdout.
%
%-----------------------------------------------------------------------------%

:- module listen.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module stream, string, tcp.
:- import_module bool, char, int, list, require, string.
:- import_module thread, thread.channel, thread.spawn.

main -->
	{ Port = 8080 },
	{ tcp__bind("193.121.72.12", Port, Listen) },
	service_connections(Listen).

%-----------------------------------------------------------------------------%

:- pred service_connections(bound_tcp::in, io__state::di, io__state::uo) is cc_multi.

service_connections(Listen) -->
	{ tcp__accept(Listen, TCP) },
	service_connection(TCP),
	service_connections(Listen).

	% XXX mode-checking doesn't like tcp::di
:- pred service_connection(tcp::in, io__state::di, io__state::uo) is cc_multi.

service_connection(TCP0) -->
	{ copy(TCP0, TCP1) },
	cat(TCP1, TCP),
	{ tcp__shutdown(TCP) }.

%-----------------------------------------------------------------------------%

:- pred cat(S, S, T, T) <= (stream__input(S), stream__output(T)).
:- mode cat(di, uo, di, uo) is det.

cat(S0, S) -->
	{ stream__read_char(Result, S0, S1) },
	(
		{ Result = ok(Char) },
		stream__write_char(Char),
		cat(S1, S)
	;
		{ Result = eof },
		{ S = S1 }
	;
		{ Result = error(String) },
		{ error(string__format("cat: %s.", [s(String)])) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*
:- pred finally_io(pred(T, io__state, io__state),
		pred(io__state, io__state), io__state, io__state) is det.

finally_io(Pred, Final
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
