%-----------------------------------------------------------------------------%*
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% File: stream_utils.m.
% Main author: petdr
% Stability: exceptionally low.
%
% This file provides some predicates for working with defining streams.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module stream_util.

:- interface.

:- import_module char, io, list.
:- import_module stream.

%-----------------------------------------------------------------------------%

% Predicates which require an input stream.

	% Reads one line of input from the current input stream.
:- pred stream_util__read_line(S::in, stream__result(list(char), Error)::out,
		io__state::di, io__state::uo) is det
		<= stream__reader(S, char, io, Error).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

read_line(Stream, Result) -->
	stream__get(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { Char = '\n' } ->
			{ Result = ok([Char]) }
		;
			read_line(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)	
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
