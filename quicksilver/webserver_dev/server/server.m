%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2001 Peter Ross
% This file may only be copied under the terms of the GNU General Public
% License - see the file COPYING
%-----------------------------------------------------------------------------%


:- module server.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
%:- import_module http, options.

:- import_module tcp.

:- import_module bool, char, exception, getopt.
:- import_module int, list, require, std_util, string.
:- import_module maybe.
:- import_module stream.
:- import_module thread, thread.channel.
:- import_module unit.

:- import_module soap.
:- import_module web_methods.
:- import_module soap_test_methods.
:- import_module stream_util.

main -->
	io__command_line_arguments(Args0),
	{ OptionOpts = option_ops(short_option, long_option, option_defaults)},
	{ getopt__process_options(OptionOpts, Args0, _Args, OptionsResult) },
	(
		{ OptionsResult = ok(OptTable) },
		{ getopt__lookup_bool_option(OptTable, help, Help) },
		(
			{ Help = yes },
			options_help
		;
			{ Help = no },
			{ getopt__lookup_string_option(OptTable, host, Host) },
			{ getopt__lookup_int_option(OptTable, port, Port) },
			{ getopt__lookup_int_option(OptTable, profiling,
				ProfilingNrConns) },
			tcp__bind(Host, Port, Result),
			tcp__ignore_sigpipe,
			(
				{ Result = ok(Listen) },
				( { ProfilingNrConns > 0 } ->
					service_connections_profiling(Listen,
						ProfilingNrConns)
				;
					service_connections(Listen)
				)
			;
				{ Result = error(String) },
				io__write_string(String),
				io__nl
			),
			tcp__unignore_sigpipe
		)
	;
		{ OptionsResult = error(OptionErrorString) },
		io__write_string(OptionErrorString),
		io__nl,
		options_help
	).

%-----------------------------------------------------------------------------%

:- pred service_connections(bound_tcp::in,
		io__state::di, io__state::uo) is cc_multi.

service_connections(Listen) -->
	tcp__accept(Listen, Result),
	(
		{ Result = ok(TCP) },
		spawn(service_connection(TCP))
	;
		{ Result = error(String) },
		io__write_string(String),
		io__nl
	),
	service_connections(Listen).

	% For profiling we exit cleanly after a preset number of connections
	% so that the profiling data can be writting out.  Also, we don't use
	% thread.spawn since profiling and threads don't work together.
	%
:- pred service_connections_profiling(bound_tcp::in, int::in,
		io__state::di, io__state::uo) is cc_multi.

service_connections_profiling(Listen, NrConns) -->
	tcp__accept(Listen, Result),
	(
		{ Result = ok(TCP) },
		service_connection(TCP)
	;
		{ Result = error(String) },
		io__write_string(String),
		io__nl
	),
	( { NrConns > 1 } ->
		service_connections_profiling(Listen, NrConns - 1)
	;
		[]
	).

:- pred service_connection(tcp::in, io__state::di, io__state::uo) is cc_multi.

service_connection(TCP, !IO) :-
	try_io((pred(unit::out, IO0::di, IO::uo) is cc_multi :-
		service_connection_2(TCP, IO0, IO)
	), Result, !IO),
	tcp__shutdown(TCP, !IO),
	(
		Result = succeeded(_)
	;
		Result = exception(_)
		% The most common reason would be that the client closed the
		% connection.  Other reasons may be worth logging.
	).

:- pred service_connection_2(tcp::in, io__state::di, io__state::uo) is cc_multi.

service_connection_2(TCP) -->
	get_request(TCP, RequestLines),
	{ parse_request(RequestLines, RequestOrResponse) },
	(
		{ RequestOrResponse = left(Request) },
		(
		    { Request^cmd = post },
		    { check_headers(Request^headers, MaybeError) },
		    (
			{ MaybeError = yes(ErrorResponse) }
		    ->
			send_response(TCP, ErrorResponse),
			{ error("Invalid HTTP headers in request.\n") }
		    ;
		        get_soapmessage(TCP, Request, Request1),
		    	(
			    { Request1 = right(ErrorResponse1) },
			    send_response(TCP, ErrorResponse1),
			    { error("Invalid SOAP body.\n") }
			;
			    { Request1 = left(Request2) }
		    	)
		    )
		;	
		    { Request^cmd = get },
	 	    { Request2 = Request }
		),
		generate_response(Request2, Response)
	;
		{ RequestOrResponse = right(Response) }
	),
	send_response(TCP, Response).

%-----------------------------------------------------------------------------%

:- pred get_request(S, list(string), io__state, io__state)
	<= ( stream__reader(S, char, io, Error),
	     stream__reader(S, line, io, Error)).
:- mode get_request(in, out, di, uo) is det.

get_request(S, RequestLines) -->
	stream__get(S, LineResult),
	(
		{ LineResult = ok(line(Line)) },
		( { Line = "\r\n" } ->
			{ RequestLines = [] }
		;
			get_request(S, RequestLines0),
			{ RequestLines = [Line | RequestLines0] }
		)
	;
		{ LineResult = eof },
		{ RequestLines = [] }
	;
		{ LineResult = error(Error) },
		{ Msg = stream__error_message(Error) },
		{ error(string__format("get_request: %s.", [s(Msg)])) }
	).

%-----------------------------------------------------------------------------%

:- type either(T, U)
	--->	left(T)
	;	right(U).

:- type request
	--->	request(
			cmd	::	cmd,
			uri	::	string,
			version	::	string,
			headers	::	list(header),
			body	::	maybe(body)
		).

:- type cmd
	--->	get
	; 	post.

:- type header
	--->	header(
			name	::	string,
			value	::	string,
			extra	::	maybe(string)
		).

:- type body == string.

:- pred parse_request(list(string)::in,
		either(request, response)::out) is det.

parse_request([], RequestOrResponse) :- 
	Response = response(501, [], no_body, yes),
	RequestOrResponse = right(Response).
parse_request([RequestLine | RequestLines], Either) :-
	three_words(RequestLine, Cmd, URI, HTTP_Version),
	(
		Cmd \= "",
		URI \= "",
		HTTP_Version \= ""
	->	
		parse_headers(RequestLines, Headers),
		(
			Cmd = "POST"
		->
			Command = post
		;
			Command = get
		),
		Request = request(Command, URI, HTTP_Version, Headers, no),
		Either = left(Request)
	;
		Response = response(501, [], no_body, yes),
		Either = right(Response)
	).

:- pred three_words(string::in, string::out, string::out, string::out) is det.

three_words(Line, One, Two, Three) :-
	Words = string.words(Line),
	(
		Words = [],
		One = "",
		Two = "",
		Three = ""
	;
		Words = [One],
		Two = "",
		Three = ""
	;
		Words = [One, Two],
		Three = ""
	;
		Words = [One, Two, Three | _]
	).

:- pred parse_headers(list(string)::in, list(header)::out) is det.

parse_headers(RequestLines, Headers) :-
	list.map(parse_header, RequestLines, Headers).

:- pred parse_header(string::in, header::out) is det.

parse_header(RequestLine, Header) :-
        three_words(RequestLine, Name, Value, Extra),
	(
		Extra \= ""
	->
		Extra0 = yes(Extra)
	;
		Extra0 = no
	),
        Header = header(Name, Value, Extra0).


%---------------------------------------------------------------------------%

	% Section 6 Using SOAP in HTTP
	% " HTTP applications MUST use the media type "text/xml" 
	%   according to RFC 2376 when including SOAP entity bodies 
	%   in HTTP messages.
	% 
	% Section 6.1.1. The SOAPAction HTTP Header Field
	% " An HTTP client MUST use this header field when issuing a 
	%   SOAP HTTP Request. "
	

:- pred check_headers(list(header)::in, maybe(response)::out) is det. 

check_headers(Headers, Response) :-
	check_each_header(Headers, soapaction_header,
		"SOAPAction header not found.\n", MayBeResponse0),
	( 
		MayBeResponse0 = no
	->
		check_each_header(Headers, content_type_header,
			"Incorrect Content-Type value.\n", MayBeResponse1),
		( 	
			MayBeResponse1 = no
		->
			Response = no 
		;
			Response = MayBeResponse1
		)
	;
		Response = MayBeResponse0
	).

:- pred check_each_header(list(header), pred(header), string, maybe(response)). 
:- mode check_each_header(in, pred(in) is semidet, in, out) is det.

check_each_header(Headers, Pred, ErrorMessage, Response) :- 
	list__filter(Pred, Headers, Result), 
	( 
	  	Result \= [], 
		list__length(Result, 1)	  % ensure header only occur once
	->
		Response = no
	;
		Response = yes(response(
			400, 
			[], 
			string_body(ErrorMessage),
			yes )
		)	
		
	).

:- pred soapaction_header(header::in) is semidet.
soapaction_header(header("SOAPAction:", _, _)).

:- pred content_type_header(header::in) is semidet.
content_type_header(header("Content-Type:", "text/xml", _)).
content_type_header(header("Content-type:", "text/xml", _)).
content_type_header(header("Content-Type:", "text/xml;", _)).
content_type_header(header("Content-type:", "text/xml;", _)).

%---------------------------------------------------------------------------%

:- pred get_soapmessage(S, request, either(request, response), 
	io__state, io__state) <= stream__reader(S, char, io, Error).
:- mode get_soapmessage(in, in, out, di, uo) is det.

get_soapmessage(S, Request, RequestOrResponse) -->
	(
		{ get_content_length(Request^headers, Length) },
		{ Length \= 0 }
	->
		get_body(S, Length, SoapMessage),
		{ RequestOrResponse = left(request(
				Request^cmd,
				Request^uri, 
				Request^version, 
				Request^headers,
				yes(string__from_char_list(SoapMessage))))
		}
	;
		{ RequestOrResponse = right(response(
			411, 
			[], 
			string_body("Content-Length not supplied.\n"),
			yes)) 
		}
	).

:- pred get_content_length(list(header)::in, int::out) is semidet.

get_content_length([], _) :- fail.
get_content_length([header(Name, Value, _) | Headers], Length) :-
        (
		is_content_length(Name),
	        string__to_int(Value, Length0)
	->
	        Length = Length0
	;
	        get_content_length(Headers, Length)
	).

:- pred is_content_length(string::in) is semidet.
is_content_length("Content-Length:").
is_content_length("Content-length:").

:- pred is_content_type(string::in) is semidet.
is_content_type("Content-Type:").
is_content_type("Content-type:").

:- pred get_body(S, int, list(char), io__state, io__state)
	<= stream__reader(S, char, io, Error).
:- mode get_body(in, in, out, di, uo) is det.

	% If there are still characters when length = 0,
	% the rest will not be obtained and consequently parsing 
	% an incomplete message will throw an exception.
get_body(S, Length, RequestLines) -->
        stream__get(S, CharResult),
	{ Length0 = Length - 1 },
	(
		{ Length0 = 0 },
		{ CharResult = ok(Char) }
	->
                { RequestLines = [Char] }
        ;
                { CharResult = error(Error) }
        ->
		{ Msg = stream__error_message(Error) },
 		{ error(string__format("get_request: %s.", [s(Msg)])) }
        ;
                { CharResult = ok(Char) }
        ->
                get_body(S, Length0, RequestLines0),
		{ RequestLines = [Char | RequestLines0] }
	;
		{ RequestLines = [] }
	).

%-----------------------------------------------------------------------------%

:- type transfer_coding
	--->	unimplemented.

:- type response
	--->	response(
			respCode	::	int,
			respHeaders	::	list(header),
			% respCoding	::	list(transfer_coding),
			respBody	::	response_body,
			respSendBody	::	bool
		).

:- type response_body
	--->	no_body
	% ;	file_body(string)	% path to file
	;	string_body(string).	% actual html

:- pred generate_response(request::in, response::out,
		io__state::di, io__state::uo) is det.

generate_response(Request, Response) -->
	(
		{ Request^cmd = get },
		io__see(uri_to_filename(Request^uri), SeeResult),
		(
			{ SeeResult = ok },
			io__read_file_as_string(Result),
			io__seen,
			(
				{ Result = ok(String) },
				{ Response = response(200, [], 
						string_body(String), yes) }
			;
				{ Result = error(_, _) },
				{ Response = response(404, [], no_body, yes) }
			)
		;
			{ SeeResult = error(_) },
			{ Response = response(404, [], no_body, yes) }
		)
	;
		{ Request^cmd = post },
		( 
			{ Request^body = yes(Body) } ,
			parse_soapmessage(Body, NsBody, ErrMesg),
			(
			    { ErrMesg = yes(Mesg) },
			    { HttpCode = 415 },
			    { Result = Mesg }, 
			    { ResBody = no_body }
			;
			    { ErrMesg = no },
			    { get_procedure_call(NsBody, Proc) },

		 	    { make_web_request(NsBody, Proc, WebRequest) },
		
			    load_dynamic_library(uri_to_filename(Request^uri), 
				WebRequest, Result, HttpCode, ErrorFlag),
			    (
				{ ErrorFlag = no },
				{ generate_response_body(NsBody, Proc, 
					Result, ResBody0) },
				{ ResBody = string_body(ResBody0) }
			    ;
				{ ErrorFlag = yes },
				{ ResBody = no_body }
			    )
			)
		;
			% 200 - 299 is client request successful
			% 400 = Bad Request
			{ Request^body = no },
			{ ResBody = no_body },
			{ Result = "Body not found" },
			{ HttpCode = 400 }
		),
		{ generate_headers(Request^headers, ResBody, Result,
				Headers) },
		{ Response = response(HttpCode, Headers, 
				ResBody, yes) } 
	).

:- func uri_to_filename(string) = string.
uri_to_filename(URI) = "." `string__append` FileName :-
	( '/' = last_char(URI) ->
		FileName = URI `string__append` "index.html"
	;
		FileName = URI
	).

:- func last_char(string) = char.
last_char(Str)
	= string__unsafe_index(Str, string__length(Str) - 1).

:- pred generate_headers(list(header)::in, response_body::in, string::in, 
	list(header)::out) is det.
generate_headers(RequestHeaders, ResBody, ErrorMsg, Headers) :-
	list__filter(is_content_type_header, RequestHeaders, Headers0), 
	(
		ResBody = string_body(Body),
		string__length(Body, BodyLength),
		string__int_to_string(BodyLength, StringLength), 
		ConLen = [header("Content-Length:", StringLength, no)],
		list__append(ConLen, Headers0, Headers)
	;
		% includes error messages resulted from loading library 
		% in the header
		ResBody = no_body,
		Headers = [header(ErrorMsg, "", no)]
	).

:- pred is_content_type_header(header::in) is semidet.
is_content_type_header(header("Content-Type:", _, _)).
is_content_type_header(header("Content-type:", _, _)).

%-----------------------------------------------------------------------------%

:- pred send_response(S, response, io__state, io__state)
	<= ( stream__writer(S, string, io),
	     stream__writer(S, char, io)).
:- mode send_response(in, in, di, uo) is det.

send_response(S, Response) -->
	status_line(S, Response^respCode),
	headers(S, Response^respHeaders),
	stream__put(S, "\r\n"),
	body(S, Response^respBody).

:- pred status_line(S, http_code, io__state, io__state)
	<= stream__writer(S, string, io).
:- mode status_line(in, in, di, uo) is det.

status_line(S, HttpCode) -->
	stream__put(S, "HTTP/1.1 "),
	stream__put(S, http_code_to_string(HttpCode)),
	stream__put(S, reason(HttpCode)),
	stream__put(S, "\r\n").

:- func http_code_to_string(int) = string.

http_code_to_string(HttpCode) =
	( http_code_to_string_2(HttpCode, String) ->
		String
	;
		string__from_int(HttpCode)
	).

	% Use constant strings for the most common codes.
	% string__from_int is inefficient.
	%
:- pred http_code_to_string_2(int::in, string::out) is semidet.

http_code_to_string_2(200, "200").
http_code_to_string_2(400, "400").
http_code_to_string_2(404, "404").
http_code_to_string_2(411, "411").
http_code_to_string_2(500, "500").
http_code_to_string_2(501, "501").

:- pred headers(S, list(header), io__state, io__state)
	<= ( stream__writer(S, string, io),
	     stream__writer(S, char, io)).
:- mode headers(in, in, di, uo) is det.

headers(_, []) --> [].
headers(S, Headers) -->
	{ Headers = [header(Name, Value, Other)|Tail] },
	stream__put(S, Name), 
	stream__put(S, ' '), 
	stream__put(S, Value), 
	stream__put(S, ' '), 
	(
		{ Other = yes(String) } ->
		stream__put(S, String)
	;
		[]
	),
	stream__put(S, '\n'), 
	headers(S, Tail).

:- pred body(S, response_body, io__state, io__state)
	<= stream__writer(S, string, io).
:- mode body(in, in, di, uo) is det.

body(_, no_body) --> [].
body(S, string_body(String)) -->
	stream__put(S, String).

%-----------------------------------------------------------------------------%

