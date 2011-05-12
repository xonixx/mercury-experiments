%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne
% This file may only be copied under the terms of the GNU General Public
% License - see the file COPYING
%---------------------------------------------------------------------------%
%
% File: soap_inteface.m 
% Author: inch
%
% Reference:
% http://www.w3.org/TR/SOAP
%
% This module provide an interface for programmers to send messages
% to the SOAP server. It will provide as basic functionality the
% ability to do RPC over SOAP.  The hard-coded XML encoding for
% Mercury datatypes (the same encoding as the SOAP server currently
% expects) will be used to encode the RPC calls.
%
%---------------------------------------------------------------------------%


:- module soap_interface.
:- interface.
:- import_module io, list, string.
:- import_module univ.
:- import_module tcp.

:- type method == string.
:- type uri == string.
:- type xml == string.

	% Sends messages (in mercury types) to the SOAP server and returns
	% the response (in mercury types).
	% host and port specifies the host and port number that the client
	% connects to; method specifies the method name of the RPC; 
	% uri indicates the intent of the SOAP HTTP request (see reference
	% section 6.1); lists of univ are the input parameters of RPC and
	% the output responses from the server.
        
:- pred soap_call_mercury_type(host::in, port::in, method::in, 
	uri::in, list(univ)::in, list(univ)::out, 
	io__state::di, io__state::uo) is det.

	% Sends messages (in XML format) to the SOAP server and returns
	% the response (in XML format).
	% host and port specifies the host and port number that the client
	% connects to; method specifies the method name of the RPC; 
	% uri indicates the intent of the SOAP HTTP request (see reference
	% section 6.1); xmls are the message sends to the server and
	% the response receives from it.

:- pred soap_call_xml_type(host::in, port::in, method::in, uri::in, 
	xml::in, xml::out, io__state::di, io__state::uo) is det.


%---------------------------------------------------------------------------%

:- implementation.
:- import_module char.
:- import_module int, require.
:- import_module stream.
:- import_module stream_util.


soap_call_mercury_type(Host, Port, Method, SOAPuri, Parameters,
	Responses) --> 
	{ generate_request(Host, Method, SOAPuri, Parameters, Request) }, 
	tcp__connect(Host, Port, Result),
	(
		{ Result = ok(TCP_Connect) },
		service_connection(TCP_Connect, Request, XMLResponse)
	;
		{ Result = error(String) },
		{ error(String) }
	), 
	tcp__shutdown(TCP_Connect),
	(
		{ Decode_Handler = decode_response(Method) }, 
		{ call(Decode_Handler, XMLResponse, Responses) } 
	;
		{ error("Soap_call_mercury_type: Method not supported") }
	).


        % Choose a corresponding predicate to decode response for 
	% a particular method.

:- type decode_handler == pred(string, list(univ)).
:- inst decode_handler == (pred(in, out) is det).

:- func decode_response(string) = decode_handler.
:- mode decode_response(in) = out(decode_handler) is semidet.

	% XXX Add a handler for the new method here.

decode_response("Hello") = decode_Hello_response.
decode_response("GetStockPrice") = decode_SP_response.
decode_response("Add3Ints") = decode_AddInt_response.
decode_response("PurchaseBook") = decode_PB_response.


%--------------------------------------------------------------------------%

soap_call_xml_type(Host, Port, _Method, SOAPuri, XMLMesg, XMLResponse) -->
	{ generate_xml_request(Host, SOAPuri, XMLMesg, Request) }, 
	tcp__connect(Host, Port, Result),
	(
		{ Result = ok(TCP_Connect) },
		service_connection(TCP_Connect, Request, Response)
	;
		{ Result = error(String) },
		{ error(String) }
	), 
	tcp__shutdown(TCP_Connect),
	{ retrieve_body(Response, XMLResponse) }.
	

:- pred generate_xml_request(host::in, uri::in, xml::in, string::out) 
	is det.

generate_xml_request(Host, SOAPuri, XMLMesg, Request) :- 
	generate_xml_body(XMLMesg, Body),
	string__length(Body, Length),
	generate_header(Host, Length, SOAPuri, Header),
	Request = insert_cr(Header) ++ Body.
	
:- pred generate_xml_body(xml::in, string::out) is det.

generate_xml_body(XMLMesg, SOAPBody) :-
  	SOAPBody = "<Envelope><Body>" ++ XMLMesg ++ "</Body></Envelope>".

:- pred retrieve_body(string::in, xml::out) is det.

retrieve_body(Response, Body) :- 
	(
		string__sub_string_search(Response, "\r\n<", Pos)
	->
		string__length(Response, Length),
		string__right(Response, Length - Pos, Body) 
	;
		error("error in retrieving body") 
	).

%-------------------------------------------------------------------------% 

	% Generates HTTP header information and SOAP message in the body. 
:- pred generate_request(host, method, uri, list(univ), string).
:- mode generate_request(in, in, in, in, out) is det.

generate_request(Host, Method, SOAPuri, Parameters, Request) :- 
	(
		Body_Handler = generate_body(Method), 
		call(Body_Handler, Method, Parameters, Body) 
	;
		error("Generate message body failed.\n") 
	),
	string__length(Body, Length),
	generate_header(Host, Length, SOAPuri, Header),	
	Request = insert_cr(Header) ++ Body.

:- type body_handler == pred(string, list(univ), string).
:- inst body_handler == (pred(in, in, out) is det).

:- func generate_body(string) = body_handler. 
:- mode generate_body(in) = out(body_handler) is semidet.

	% XXX Add a body handler for the new method here.

generate_body("Hello") = generate_Hello_body.
generate_body("GetStockPrice") = generate_SP_body.
generate_body("Add3Ints") = generate_AddInt_body.
generate_body("PurchaseBook") = generate_PB_body.


%-------------------------------------------------------------------------% 
% Hello
%-------------------------------------------------------------------------% 

	% Generates SOAP message.
	% Hello pred has no input.
:- pred generate_Hello_body(string, list(univ), string).
:- mode generate_Hello_body(in, in, out) is det.

generate_Hello_body(MethodName, _Parameters, Body) :-
	insert_envelope(MethodName, "", Body). 

:- pred decode_Hello_response(string::in, list(univ)::out) is det.

decode_Hello_response(XMLResponse, Responses) :- 
	( 
		string__sub_string_search(XMLResponse, "<output>", Start),
		string__sub_string_search(XMLResponse, "</output>", End) 
	->
		string__left(XMLResponse, End, Response0),
		TagLength = 8,		% <output>
		string__right(Response0, End-Start-TagLength, Response1),
		Responses = [univ(Response1)]
	;
		error("decode error") 
	).

%--------------------------------------------------------------------------%
% Add3Ints
%--------------------------------------------------------------------------%

	% Generate message body for Add3Ints.
:- pred generate_AddInt_body(method::in, list(univ)::in, string::out)
	is det.

generate_AddInt_body(Method, Parameters, Body) :-
	list__map(create_xml_parameter, Parameters, XMLList),
	string__append_list(XMLList, XMLString), 
	insert_envelope(Method, XMLString, Body).

:- pred create_xml_parameter(univ::in, string::out) is det.

create_xml_parameter(ParameterAsUniv, ParameterAsXML) :-
	det_univ_to_type(ParameterAsUniv, ParameterAsInt),
	string__int_to_string(ParameterAsInt, ParameterAsString),
	ParameterAsXML = "<int>" ++ ParameterAsString ++ "</int>".

:- pred decode_AddInt_response(string::in, list(univ)::out) is det.

decode_AddInt_response(XMLResponse, Responses) :- 
	( 
		string__sub_string_search(XMLResponse, "<result>", Start),
		string__sub_string_search(XMLResponse, "</result>", End) 
	->
		string__left(XMLResponse, End, Response0),
		TagLength = 8,		% <result>
		string__right(Response0, End-Start-TagLength, Response1),
		(
		    string__to_int(Response1, ResponseAsInt)
		->
		    Responses = [univ(ResponseAsInt)]
		;
		    error("decode error")
		)
	;
		error("decode error") 
	).

%--------------------------------------------------------------------------%
% GetStockPrice
%--------------------------------------------------------------------------%

	% Generates SOAP message body for GetStockPrice.
	% XXX	assume no namespace and client has a copy of the schema
	% 	used in the server side to encode mercury types.
:- pred generate_SP_body(method, list(univ), string).
:- mode generate_SP_body(in, in, out) is det.

	% schema for GetStockPrice:
        % <element name="stocknum" type="xsd:int">

generate_SP_body(MethodName, Parameters, Body) :-
	% since client knows that this function takes in only
	% one parameter,  there is no need to call list_foldl to
	% translate the parameters.
	list__index1_det(Parameters, 1, Parameter),
	generate_SP_param(Parameter, ParamAsString),
	insert_envelope(MethodName, ParamAsString, Body).


:- pred generate_SP_param(univ, string).
:- mode generate_SP_param(in, out) is det.

generate_SP_param(ParamAsUniv, Tag) :-  
	det_univ_to_type(ParamAsUniv, ParamAsValue),	
	string__int_to_string(ParamAsValue, ParamAsString),
	Tag = "<stocknum>" ++ ParamAsString ++ "</stocknum>".

:- pred decode_SP_response(string::in, list(univ)::out) is det.

decode_SP_response(XMLResponse, Responses) :- 
	( 
		string__sub_string_search(XMLResponse, "<price>", Start),
		string__sub_string_search(XMLResponse, "</price>", End) 
	->
		string__left(XMLResponse, End, Response0),
		TagLength = 7,		% <price>
		string__right(Response0, End - Start - TagLength, Response1),
		(
		    string__to_int(Response1, ResponseAsInt)
		->
		    Response = ResponseAsInt
		;
		    error("decode error")
		),
		Responses = [univ(Response)]
	;
		error("decode error") 
	).

%-------------------------------------------------------------------------%
% PurchaseBook
%-------------------------------------------------------------------------%
	
	% Generate message body for PurchaseBook.
:- pred generate_PB_body(method::in, list(univ)::in, string::out) is det.
generate_PB_body(Method, Parameters, Body) :-
	generate_PB_parameters(Parameters, ParameterString0),
	ParameterString = "<book>" ++ ParameterString0 ++ "</book>",
	insert_envelope(Method, ParameterString, Body). 

:- pred generate_PB_parameters(list(univ)::in, string::out) is det.

generate_PB_parameters(ParamList, Body):-
	list__index1_det(ParamList, 1, TitleAsUniv),
	det_univ_to_type(TitleAsUniv, TitleAsString),
	Title = "<title>" ++  TitleAsString ++ "</title>",
	
	list__index1_det(ParamList, 2, AuthorAsUniv),
	det_univ_to_type(AuthorAsUniv, AuthorAsString),
	(
		string__sub_string_search(AuthorAsString, ",", Pos)
	->
		string__left(AuthorAsString, Pos, Surname),
		string__length(AuthorAsString, Length),
		string__right(AuthorAsString, Length - (Pos + 1), Firstname)
	;
		Surname = AuthorAsString,
		Firstname = " "
	),	
	Author = "<author>" ++ 
		 "<surname>" ++ Surname ++ "</surname>" ++
		 "<firstname>" ++ Firstname ++ "</firstname>" ++
		 "</author>",
	Body = Title ++ Author.

:- pred decode_PB_response(string::in, list(univ)::out) is det.

decode_PB_response(XMLResponse, Responses) :- 
	( 
		string__sub_string_search(XMLResponse, "<comment>", Start),
		string__sub_string_search(XMLResponse, "</comment>", End) 
	->
		string__left(XMLResponse, End, Comment0),
		TagLength = 9,		% <comment>
		string__right(Comment0, End - Start - TagLength, Comment1),
		Comment = [univ(Comment1)]
	;
		Comment = []
	),
	(
	    string__sub_string_search(XMLResponse, "<price>", Start1),
	    string__sub_string_search(XMLResponse, "</price>", End1)
	->
	    string__left(XMLResponse, End1, Price0),
	    TagLength1 = 7,		% <price>
	    string__right(Price0, End1 - Start1 - TagLength1, Price1),
	    Price = [univ("Price = " ++ Price1)]
	;
	    Price = []
	),
	list__append(Comment, Price, Responses).

	
%-------------------------------------------------------------------------%
% Shared functions
%-------------------------------------------------------------------------%

	% Generates HTTP header.
:- pred generate_header(host::in, int::in, uri::in, string::out) is det.

generate_header(Host, Length, SOAPuri, Headers) :-
	Header1 = "POST /libsoap_test_methods.so HTTP/1.1",
	Header2 = "Host: " ++ Host,
	Header3 = "Content-Type: text/xml", 
	string__int_to_string(Length, LengthAsString),
	Header4 = "Content-Length: " ++ LengthAsString,
	(
		SOAPuri = "no value"
	->
		Header5 = "SOAPAction:" 
	;
		Header5 = "SOAPAction: " ++ SOAPuri
	),
	Headers = insert_cr(Header1) ++ insert_cr(Header2) ++
		  insert_cr(Header3) ++ insert_cr(Header4) ++
		  insert_cr(Header5).

	% Generate SOAP Envelope

:- pred insert_envelope(string::in, string::in, string::out) is det.
insert_envelope(MethodName, Parameters, Body) :-  
	Body = 	"<Envelope><Body><" ++ MethodName ++ ">" ++
		Parameters ++ "</" ++ MethodName ++ ">" ++
		"</Body></Envelope>".

	% Insert carriage return into the line.
:- func insert_cr(string) = string.
insert_cr(Line) = Line ++ "\r\n". 

%------------------------------------------------------------------------%
	
	% Sends request to server using HTTP and receives response 
	% from server. 
:- pred service_connection(tcp::in, string::in, string::out, 
	io__state::di, io__state::uo) is det.

service_connection(TCP, Request, Response) -->
	send_request(TCP, Request),
	read_response(TCP, Response).

	% Sends the request to the server side.
:- pred send_request(S, string, io__state, io__state)
	<= stream__writer(S, string, io).
:- mode send_request(in, in, di, uo) is det.

send_request(S, Requests) -->
	stream__put(S, Requests).

	% Reads responses from server.
:- pred read_response(S, string, io__state, io__state)
	<= stream__reader(S, char, io, Error).
:- mode read_response(in, out, di, uo) is det.

read_response(S, Response) -->
	stream_util__read_line(S, LineResult),
	(
		{ LineResult = ok(Line) },
		{ string__from_char_list(Line, String) },
		{ Response = String ++ Response0 },
		read_response(S, Response0)
	;
		{ LineResult = eof },
		{ Response = "" }
	;
		{ LineResult = error(Error) },
		{ Msg = error_message(Error) },
		{ error(string__format("read_response: %s.", [s(Msg)])) }
	).
		
%-------------------------------------------------------------------------%

