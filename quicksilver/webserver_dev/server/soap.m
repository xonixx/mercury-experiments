%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: soap.m
% Author: conway, inch
%
% This module translates SOAP messages from XML messages to namespace 
% aware XML messages. It also generates web requests and responses. 
%
%---------------------------------------------------------------------------%


:- module soap.
:- interface.
:- import_module io, string.
:- import_module maybe.
:- import_module xml, xml.ns.
:- import_module web_methods.

	% Translates SOAP message to namespace aware SOAP message.
:- pred parse_soapmessage(string, ((xml.ns).nsDocument), maybe(string), 
	io__state, io__state).
:- mode parse_soapmessage(in, out, out, di, uo) is det.

	% Retrieves method name from the SOAP message.
:- pred get_procedure_call(nsDocument, nsElement).
:- mode get_procedure_call(in, out) is det.

        % Retrieves parameters from the SOAP message and generates a 
	% web request.
:- pred make_web_request(nsDocument, nsElement, web_method_request).
:- mode make_web_request(in, in, out) is det.

	% Generates response body in XML format.
:- pred generate_response_body(nsDocument, nsElement, string, string).
:- mode generate_response_body(in, in, in, out) is det.


%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module array, assoc_list, bool, char, map, require, pair.
:- import_module parsing, xml.cat, xml.doc,  xml.encoding, xml.parse.

%---------------------------------------------------------------------------%
% Parsing soap message
%---------------------------------------------------------------------------%

	% Parses soap message.
	% NsDoc is namespace aware soap message.
parse_soapmessage(SoapMessage, NsDoc, ErrorMesg) -->
 	pstate(mkEntity(SoapMessage), mkEncoding(utf8), init),
 	io((pred(Dirs0::out, di, uo) is det -->
 	  	get_environment_var("XML_DIRS", MStr),
 		(
 		    { MStr = no },
 		    { Str = "." }
 		;
 		    { MStr = yes(Str) }
 		),
 		{ split((':'), Str, Dirs0) }
 	), Dirs),
 	set(gDirs, dirs(Dirs)),
 	{ map__from_assoc_list([
 		"ASCII"		- mkEncoding(ascii7),
 		"ascii"		- mkEncoding(ascii7),
 		"Latin-1"	- mkEncoding(latin1),
 		"Latin1"	- mkEncoding(latin1),
 		"UTF-8"		- mkEncoding(utf8),
 		"utf-8"		- mkEncoding(utf8)
 	], Encodings) },
 	set(gEncodings, encodings(Encodings)),
  	document,
 	finish(Res),
	(
	 	{ Res = ok((_, Doc)) },
		{ nsTranslate(Doc, NsDoc) },
		{ ErrorMesg = no }
	;
		% XXX instead of error, should change to response
		%     with http code 415 Unsupported Media Type

		{ Res = error(Err) },
		{ string__append("parse_soapmessage failed: ", Err, ErrMsg) },
		{ ErrorMesg = yes(ErrMsg) },
		{ NsDoc = nsDoc([], 0, [], array([comment("")])) }
	).


:- pred split(char, string, list(string)).
:- mode split(in, in, out) is det.

split(C, Str0, Strs) :-
    string__to_char_list(Str0, Chars),
    split1(C, [], Strs0, Chars, _),
    reverse(Strs0, Strs).

:- pred split1(char, list(string), list(string), list(char), list(char)).
:- mode split1(in, in, out, in, out) is det.

split1(_C, Strs, Strs, [], []).
split1(C, Strs0, Strs) -->
	=([_|_]),
	split2(C, [], Cs0),
	{ reverse(Cs0, Cs) },
	( { Cs \= [] } ->
	    { string__from_char_list(Cs, Str) },
	    { Strs1 = [Str|Strs0] }
	;
	    { Strs1 = Strs0 }
	),
	split1(C, Strs1, Strs).

:- pred split2(char, list(char), list(char), list(char), list(char)).
:- mode split2(in, in, out, in, out) is det.

split2(_C, Cs, Cs, [], []).
split2(C, Cs0, Cs) -->
    [C0],
    ( { C = C0 } ->
    	{ Cs = Cs0 }
    ;
    	split2(C, [C0|Cs0], Cs)
    ).


%---------------------------------------------------------------------------%
% Getting method name
%---------------------------------------------------------------------------%

% XXX Assume only one procedure call per soap message

	% Gets method name.
get_procedure_call(NsDoc, Procedure) :-  
 	get_procedure(NsDoc, [], Procedurelist),
	list__index1_det(Procedurelist, 1, Procedure).

:- pred get_procedure(nsDocument, list(nsElement), list(nsElement)).
:- mode get_procedure(in, in, out) is det.
get_procedure(NsDoc, Acc0, Acc) :- 
 	get_procedure(NsDoc^content, NsDoc^root, Acc0, Acc).
 
:- pred get_procedure(array(nsContent), ref(nsContent), list(nsElement), 
	list(nsElement)).
:- mode get_procedure(in, in, in, out) is det.
 
get_procedure(ContentArray, ContentRef, Acc0, Acc) :-
 	lookup(ContentArray, ContentRef, Content),
 	(
 		Content = nsElement(Elem)
	->
 		(
			not(is_envelope_body(Elem^eName^localName))
		->
 			Acc = [Elem]
 		;
 			Kid = Elem^eContent,	
 			elem_foldl(get_procedure, ContentArray, Kid,
					Acc0, Acc)
 		)
 	;
		Acc = Acc0
 	).	
 
:- pred elem_foldl(pred(array(nsContent), ref(nsContent), 
	list(nsElement), list(nsElement)), array(nsContent),
	list(ref(nsContent)), list(nsElement), list(nsElement)).
:- mode elem_foldl(pred(in, in, in, out) is det, in, in, in, out) is det.
 
elem_foldl(_Pred, _, [], Acc, Acc).
elem_foldl(Pred, ContentArray, [Ref|Refs], Acc0, Acc) :-
	call(Pred, ContentArray, Ref, Acc0, Acc1),
	elem_foldl(Pred, ContentArray, Refs, Acc1, Acc).


%---------------------------------------------------------------------------%
% Generating web request
%---------------------------------------------------------------------------%

	% Filter out parameters (simple / compound types) from the
	% SOAP message and generates a web request.
make_web_request(NsDoc, Proc, Request) :-  
 	Param_Index = Proc^eContent, 	% array index pointing to parameters	
	content_foldl(get_parameters, NsDoc^content, Param_Index, [], 
			Parameters),
	Request =  web_method_request(Proc^eName^localName, Parameters,
			Proc^eName^nsURI).

:- pred get_parameters(array(nsContent), ref(nsContent),
	list(parameter), list(parameter)).
:- mode get_parameters(in, in, in, out) is det.

get_parameters(ContentArray, ContentRef, Acc0, Acc) :-
 	lookup(ContentArray, ContentRef, Content),
	(
		Content = nsElement(Elem)
	->
		Name = Elem^eName^localName,
		URI = Elem^eName^nsURI,

		% search any defined `xsi:type' attribute
		(
			web_methods__search_attributes(Elem^eAttrs, Type0)
		->
			Type = yes(Type0)
		;
			Type = no
		),

		% message is parsed from bottom-up, therefore need
		% to reverse the list to keep the parameters' order
		RevKids = Elem^eContent,
		list__reverse(RevKids, Kids),
		(	
			% For case where Kids points to 1 data only
			% Eg.
			%     <Author>Foo</Author>
			list__length(Kids, 1),		
			Kids = [Ref],
			lookup(ContentArray, Ref, Data),
			Data = data(Value0)
		->
			Value = yes(Value0), 
			Fields  = no,
			Acc = [parameter(Name, Type, URI, Value, Fields)|Acc0] 
		; 
			% For case where Kids points to elements and data
			% Eg.
			%     <Author>
			%	<surname>Foo</surname>
			%	<givenname>Bar</givenname>
			%     </Author>
			Value = no,		
			content_foldl(get_parameters, ContentArray, Kids, 
					[], Acc1), 
			Fields = yes(Acc1),
			Acc = [parameter(Name, Type, URI, Value, Fields)|Acc0] 
		)
	;
		Acc = Acc0		
	).

:- pred content_foldl(pred(array(nsContent), ref(nsContent), 
	list(parameter), list(parameter)), array(nsContent), 
	list(ref(nsContent)), list(parameter), list(parameter)).
:- mode content_foldl(pred(in, in, in, out) is det, in, in, in, out) 
	is det.

content_foldl(_Pred, _, [], Acc, Acc).
content_foldl(Pred, ContentArray, [Ref|Refs], Acc0, Acc) :-
	call(Pred, ContentArray, Ref, Acc0, Acc1),
	content_foldl(Pred, ContentArray, Refs, Acc1, Acc).


%---------------------------------------------------------------------------%
% Generating Response Body
%---------------------------------------------------------------------------%

% assume Method has prefix or default namespace 
% eg. m:GetStockPrice or GetStockPrice

generate_response_body(NsDoc, Method, Result, ResponseBody) :-
	generate_res_body(NsDoc^content, NsDoc^root, Method, Result, [], 
			[], ResponseBodyList),
	list__reverse(ResponseBodyList, ResponseBodyList0),
	string__append_list(ResponseBodyList0, ResponseBody).

:- pred generate_res_body(array(nsContent), ref(nsContent), nsElement,
	string, nsList, list(string), list(string)).
:- mode generate_res_body(in, in, in, in, in, in, out) is det.

generate_res_body(ContentArray, ContentRef, Method, Result, URIList0, 
		Acc0, Acc) :-
	lookup(ContentArray, ContentRef, Content),
	(
		Content = nsElement(Elem),
		is_envelope_body(Elem^eName^localName)
	->
		list__append(URIList0, Elem^eNamespaces, URIList),
		assoc_list__reverse_members(URIList, URIListRev),
		get_prefix(URIListRev, Elem^eName^nsURI, Elem^eName^localName,
			ElementName), 
		string__append("<", ElementName, ElemName),

		format_attrs(Elem^eAttrs, URIList, URIListRev, Attrs), 
		string__append_list(Attrs, AttrsString),
		string__append(ElemName, AttrsString, Elem_Attrs),
		
		Acc1 = [Elem_Attrs|Acc0],
		(
			not(is_body(Elem^eName^localName))
		->
			Kids = Elem^eContent,
			doc_foldl(generate_res_body, ContentArray, Kids, 
				Method, Result, URIList, Acc1, Acc2)
		;

			generate_method_response(Method, Result, ResBody),
			Acc2 = [ResBody|Acc1]  
		),
		make_end_tag(ElementName, EndTag),
		Acc = [EndTag|Acc2]
	;
		Acc = Acc0
	).


:- pred is_envelope_body(string::in) is semidet.
is_envelope_body("Envelope").
is_envelope_body("Header").
is_envelope_body("Body").

:- pred is_body(string::in) is semidet.
is_body("Body").

:- pred get_prefix(nsList::in, nsURI::in, string::in, string::out) is det. 

get_prefix(URIListRev, URI, ElementName0, ElementName) :-
	(	% element has prefix 
		% search_prefix(URIList, Elem^eName^nsURI, Prefix)
		assoc_list__search(URIListRev, URI, Prefix)
	->
		string__append(Prefix, ":", Name0),
		string__append(Name0, ElementName0, ElementName)
	;
		% element has default namespace
		ElementName = ElementName0 
	).

:- pred format_attrs(list(nsAttribute), nsList, nsList, list(string)).
:- mode format_attrs(in, in, in, out) is det.

format_attrs([], _, _, [">\n"]).
format_attrs([Attr|Attrs], URIList, URIListRev, StringList) :-

	(
		assoc_list__search(URIList, Attr^aName^localName, _URI)
	->
		Prefix = "\nxmlns:"
	;
		assoc_list__search(URIListRev, Attr^aName^nsURI, Prefix0)
	->
		string__append(Prefix0, ":", Prefix)
	;
	 	Prefix = "\n"	
	),
	string__append(Prefix, Attr^aName^localName, Attr1),
	string__append(Attr1, "=\"", Attr2),
	string__append(Attr2, Attr^aValue, Attr3),
	string__append(Attr3, "\"", Attr4),
	(
		Attrs \= []
	->
		string__append(Attr4, "\n",  Attr5)
	; 	
		Attr5 = Attr4	
	),
	% string__append(Attr4, "\n", Attr4)
	StringList = [Attr5 | StringList0],
	format_attrs(Attrs, URIList, URIListRev, StringList0).

:- pred generate_method_response(nsElement, string, string). 
:- mode generate_method_response(in, in, out) is det.
	
generate_method_response(Method, Result, ResBody) :-
	assoc_list__reverse_members(Method^eNamespaces, URIListRev),
	get_prefix(URIListRev, Method^eName^nsURI, Method^eName^localName,
			ElementName), 
	string__append("<", ElementName, ResBody0),
	string__append(ResBody0, "Response", ResBody1),

	format_attrs(Method^eAttrs, Method^eNamespaces, URIListRev, Attrs),
	string__append_list(Attrs, AttrsString),

	string__append(ResBody1, AttrsString, ResBody2),
	string__append(ResBody2, Result, ResBody3), 
	string__append(ResBody3, "\n", ResBody4),

	make_end_tag((ElementName ++ "Response"), EndTag),

	string__append(ResBody4, EndTag, ResBody).
	
:- pred make_end_tag(string::in, string::out) is det.

make_end_tag(ElementName, EndTag) :-
	string__append("</", ElementName, EndTag0), 
	string__append(EndTag0, ">\n", EndTag). 


:- pred doc_foldl(pred(array(nsContent), ref(nsContent), nsElement, 
        string, nsList, list(string), list(string)), 
	array(nsContent), list(ref(nsContent)), nsElement,  string, nsList, 
	list(string), list(string)).
:- mode doc_foldl(pred(in, in, in, in, in, in, out) is det, in, in, 
	in, in, in, in, out) is det.
	       
doc_foldl(_Pred, _, [], _, _, _, Acc, Acc).
	% get_first_element(ElemName0, FirstElem),
	% string__append("</", ElemName, FirstElem1),
	% string__append(FirstElem1, ">", FirstElem2),
	% list__reverse(Acc0, Acc1),
	% Acc = [FirstElem2|Acc0].
	% list__reverse(Acc2, Acc),
	% my_delete(ElemName0, ElemName).

doc_foldl(Pred, ContentArray, [Ref|Refs], Method, Result, URIs, 
		Acc0, Acc) :-
	doc_foldl(Pred, ContentArray, Refs, Method, Result, URIs, 
		Acc0, Acc1),
	call(Pred, ContentArray, Ref, Method, Result, URIs, 
		Acc1, Acc).

:- pred my_delete(list(T)::in, list(T)::out) is det.
my_delete([], []).
my_delete([_H|T], T).
