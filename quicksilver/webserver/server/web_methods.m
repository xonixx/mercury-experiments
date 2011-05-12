%-------------------------------------------------------------------------%
% Copyright (C) 2000, 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-------------------------------------------------------------------------%
%
% File: web_methods.m
% Author: inch 
%
% Reference:
% http://www.w3.org/TR/SOAP
% http://www.w3.org/TR/xmlschema-0/
% http://www.w3.org/TR/xmlschema-1/
% http://www.w3.org/TR/xmlschema-2/
% 
% This module handles remote procedure calls using SOAP protocol and
% generates corresponding responses. Depending on the name of the 
% procedure call, a different method will be called. 
%
%-------------------------------------------------------------------------%


:- module web_methods.
:- interface.
:- import_module bool, io, list, maybe, string.
:- import_module http.
:- import_module xml, xml.ns.

:- type web_method_request 
	--->	web_method_request(
			name	:: string,		% method name
			params 	:: list(parameter),	% list of parameters 
			uri     :: nsURI		% namespace (method) 
		).

:- type parameter
	--->	parameter(
			pName	:: string,		% parameter name
			pType	:: maybe(string), 	% type if any 
			pURI	:: nsURI,		% namespace (param)
		%	pValue  :: string
			pValue	:: maybe(string),	% data (simple type)
			pFields :: maybe(list(parameter))
							% Struct or Array
		).

	% Loads library, invokes method call and generates corresponding 
	% response.
:- pred load_dynamic_library(string::in, web_method_request::in,
	string::out, http_code::out, bool::out,  
	io__state::di, io__state::uo) is det. 

	% Search the attributes to find if there is any attribute
	% that has "xsd:type" defined.
:- pred search_attributes(list(nsAttribute)::in, string::out) is semidet.

%-----------------------------------------------------------------------%

:- implementation.
:- import_module int, require.
:- import_module univ.
:- import_module dl, name_mangle, soap_test_methods.

% :- func soap_library_file = string.
% soap_library_file = "soap_test_methods".


	% Types in XML can be defined either by using xsi:type attribute 
	% or by using schema. This predicate is used to search if
	% any attribute contains `xsi:type'.
	%
	% For all attribute list, each list can only contain one 
	% `xsi:type' attribute.
	% ie. <stocknum xsi:type="xsd:int">1</stocknum> is valid but
	%     <stocknum xsi:type="xsd:int" xsi:type="xsd:float">1</stocknum> 
	%     is not valid 
	% The XML parser has no checking on this.


search_attributes([], _) :- fail.
search_attributes([Attr|Attrs], Type) :-
	(
		is_type(Attr^aName^localName),
		check_attrs(Attrs)
	->
		Type = Attr^aValue
	;
		is_type(Attr^aName^localName),
		not(check_attrs(Attrs))
	->
		error("Invalid format: More than one xsi:type attribute 
			defined for one element")
	;
		search_attributes(Attrs, Type0),
		Type = Type0
	).

:- pred is_type(string::in) is semidet.
is_type("type").

:- pred check_attrs(list(nsAttribute)::in) is semidet.

check_attrs([]) :- true.
check_attrs([Attr|Attrs]) :-
	not(is_type(Attr^aName^localName)),
	check_attrs(Attrs).

%-----------------------------------------------------------------------%

	% Opens library file, invokes desire function, and gets back 
	% corresponding response and http code.
	% 
	% LibFile format = "./libfilename.so"
	%
load_dynamic_library(LibFile, Request, Response, HttpCode, ErrorFlag) --> 
	dl__open(LibFile, lazy, local, MaybeHandle),
	{ get_filename(LibFile, Filename) },
	(
	 	{ MaybeHandle = dl_error(OpenMsg) },
		{ string__append("dlopen failed: ", OpenMsg, OpenErrorMsg) },
		{ Response = OpenErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 } 		% 500 Internal Server Error
	;
	 	{ MaybeHandle = dl_ok(Handle) },
		(
			% Hello has no parameter
			{ Request^name = "Hello" }
		->
			call_Hello_pred(Handle, Filename, Request, 
				Response0, HttpCode0, ErrorFlag0)
		;
			% GetStockPrice has 1 parameter
			{ Request^name = "GetStockPrice" }
		->
			call_SP_func(Handle, Filename, Request, 
				Response0, HttpCode0, ErrorFlag0) 
		;
			% Add3Ints has 3 parameters
			{ Request^name = "Add3Ints" }
		->
			call_AI_func(Handle, Filename, Request, 
				Response0, HttpCode0, ErrorFlag0)
		;
			% PurchaseBook takes in a struct
			{ Request^name = "PurchaseBook" }
		->
			call_PB_pred(Handle, Filename, Request, 
				Response0, HttpCode0, ErrorFlag0)
		;
			{ Request^name = "Sumlist" }
		->
			call_list_pred(Handle, Filename, Request,
				Response0, HttpCode0, ErrorFlag0)
		;
			{ Response0 = "Method requested not implemented." }, 
			{ ErrorFlag0 = yes },
			{ HttpCode0 = 501 }	% 501 Not Implemented
		),

/* commented out dl__close/4 so that the .so can be referred to it 
   when generating responses. If the .so file is closed, any pointers 
   referencing the .so file will become unavailable, causing 
   runtime error: segmentation violation.

		dl__close(Handle, Result),
		(
			{ Result = error(CloseMsg) },
			{ string__append("dlclose failed: ", CloseMsg, 
				CloseErrorMsg) },
			{ Response1 = no }, 
			{ HttpCode1 = 500 }, 
			{ ChangeHttpCode = yes },
			{ error(CloseErrorMsg) } 
		;
			{ Result = ok },
			{ Response1 = Response0 },
			{ HttpCode1 = HttpCode0 },
			{ ChangeHttpCode = no }
		),
		(
			{ ChangeHttpCode = yes }
		->
			{ Response = Response1 },
			{ HttpCode = HttpCode1 }
		;
			{ Response = Response0 },
			{ HttpCode = HttpCode0 }
		)
*/
		{ Response = Response0 },
		{ ErrorFlag = ErrorFlag0 },
		{ HttpCode = HttpCode0 }
 	).

%-----------------------------------------------------------------------%
% Hello 
%-----------------------------------------------------------------------%

:- pred call_Hello_pred(handle, string, web_method_request, 
	string, http_code, bool, io__state, io__state).
:- mode call_Hello_pred(in, in, in, out, out, out, di, uo) is det.

call_Hello_pred(Handle, Filename, _Request, Response, HttpCode, ErrorFlag) -->
	{ HelloProc = mercury_proc(predicate, unqualified(Filename),
				"hello", 1, 0) },
	dl__mercury_sym(Handle, HelloProc, MaybeHello),
	(
		{ MaybeHello = dl_error(Msg) },
		{ string__append("dlsym failed: ", Msg, ErrorMsg) },
		{ Response = ErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 } 
	;
		{ MaybeHello = dl_ok(HelloPred0) },

		% Cast the higher-order term that we obtained
		% to the correct higher-order inst.
		{ HelloPred = inst_cast_hello(HelloPred0) },

		% Call the procedure whose address
		% we just obtained.
		{ HelloPred(HelloUniv) },
		{ det_univ_to_type(HelloUniv, HelloString) },	

		{ Response = HelloString },
		{ ErrorFlag = no },
		{ HttpCode = 200 }
	).

% dl__mercury_sym returns a higher-order term with inst `ground'.
% We need to cast it to the right higher-order inst, which for the
% `hello' procedure is `pred(di, uo) is det', before we can actually
% call it.  The function inst_cast_hello/1 defined below does that.

:- type hello_pred == pred(univ).
:- inst hello_pred == (pred(out) is det).

:- func inst_cast_hello(hello_pred) = hello_pred.
:- mode inst_cast_hello(in) = out(hello_pred) is det.
:- pragma c_code(inst_cast_hello(X::in) = (Y::out(hello_pred)),
        [will_not_call_mercury, thread_safe], "Y = X").

%-----------------------------------------------------------------------%
% Add3Ints 
%-----------------------------------------------------------------------%

:- pred call_AI_func(handle, string, web_method_request, 
	string, http_code, bool, io__state, io__state).
:- mode call_AI_func(in, in, in, out, out, out, di, uo) is det.

call_AI_func(Handle, Filename, Request, Response, HttpCode, ErrorFlag) -->
	{ AIProc = mercury_proc(function, unqualified(Filename),
				"add3Ints", 1, 0) },
	dl__mercury_sym(Handle, AIProc, MaybeAddInts),
	(
		{ MaybeAddInts = dl_error(Msg) },
		{ string__append("dlsym failed: ", Msg, ErrorMsg) },
		{ Response = ErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 } 
	;
		{ MaybeAddInts = dl_ok(AIFunc0) },
		{ wrapper(AIFunc) = inst_cast_addInts(wrapper(AIFunc0)) },
		{ list__map(lookup_AI_schema, Request^params, UnivList) },
		{ ResultAsUniv = AIFunc(UnivList) },
		{ det_univ_to_type(ResultAsUniv, ResultAsInt) },
		{ string__int_to_string(ResultAsInt, ResultAsString) },
		{ Response = "<result>" ++ ResultAsString ++ "</result>" },
		{ ErrorFlag = no },
		{ HttpCode = 200 }
	).

	% schema 
	% <element name="int" type="xsd:int">

:- pred lookup_AI_schema(parameter::in, univ::out) is det.
lookup_AI_schema(Param, ValueAsUniv) :-
	(
		Param^pName = "int",
		Param^pValue = yes(Value)
	->
	 	type_cast_parameter("int", Value, ValueAsUniv)
	;
		string__append("Element Name not defined in schema: ", 
				Param^pName, ErrorMsg),
		require__error(ErrorMsg)
	).

	% inst cast for add3Ints (function)
:- type addInts == (func(list(univ)) = univ ).
:- type addInts_wrapper ---> wrapper(addInts).
:- inst addInts_wrapper ---> wrapper(func(in) = out is det).

:- func inst_cast_addInts(addInts_wrapper) = addInts_wrapper.
:- mode inst_cast_addInts(in) = out(addInts_wrapper) is det.
:- pragma c_code(inst_cast_addInts(X::in) = (Y::out(addInts_wrapper)),
	[will_not_call_mercury, thread_safe], "Y=X").

%-----------------------------------------------------------------------%
% GetStockPrice 
%-----------------------------------------------------------------------%

:- pred call_SP_func(handle, string, web_method_request, 
	string, http_code, bool, io__state, io__state).
:- mode call_SP_func(in, in, in, out, out, out, di, uo) is det.

call_SP_func(Handle, Filename, Request, Response, HttpCode, ErrorFlag) -->
	{ list__length(Request^params, Arity) },
	{ GetSPProc = mercury_proc(function, unqualified(Filename),
			"get_stockprice", Arity, 0) }, 
	dl__mercury_sym(Handle, GetSPProc, MaybeGetStockPrice),
	(
		{ MaybeGetStockPrice = dl_error(Msg) }, 
		{ string__append("dlsym failed: ", Msg, ErrorMsg) },
		{ Response = ErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 }
	;
		{ MaybeGetStockPrice = dl_ok(SPProc0) }, 

		% Cast the higher-order term that we obtained
		% to the correct higher-order inst.
		{ wrapper(SPFunc) = inst_cast_stockprice(wrapper(SPProc0)) },

		% Convert parameters (string) to the corresponding types
		{ list__map(lookup_SP_schema, Request^params, UnivList) },

		% Call the procedure whose address we just obtained
		{ SPUniv = SPFunc(UnivList) },

		{ det_univ_to_type(SPUniv, SPInt) },	
		{ string__int_to_string(SPInt, SPString) },

		{ string__append("<price>", SPString, SPresult0) },
		{ string__append(SPresult0, "</price>", SPresult) },
		{ Response = SPresult },
		{ ErrorFlag = no },
		{ HttpCode = 200 }
	).


	% schema for GetStockPrice:
	% <element name="stocknum" type="xsd:int"/>
	% </element>

	% Lookup element name in schema, find the corresponding type
	% and type cast to that type.
:- pred lookup_SP_schema(parameter::in, univ::out) is det.

lookup_SP_schema(Param, ValueAsUniv) :-   
	(	
		% Case 1 <stocknum>1</stocknum>
		% web_method_request("GetStockPrice", 
		%   [parameter("stocknum", no, "", yes("1"), no)], 
		%   "some uri")
		Param^pName = "stocknum",
		Param^pValue = yes(Value)
	->
	 	type_cast_parameter("int", Value, ValueAsUniv)
	;
		% Case 2 <SOAP-ENC:int xmlns:SOAP-ENC="uri" id="int1">1
		%	 </SOAP-ENC:int>
		% web_method_request("GetStockPrice", 
		%    [parameter("int", no, "uri", yes("1"), no)], 
		%    "some uri")
		Param^pName = "int",
		Param^pValue = yes(Value)
	->
	 	type_cast_parameter("int", Value, ValueAsUniv)
	;	
		% assume Type must be simple type eg. int, float
		% XXX type may contain prefix 
		% Eg. xsd:int, xsd:float

		% Case 3 <stocknum xsi:type="xsd:int">1</stocknum>
		% web_method_request("GetStockPrice", 
		%   [parameter("stocknum", yes("xsd:int"), "",
		%   yes("1"), no)], "some uri")

		Param^pType = yes(Type),
		Param^pValue = yes(Value)
	->
		split_on_colon(Type, _Prefix, Suffix),	
	 	type_cast_parameter(Suffix, Value, ValueAsUniv)
	;
		string__append("Element Name not defined in schema: ", 
				Param^pName, ErrorMsg),
		require__error(ErrorMsg)
	).


	% inst cast for get_sp (predicate) 
:- type sp_pred == pred(list(univ), univ).
:- inst sp_pred == (pred(in, out) is det).
 
:- func inst_cast_sp(sp_pred) = sp_pred.
:- mode inst_cast_sp(in) = out(sp_pred) is det.
:- pragma c_code(inst_cast_sp(X::in) = (Y::out(sp_pred)),
	[will_not_call_mercury, thread_safe], "Y=X").

	% inst cast for get_stockprice (function)
:- type stockprice == (func(list(univ)) = univ ).
:- type stockprice_wrapper ---> wrapper(stockprice).
:- inst stockprice_wrapper ---> wrapper(func(in) = out is det).

:- func inst_cast_stockprice(stockprice_wrapper) = stockprice_wrapper.
:- mode inst_cast_stockprice(in) = out(stockprice_wrapper) is det.
:- pragma c_code(inst_cast_stockprice(X::in) = (Y::out(stockprice_wrapper)),
	[will_not_call_mercury, thread_safe], "Y=X").

%-----------------------------------------------------------------------%
% PurchaseBook
%-----------------------------------------------------------------------%

/* see Section 3.4 Complex Type Definition Details 
   ( http://www.w3.org/TR/xmlschema-1/ )

Complex types definitions are identified by their {name} and 
{target namespace}. Except for anonymous complex type definitions 
(those with no {name}), since type definitions (i.e. both simple and 
complex type definitions taken together) must be uniquely identified 
within an XML Schema, no complex type definition can have the same name 
as another simple or complex type definition. Complex type {name}s and 
{target namespace}s are provided for reference from instances (see
xsi:type (2.6.1)), and for use in the XML Representation of Schemas 
and Schema Components (4) (specifically in element). See References to 
schema components across namespaces (6.2.3) for the use of component 
identifiers when importing one schema into another.
*/
	% schema for PurchaseBook:
	%
	% <element name="book" type="tns:book"/>
	% <element name="author" base="tns:author"/>
	%
	% <complexType name="book">
	%   <sequence>
	%     <element name="title" type="xsd:string"/>
	%     <element name="author" type="tns:author"/>
	%    % <element name="intro" type="xsd:string"/>
	%   </sequence>
	% </complexType>
	%    
	% <complexType name="author">
	%   <sequence>
	%     <element name"surname" type="xsd:string"/>
	%     <element name"firstname" type="xsd:string"/>
	%   </sequence>
	% </complexType>
	%
	% Mercury representation:
	%
	% :- type book
	%	--->	book(
	%			title 	:: string,
	%			author 	:: author,
	%		%	intro	:: string
	%		).
	%	
	% :- type author
	%	--->	author(
	%			surname	  :: string,
	%			firstname :: string
	%		).

:- pred call_PB_pred(handle, string, web_method_request,
	string, http_code, bool, io__state, io__state).
:- mode call_PB_pred(in, in, in, out, out, out, di, uo) is det.

call_PB_pred(Handle, Filename, Request, Response, HttpCode, ErrorFlag) -->
	{ GetPBProc = mercury_proc(predicate, unqualified(Filename),
			"purchase_book", 2, 0) }, 
	dl__mercury_sym(Handle, GetPBProc, MaybePurchaseBook),
	(
		{ MaybePurchaseBook = dl_error(Msg) }, 
		{ string__append("dlsym failed: ", Msg, ErrorMsg) },
		{ Response = ErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 }
	;
		{ MaybePurchaseBook = dl_ok(PBProc0) }, 

		% Cast the higher-order term that we obtained
		% to the correct higher-order inst.
		{ PBProc = inst_cast_pb(PBProc0) },

		% Convert parameters (string) to the corresponding types
		{ list__map(lookup_PB_schema, Request^params, UnivList) },
		
		% Call the procedure whose address we just obtained
		{ call(PBProc, UnivList, PBUniv) },

		{ det_univ_to_type(PBUniv, PBInt) },	
		( 
		    	{ PBInt \= 0 }
		->
			{ string__int_to_string(PBInt, PBString) },
			{ Response = "<comment>Purchase order placed " ++
				     	"successfully</comment>\n<price>" ++
					PBString ++ "</price>" }
		;
			{ Response = "<comment>No such book. Purchase " ++
					"order not placed.</comment>" }	
		),
		{ ErrorFlag = no },
		{ HttpCode = 200 }
	).


:- pred lookup_PB_schema(parameter::in, univ::out) is det.

lookup_PB_schema(Param, ValueAsUniv) :-   
	(
		Param^pName = "book",
		Param^pFields = yes(FieldList)
	->
		get_PB_param(FieldList, "title", Title0),
		lookup_PB_schema(Title0, TitleAsUniv),
		det_univ_to_type(TitleAsUniv, Title),

		get_PB_param(FieldList, "author", Author0),
		lookup_PB_schema(Author0, AuthorAsUniv),
		det_univ_to_type(AuthorAsUniv, Author),
	
		% get_PB_param(FieldList, "intro", Intro0),
		% lookup_PB_schema(Intro0, IntroAsUniv),
		% det_univ_to_type(IntroAsUniv, Intro),

		% ValueAsBook = book(Title, Author, Intro),
		ValueAsBook = book(Title, Author),
		ValueAsUniv = univ(ValueAsBook)
	;	
		Param^pName = "title",
		Param^pValue = yes(Value)
	->
		type_cast_parameter("string", Value, ValueAsUniv)
	;
		Param^pName = "author",
		Param^pFields = yes(FieldList)
	->
		get_PB_param(FieldList, "surname", Surname0),
		lookup_PB_schema(Surname0, SurnameAsUniv),
		det_univ_to_type(SurnameAsUniv, Surname),

		get_PB_param(FieldList, "firstname", Firstname0),
		lookup_PB_schema(Firstname0, FirstnameAsUniv),
		det_univ_to_type(FirstnameAsUniv, Firstname),

		ValueAsAuthor = author(Surname, Firstname),
		ValueAsUniv = univ(ValueAsAuthor)
	;
		Param^pName = "surname",
		Param^pValue = yes(Value)
	->
		type_cast_parameter("string", Value, ValueAsUniv)
	;
		Param^pName = "firstname",
		Param^pValue = yes(Value)
	->
		type_cast_parameter("string", Value, ValueAsUniv)
	;
		Param^pName = "intro",
		Param^pValue = yes(Value)
	->
		type_cast_parameter("string", Value, ValueAsUniv)
	;
		require__error("Element Structure not defined in schema.")
	).

:- pred get_PB_param(list(parameter)::in, string::in, parameter::out) is det.

get_PB_param(ParamList, SearchString, Parameter) :-
	list__filter((pred(X::in) is semidet :-
		X = parameter(SearchString,_,_,_,_)), ParamList, Result),
	list__index1_det(Result, 1, Parameter).	

	% inst cast for purchase_book
:- type pb_pred == pred(list(univ), univ).
% :- type pb_pred == pred(book, univ).
:- inst pb_pred == (pred(in, out) is det).
 
:- func inst_cast_pb(pb_pred) = pb_pred.
:- mode inst_cast_pb(in) = out(pb_pred) is det.
:- pragma c_code(inst_cast_pb(X::in) = (Y::out(pb_pred)),
	[will_not_call_mercury, thread_safe], "Y=X").

%-----------------------------------------------------------------------%
% Sumlist 
%-----------------------------------------------------------------------%

/*
schema:
        <element name="list" type="tns:list"/>
        <element name="nil" type="tns:nil"/>
        <element name="cons" type="tns:cons"/>

        <complexType name="list">
          <sequence>
            <choice>
              <element name="nil" type="tns:nil"/>
              <element name="cons" type="tns:cons"/>
            </choice>
          </sequence>
        </complexType>

        <complexType name="nil>
          <complexContent>
            <restriction base="xsd:anyType">
            </restriction>
          </complexContent>
        </complexType>
         
    or  <complexType name="nil">        shorthand for complex content
        </complexType>                  that restricts anyType

        <complexType name="cons">
          <sequence>
            <element name="head" type="xsd:anyType">
            <element name="tail" type="tns:list"/>
          </sequence>
        </complexType>
*/

:- pred call_list_pred(handle, string, web_method_request,
	string, http_code, bool, io__state, io__state).
:- mode call_list_pred(in, in, in, out, out, out, di, uo) is det.

call_list_pred(Handle, Filename, Request, Response, HttpCode, ErrorFlag) -->
	{ GetListProc = mercury_proc(predicate, unqualified(Filename),
			"sum_list", 2, 0) }, 
	dl__mercury_sym(Handle, GetListProc, MaybeListProc),
	(
		{ MaybeListProc = dl_error(Msg) }, 
		{ string__append("dlsym failed: ", Msg, ErrorMsg) },
		{ Response = ErrorMsg },
		{ ErrorFlag = yes },
		{ HttpCode = 500 }
	;
		{ MaybeListProc = dl_ok(ListProc0) }, 

		{ ListProc = inst_cast_list(ListProc0) },
		% parse the parameters to obtain a list of string
		{ list__foldl(retrieve_list, Request^params, [], StringList) },
		% cast the list of string to list of int
		{ type_cast_list("int", StringList, UnivList) },	
		{ call(ListProc, [UnivList], ResultAsUniv) },
		{ det_univ_to_type(ResultAsUniv, ResultAsInt) },
		{ string__int_to_string(ResultAsInt, ResultAsString) },
		{ Response = "<sum>" ++ ResultAsString ++ "</sum>" }, 
		{ ErrorFlag = no },
		{ HttpCode = 200 }
	).

:- pred retrieve_list(parameter::in, list(string)::in, list(string)::out) 
	is det.

retrieve_list(Param, Acc0, Acc) :-   
	(
		Param^pName = "list",
		Param^pFields = yes(Nil),
		Nil = [parameter("nil", no, "", no, yes([]))]
	->
		Acc = Acc0
	;
		Param^pName = "list",
		Param^pFields = yes(Cons)
	->
		list__foldl(retrieve_list, Cons, Acc0, Acc1),
		Acc =  Acc1  
	;
		Param^pName = "cons",
		Param^pFields = yes(Head_Tail)
	->
		list__foldl(retrieve_list, Head_Tail, Acc0, Acc)
	;
		Param^pName = "head",
		Param^pValue = yes(Value)
	->
		list__reverse(Acc0, RevAcc0),
		RevAcc = [ Value | RevAcc0 ],
		list__reverse(RevAcc, Acc)
	;
		Param^pName = "tail",
		Param^pFields = yes(Tail)
	->
		list__foldl(retrieve_list, Tail, Acc0, Acc)
	;
		error("decode list error")
	).

:- pred type_cast_list(string::in, list(string)::in, univ::out) is det.
type_cast_list(Type, List, UnivList) :-   
 	(
 		Type = "int",
		list__map(string__to_int, List, ListAsInt)
 	->
		UnivList = univ(ListAsInt)
 	;
		Type = "float",
		list__map(string__to_float, List, ListAsFloat)
	->
		UnivList = univ(ListAsFloat)
	;
		Type = "string"
	->
		UnivList = univ(List)
	;
 		require__error("Type cast list failed")
 	).
			 		
	% inst cast for list 
:- type list_pred == pred(list(univ), univ).
:- inst list_pred == (pred(in, out) is det).
 
:- func inst_cast_list(list_pred) = list_pred.
:- mode inst_cast_list(in) = out(list_pred) is det.
:- pragma c_code(inst_cast_list(X::in) = (Y::out(list_pred)),
	[will_not_call_mercury, thread_safe], "Y=X").

%-----------------------------------------------------------------------%
% Shared functions 
%-----------------------------------------------------------------------%

	% Returns filename from ./libfilename.so 
:- pred get_filename(string::in, string::out) is det.
	
get_filename(LibFile, Filename) :-
	string__split(LibFile, 5, _Left, Filename0),
	string__length(Filename0, Length),
	string__left(Filename0, Length-3, Filename).

	% Separates prefix and suffix.
:- pred split_on_colon(string::in, string::out, string::out) is det.

split_on_colon(Name, Prefix, Suffix) :-
        ( 
            string__sub_string_search(Name, ":", Index)
        ->
            string__length(Name, Length), 
            string__right(Name, Length-(Index+1), Suffix),
            string__left(Name, Index, Prefix)
        ;
            Suffix = Name,
            Prefix = "" 
        ).

	% Used to convert data value from string to the desire type
	% and return it as a univ.
:- pred type_cast_parameter(string::in, string::in, univ::out) is det.
type_cast_parameter(Type, ValueAsString, ValueAsUniv) :-   
 	(
 		Type = "int",
		string__to_int(ValueAsString, ValueAsInt)
 	->
		ValueAsUniv = univ(ValueAsInt)
 	;
		Type = "float",
		string__to_float(ValueAsString, ValueAsFloat) 
	->
		ValueAsUniv = univ(ValueAsFloat)
	;
		Type = "string"
	->
		ValueAsUniv = univ(ValueAsString)
	;
		Type = "char",
		string__index(ValueAsString, 0, ValueAsChar)
	->
		ValueAsUniv = univ(ValueAsChar)
	;
 		require__error("Type cast failed")
 	).
			 		




