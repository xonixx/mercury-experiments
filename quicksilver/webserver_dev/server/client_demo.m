%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University Of Melbourne
% This file may only be copied under the terms of the GNU General Public
% License - see the file COPYING
%---------------------------------------------------------------------------%
%
% File: client_demo.m
% Author: inch
%
% This module provide a sample of how to call the SOAP interface to send
% messages from the client to the SOAP server.
%
%---------------------------------------------------------------------------%


:- module client_demo.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module http, options.

:- import_module bool, char, exception, getopt.
:- import_module int, list, require, string.
:- import_module univ.

:- import_module soap_interface.

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
		{ getopt__lookup_string_option(OptTable, method, Method) },
		{ getopt__lookup_string_option(OptTable, uri, URI) },
		{ getopt__lookup_string_option(OptTable, xml, XML) },
		( 
		    { XML = "" }
		->
		    (
		        { Method_Handler = get_method_handler(Method) }
		    ->
		      	call(Method_Handler, OptTable, Host, Port, Method, 
				URI, Responses, Mesg)
		    ;
		  	{ error("Method not supported.") }
		    ),
		    display_mercury_response(Mesg, Responses)
	    	;
	    	    soap_call_xml_type(Host, Port, Method, URI, XML,
		    	Responses),
		    io__write_string(Responses) 
		)
	    )
	;
   	    { OptionsResult = error(OptionErrorString) },
	    io__write_string(OptionErrorString),
	    io__nl,
	    options_help
	).

%---------------------------------------------------------------------------%

:- type method_handler == pred(option_table(option), string, int, method, 
	uri, list(univ), string, io__state, io__state).
:- inst method_handler == (pred(in, in, in, in, in, out, out, di, uo) is det).

:- func get_method_handler(string) = method_handler.
:- mode get_method_handler(in) = out(method_handler) is semidet.

	% Choose a corresponding predicate for a particular method.
get_method_handler("Hello") = handle_Hello.
get_method_handler("GetStockPrice") = handle_GetStockPrice.
get_method_handler("Add3Ints") = handle_Add3Ints.
get_method_handler("PurchaseBook") = handle_PurchaseBook.

%---------------------------------------------------------------------------%

:- pred handle_Hello(option_table(option), string, int, method, uri, 
	list(univ), string, io__state, io__state).
:- mode handle_Hello(in, in, in, in, in, out, out, di, uo) is det.

handle_Hello(_OptTable, Host, Port, Method, URI, Responses, Mesg) --> 
	soap_call_mercury_type(Host, Port, Method, URI, [], Responses),
	{ Mesg = "" }.

%---------------------------------------------------------------------------%

:- pred handle_GetStockPrice(option_table(option), string, int, method, uri, 
	list(univ), string, io__state, io__state).
:- mode handle_GetStockPrice(in, in, in, in, in, out, out, di, uo) is det.

handle_GetStockPrice(OptTable, Host, Port, Method, URI, Responses, Mesg) --> 
	{ getopt__lookup_int_option(OptTable, int, Int) },
	(
		{ Int = -1 }
	->
		io__write_string("Parameter not supplied.\n"),
		io__write_string("Program terminated.\n"),
		{ Mesg = "" },
		{ Responses = [] }
	;
		soap_call_mercury_type(Host, Port, Method, URI, 
			[univ(Int)],  Responses),
		{ Mesg = "Stockprice = " }
	).

%---------------------------------------------------------------------------%

:- pred handle_Add3Ints(option_table(option), string, int, method, uri, 
	list(univ), string, io__state, io__state).
:- mode handle_Add3Ints(in, in, in, in, in, out, out, di, uo) is det.

handle_Add3Ints(OptTable, Host, Port, Method, URI, Responses, Mesg) --> 
	{ getopt__lookup_int_option(OptTable, add1, X) },
	{ getopt__lookup_int_option(OptTable, add2, Y) },
	{ getopt__lookup_int_option(OptTable, add3, Z) },
	(
		% If some of the arguments are not supplied, 
		% they are treated as 0. Only when all 
		% arguments are not supplied, the program
		% terminates.
		{ X = 0 }, { Y = 0 }, { Z = 0 } 
	->
		io__write_string("Parameter not supplied.\n"),
		io__write_string("Program terminated.\n"),
		{ Mesg = "" },
		{ Responses = [] }
	;
		soap_call_mercury_type(Host, Port, Method, URI, 
			[univ(X), univ(Y), univ(Z)], Responses),
		{ Mesg = "Add3Ints = " }
	).
%---------------------------------------------------------------------------%

:- pred handle_PurchaseBook(option_table(option), string, int, method, uri, 
	list(univ), string, io__state, io__state).
:- mode handle_PurchaseBook(in, in, in, in, in, out, out, di, uo) is det.

handle_PurchaseBook(OptTable, Host, Port, Method, URI, Responses, Mesg) -->
	{ getopt__lookup_string_option(OptTable, title, Title) },
	{ getopt__lookup_string_option(OptTable, author, Author)},
	(
	  	{ Title = "" }
	->
		io__write_string("Title not supplied.\n"),
		io__write_string("Program terminated.\n"),
		{ Mesg = "" },
		{ Responses = [] }
	;
		{ Author = "" }
 	-> 
		io__write_string("Author not supplied.\n"),
		io__write_string("Program terminated.\n"),
		{ Mesg = "" },
		{ Responses = [] }
	;
		soap_call_mercury_type(Host, Port, Method, URI, 
			[univ(Title), univ(Author)], Responses),
		{ Mesg = "" }
	). 
		    
%---------------------------------------------------------------------------%
:- pred display_mercury_response(string::in, list(univ)::in, io__state::di,
	io__state::uo) is det.

display_mercury_response(Message, Responses) -->  
	io__write_string(Message),
	list__foldl((pred(X::in, di, uo) is det --> 
		write(univ_value(X)), nl), 
		Responses).


