:- module letter_tree.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module list.

:- type node ---> node(letter, nodes).
:- type nodes == list(node).
:- type letter == string.

main(!IO) :-
	R = node("a",[ 
  		node("b",[
    			node("c", 
      				[node("d", [])]),
   			node("e",
      				[node("f", [])])]),
  		node("g",[   
    			node("h",
      				[node("i", [])]),
    			node("j", [])])]),
	print(R, !IO),
	print(""),
	% print_tree(0, 0, R, !IO).
/*
print_tree(Indent, Depth, node(L, Nodes)) -->
	letter_indent(Indent, Depth),
	print(L),
	print_nodes(Indent + 1, Depth, Nodes).

letter_indent(Indent, Depth, !IO) :-
		.

print_nodes(_,_,[], !IO).	
print_nodes(Indent, Depth, [N | NN]) -->
	print_tree(Indent, Depth, N),
	print_nodes(Indent, Depth + 1, NN).

*/
