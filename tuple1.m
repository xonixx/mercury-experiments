:- module tuple1.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module list.

main -->
	{
	Boy = {"Peter", 12}, % name, age
	Apple = {"Antonovka", 5}, % sort, storing time (months)
	Car = {"BMW", 1929}, % name, year
	%Car1 = {"BMW", 1929,1}, % name, year
	L = [Boy, Apple, Car]%,Car1]
	},
	print(L).