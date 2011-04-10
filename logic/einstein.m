:- module einstein.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe, list, solutions, logic.

:- type house ---> house(maybe(nationality), maybe(color), maybe(pet), maybe(cigarettes), maybe(drink)).

:- type nationality ---> englishman; spaniard; norwegian; ukrainian; japanese.

:- type color ---> red; yellow; blue; green; ivory.
:- type pet ---> dog; snails; fox; horse; zebra.
:- type cigarettes ---> kools; chesterfields; winston; lucky_strike; parliaments.
:- type drink ---> orange_juice; tea; coffee; milk; water.

:- instance unifiable_with_free(house) where [
	unify(
		house(N, C, P, S, D),
		house(N1, C1, P1, S1, D1)) = 
			house(unify(N, N1), unify(C, C1), unify(P, P1), unify(S, S1), unify(D, D1)),
			
	free_var = house(no,no,no,no,no)
].

unknown_house = free_var.

solve(!Street):-
        % The Englishman lives in the red house
	
	logic.member(house(yes(englishman),yes(red),no,no,no), !Street),
        
	% The Spaniard owns the dog
        
	logic.member(house(yes(spaniard),no,yes(dog),no,no), !Street),
        
	% The Norwegian lives in the first house on the lleft
        
	unify([house(yes(norwegian),no,no,no,no),unknown_house,unknown_house,unknown_house,unknown_house], !Street),
        
	% Kools are smoked in the yellow house.
        
	logic.member(house(no,yes(yellow),no,yes(kools),no), !Street),
        
	% The man who smokes Chesterfields lives in the house
        % next to the man with the fox.
        
	next(house(no,no,yes(fox),no,no), house(no,no,no,yes(chesterfields),no), !Street),
        
	% The Norwegian lives next to the blue house
        
	next(house(yes(norwegian),no,no,no,no), house(no,yes(blue),no,no,no), !Street),
        
	% The Winston smoker owns snails.
        
	logic.member(house(no,no,yes(snails),yes(winston),no), !Street),
        
	% The lucky strike smoker drinks orange juice
        
	logic.member(house(no,no,no,yes(lucky_strike),yes(orange_juice)), !Street),
        
	% The Ukrainian drinks tea
        
	logic.member(house(yes(ukrainian),no,no,no,yes(tea)), !Street),
        
	% The Japanese smokes parliaments
        
	logic.member(house(yes(japanese),no,no,yes(parliaments),no), !Street),
        
	% Kools are smoked in the house next to the house where the horse is kept.
        
	next(house(no,no,yes(horse),no,no), house(no,no,no,yes(kools),no), !Street),
        
	% Coffee is drunk in the green house
        
	logic.member(house(no,yes(green),no,no,yes(coffee)), !Street),
        
	% The green house is immediately to the right (your right) of the ivory house
        
	left(house(no,yes(ivory),no,no,no), house(no,yes(green),no,no,no), !Street),
        
	% Milk is drunk in the middle house.
        
	unify([unknown_house,unknown_house,house(no,no,no,no,yes(milk)),unknown_house,unknown_house], !Street),
        
	% And, finally, zebra and water :)
        
	logic.member(house(no,no,yes(zebra),no,no), !Street),
        logic.member(house(no,no,no,no,yes(water)), !Street).

next(H1, H2, !Street):-
        left(H1, H2, !Street);
        left(H2, H1, !Street).

left(H1, H2, !Street):-
        unify([H1,H2,unknown_house,unknown_house,unknown_house], !Street);
        unify([unknown_house,H1,H2,unknown_house,unknown_house], !Street);
        unify([unknown_house,unknown_house,H1,H2,unknown_house], !Street);
        unify([unknown_house,unknown_house,unknown_house,H1,H2], !Street).
	
main -->
	{ solutions(pred(S::out) is nondet :- solve([unknown_house,unknown_house,unknown_house,unknown_house,unknown_house], S), L)},
	print(L).

