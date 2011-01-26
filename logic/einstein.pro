solve(Street):-
        Street = [H1,H2,H3,H4,H5],
        % The Englishman lives in the red house
        member(house(englishman,red,_,_,_), Street),
        % The Spaniard owns the dog
        member(house(spaniard,_,dog,_,_), Street),
        % The Norwegian lives in the first house on the lleft
        H1 = house(norwegian,_,_,_,_),
        % Kools are smoked in the yellow house.
        member(house(_,yellow,_,kools,_), Street),
        % The man who smokes Chesterfields lives in the house
        % next to the man with the fox.
        next(house(_,_,fox,_,_), house(_,_,_,chesterfields,_), Street),
        % The Norwegian lives next to the blue house
        next(house(norwegian,_,_,_,_), house(_,blue,_,_,_), Street),
        % The Winston smoker owns snails.
        member(house(_,_,snails,winston,_), Street),
        % The lucky strike smoker drinks orange juice
        member(house(_,_,_,lucky_strike,orange_juice), Street),
        % The Ukrainian drinks tea
        member(house(ukrainian,_,_,_,tea), Street),
        % The Japanese smokes parliaments
        member(house(japanese,_,_,parliaments,_), Street),
        % Kools are smoked in the house next to the house where the horse is kept.
        next(house(_,_,horse,_,_), house(_,_,_,kools,_), Street),
        % Coffee is drunk in the green house
        member(house(_,green,_,_,coffee), Street),
        % The green house is immediately to the right (your right) of the ivory house
        left(house(_,ivory,_,_,_), house(_,green,_,_,_), Street),
        % Milk is drunk in the middle house.
        H3 = house(_,_,_,_,milk),
        % And, finally, zebra and water :)
        member(house(_,_,zebra,_,_), Street),
        member(house(_,_,_,_,water), Street).

next(H1, H2, Street):-
        left(H1, H2, Street);
        left(H2, H1, Street).

left(H1, H2, Street):-
        Street = [H1,H2,_,_,_];
        Street = [_,H1,H2,_,_];
        Street = [_,_,H1,H2,_];
        Street = [_,_,_,H1,H2].
