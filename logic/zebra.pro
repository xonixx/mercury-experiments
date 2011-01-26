neigh(Left, Right, List) :- 
        List = [Left | [Right | _]];
        List = [_ | [Left | Right]].

zebraowner(Houses, ZebraOwner):-
        member([englishman, _, red], Houses),
        member([spanish, jaguar, _], Houses),
        neigh([_, snail, _], [japanese, _, _], Houses),
        neigh([_, snail, _], [_, _, blue], Houses),
        member([ZebraOwner, zebra, _], Houses),
        member([_, _, green], Houses).


zebra(X) :- zebraowner([_, _, _], X).