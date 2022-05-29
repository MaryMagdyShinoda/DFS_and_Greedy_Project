%Problem 2
%deletiveEditing(['D','E','T','E','R','M','I','N','E','D'], ['T','R','M','E']).
%deletiveEditing(['D','E','T','E','R','M','I','N','E','D'], ['T','E','R','M']).


setCharFact('A').
setCharFact('B').
setCharFact('C').
setCharFact('D').
setCharFact('E').
setCharFact('F').
setCharFact('G').
setCharFact('H').
setCharFact('I').
setCharFact('J').
setCharFact('K').
setCharFact('L').
setCharFact('M').
setCharFact('N').
setCharFact('O').
setCharFact('P').
setCharFact('Q').
setCharFact('R').
setCharFact('S').
setCharFact('T').
setCharFact('U').
setCharFact('V').
setCharFact('W').
setCharFact('X').
setCharFact('Y').
setCharFact('Z').

remove1stOccurrenceChar([Target|Tail], Target, Tail):- !.
remove1stOccurrenceChar([Head|Tail], Target, [Head|Result]):-
    remove1stOccurrenceChar(Tail, Target, Result).

checkSubsequence(_, [], -1):- !.
checkSubsequence([], [_|_], 100):- !.

checkSubsequence([Head1|Tail1], [Head2|Tail2], Heu):-
    (
    Head1 = Head2
    -> checkSubsequence(Tail1, Tail2, Heu);
    checkSubsequence(Tail1, [Head2|Tail2], Heu)
).


getHeuristic(Stat, Target, Heu):-
    checkSubsequence(Stat, Target, Heu).


move(Stat, Target, FollowingState):-
    remove1stOccurrenceChar(Stat, Target, FollowingState).

getChildren(Stat, Target, UnvisitedList, Children):-
    findall(FollowingState, moves(Stat, Target, UnvisitedList, FollowingState), Children).

moves(Stat, Target, UnvisitedList, FollowingState):-
    setCharFact(Letter),
    membership(Letter, Stat),
    move(Stat, Letter, FollowingState),
    \+(membership(FollowingState, UnvisitedList)),
    getHeuristic(FollowingState, Target, Heuristic),
    Heuristic < 100.

path([Target|_], Target):- !.
path([Head|Tail], Target):-
    getChildren(Head, Target, Tail, Children),
    appending(Tail, Children, NewOpenList),
    path(NewOpenList, Target).

path([],_,_,_):-
    fail,nl,!.

deletiveEditing(Stat, Target):-
    path([Stat], Target).


%Member funcion
membership(Member,[Member|_]).
membership(Member,[_|Tail]):-
       membership(Member,Tail).


appending([],X,X).
appending([Head|Tail],X,[Head|S]) :-
	appending(Tail,X,S).
