%KB

tile(1, dots, a).
tile(1, dots, b).
tile(1, dots, c).
tile(1, dots, d).

tile(2, dots, a).
tile(2, dots, b).
tile(2, dots, c).
tile(2, dots, d).

tile(3, dots, a).
tile(3, dots, b).
tile(3, dots, c).
tile(3, dots, d).

tile(4, dots, a).
tile(4, dots, b).
tile(4, dots, c).
tile(4, dots, d).

tile(5, dots, a).
tile(5, dots, b).
tile(5, dots, c).
tile(5, dots, d).

tile(6, dots, a).
tile(6, dots, b).
tile(6, dots, c).
tile(6, dots, d).

tile(7, dots, a).
tile(7, dots, b).
tile(7, dots, c).
tile(7, dots, d).

tile(8, dots, a).
tile(8, dots, b).
tile(8, dots, c).
tile(8, dots, d).

tile(9, dots, a).
tile(9, dots, b).
tile(9, dots, c).
tile(9, dots, d).

tile(north, sp, a).
tile(north, sp, b).
tile(north, sp, c).
tile(north, sp, d).

tile(west, sp, a).
tile(west, sp, b).
tile(west, sp, c).
tile(west, sp, d).

tile(south, sp, a).
tile(south, sp, b).
tile(south, sp, c).
tile(south, sp, d).

tile(east, sp, a).
tile(east, sp, b).
tile(east, sp, c).
tile(east, sp, d).

tile(red, sp, a).
tile(red, sp, b).
tile(red, sp, c).
tile(red, sp, d).

tile(green, sp, a).
tile(green, sp, b).
tile(green, sp, c).
tile(green, sp, d).

tile(white, sp, a).
tile(white, sp, b).
tile(white, sp, c).
tile(white, sp, d).

tiles([tile(1, dots, a),
tile(1, dots, b),
tile(1, dots, c),
tile(1, dots, d),

tile(2, dots, a),
tile(2, dots, b),
tile(2, dots, c),
tile(2, dots, d),

tile(3, dots, a),
tile(3, dots, b),
tile(3, dots, c),
tile(3, dots, d),

tile(4, dots, a),
tile(4, dots, b),
tile(4, dots, c),
tile(4, dots, d),

tile(5, dots, a),
tile(5, dots, b),
tile(5, dots, c),
tile(5, dots, d),

tile(6, dots, a),
tile(6, dots, b),
tile(6, dots, c),
tile(6, dots, d),

tile(7, dots, a),
tile(7, dots, b),
tile(7, dots, c),
tile(7, dots, d),

tile(8, dots, a),
tile(8, dots, b),
tile(8, dots, c),
tile(8, dots, d),

tile(9, dots, a),
tile(9, dots, b),
tile(9, dots, c),
tile(9, dots, d),

tile(north, sp, a),
tile(north, sp, b),
tile(north, sp, c),
tile(north, sp, d),

tile(west, sp, a),
tile(west, sp, b),
tile(west, sp, c),
tile(west, sp, d),

tile(south, sp, a),
tile(south, sp, b),
tile(south, sp, c),
tile(south, sp, d),

tile(east, sp, a),
tile(east, sp, b),
tile(east, sp, c),
tile(east, sp, d),

tile(red, sp, a),
tile(red, sp, b),
tile(red, sp, c),
tile(red, sp, d),

tile(green, sp, a),
tile(green, sp, b),
tile(green, sp, c),
tile(green, sp, d),

tile(white, sp, a),
tile(white, sp, b),
tile(white, sp, c),
tile(white, sp, d)]).

list([tile(1, dots, a),
tile(white, sp, b),
tile(5, dots, c),
tile(red, sp, d)]).

%start sets the list of tiles, distributes tiles to the player and AI using distributeP, sorts both lists using sort1, makes the players tiles readable using writeNeat.
%The player starts with 14 tiles, and must discard one using discd, and then the game can officially begin with play
start:-write("Start mahjong game."), 
		tiles(DrawPile), 
		distributeP(DrawPile, 14, [], HandPlayer, DrawPile1), 
		distributeP(DrawPile1, 13, [], HandAI, UpdatedDrawPile2),
 		sort1(HandPlayer, SortedPlayer), 
 		sort1(HandAI, SortedAI),
		nl, nl, nl, 
		write("Your tiles are: "), 
		sleep(2), nl, nl, 
		writeNeat( SortedPlayer), 
		discd(SortedPlayer, UpdatedPlayerHand, Discarded), 
		writeDiscarded(Discarded, [], 0, NewDiscarded),
		play(1, UpdatedPlayerHand, SortedAI, NewDiscarded, UpdatedDrawPile2).

append([],L,L).
append([A|R],L,[A|C]) :-
    append(R,L,C).

%distributeP(DrawPile, ReqTiles, AppendTo, NewHand, UpdatedDrawPile1) is true if after distributing ReqTiles number many tiles to the player, 
%UpdatedDrawPile1 is the updated list of DrawPile excluding the distributed tiles, and the distributed tiles are appended to AppendTo to equal NewHand

distributeP(DrawPile, 1, AppendTo, NewHand, UpdatedDrawPile1) :- length(DrawPile, NumTiles), is(I, random(NumTiles-1)), 
																 nth0(I, DrawPile, Chosen), append(AppendTo,[Chosen],NewHand),
																 delete(DrawPile, Chosen, UpdatedDrawPile1).

distributeP(DrawPile, Num, AppendTo, NewHand, UpdatedDrawPile1) :- length(DrawPile, NumTiles), 
																	is(Index, random(NumTiles-1)), 
																	nth0(Index, DrawPile,Chosen),
																	append(AppendTo,[Chosen],NewHand1), 
																	delete(DrawPile, Chosen, UpdatedDrawPile2), 
																	is(Num2, Num-1),
																	distributeP(UpdatedDrawPile2, Num2, NewHand1, NewHand, UpdatedDrawPile1).


%writeNeat(A,C) is true if given list A of tiles, C is the same list of tiles, except in the form of Tile(X,Y,Z) to XY
writeNeat(Hand) :- writeNeat1(Hand, Result), write(Result).
writeNeat1([], []).
writeNeat1([tile(X,Y,_)|T], [Result1|Result]) :- atom_concat(X,Y, Result1), writeNeat1(T, Result).


%discd(Hand,UpdatedHand, Discarded) is true if given user input X, the UpdatedHand is equal to Hand minus the chosen tile and index X, and the Discarded list now contains that tile.

discd(Hand, UpdatedHand, Discarded) :-nl, nl, nl,
									write("Please choose a tile to discard by choosing the index (1-14)"), 
									sleep(2), nl,nl, read(X), 			
									discdBound(X, ProperIndex),		
									is(Index, ProperIndex-1),
									nth0(Index, Hand, Discarded),
									delete(Hand, Discarded, UpdatedHand), nl, nl.



discdBound(Num, Num) :- (Num < 15).
discdBound(Num, Proper) :- (Num > 14), write("Please choose an index between 1 and 14"), nl,nl, read(X), discdBound(X,Proper). 

%sort1(Hand, SortedHand) is true if SortedHand is the sorted version of Hand.
sort1(Hand, SortedHand) :- sort2(Hand, D, Sp), sort(D, D1), sort(Sp, Sp2), append(D1, Sp2, SortedHand).


%sort2(Hand, Dots, Specials) is true if Dots is a list of all dot tiles in Hand, and Specials is a list of all special tiles in Hand
sort2([],[],[]).
sort2([tile(X,dots,Z)|T], [tile(X,dots,Z)|D], Sp) :- sort2(T,D,Sp).
sort2([tile(X,sp,Z)|T], D,[tile(X,sp,Z)|Sp]) :- sort2(T,D,Sp).




%play(Turn, PI, AI, TilesInPlay) :- Turn is 0 (for player turn) or 1 (for AI turn), PI is the players hand, AI is the AIs hand, and TilesInPlay is the current tiles in play

play(_, _,_,_,[]) :- write("No more tiles left, tie"), sleep(2).

% checks to see if AI has fulfilled the win condition
play(1, _, AI, _, TilesInPlay) :- drawTile(1, TilesInPlay, AI, _, NewAI), 
								win(NewAI),nl, 
								write("AI won! Their hand was: "), 
								sleep(1), nl,
								writeNeat(NewAI), sleep(1).

% checks to see if player has fulfilled the win condition
play(0, P1, _, _, TilesInPlay) :- drawTile(0, TilesInPlay, P1, _, NewP1, DrewTile),
								win(NewP1), 
								nl, nl,nl,
								write("You picked up tile: "), 
								sleep(1), write(DrewTile), 
								nl, nl, nl,
								write("You win! Your winning hand is: "), 
								sleep(1),nl, writeNeat(NewP1), sleep(1).
                            
% AI receives a tile and has to discard a tile
play(1, P1, AI, Discarded, TilesInPlay) :- drawTile(1, TilesInPlay, AI, NewTilesInPlay, NewAI), nl, nl,
											\+win(NewAI), nl, nl,									
											pickTile(NewAI, Tile, Discarded), nl, nl,
											delete(NewAI, Tile, FinalAI), 
											writeDiscarded(Tile, Discarded, 1, NewDiscarded), nl,											
											play(0, P1, FinalAI, NewDiscarded, NewTilesInPlay).
                      
% player receives a tile and has to discard a tile
play(0, P1, AI, Discarded, TilesInPlay) :- drawTile(0, TilesInPlay, P1, NewTilesInPlay, NewP1, TileDrew),
											\+win(NewP1), 
											nl, nl, 
											write("You picked up tile: "), nl,
											sleep(1), write(TileDrew), 
											nl, nl, 
											write("Your tiles are now: "), 
											nl, writeNeat(NewP1),
											discd(NewP1, FinalP1, Tile),
											writeDiscarded(Tile, Discarded, 0, NewDiscarded), 
											write("Your new tiles are: "), nl, 
											writeNeat(FinalP1),
											play(1, FinalP1, AI, NewDiscarded, NewTilesInPlay).


%drawTile(Turn, TilesInPlay, Hand, NewTilesInPlay, NewHand) is true if NewHand is equal to Hand with a new random tile from TilesInPlay, 
%															and if NewTilesInPlay is equal to TilesInPlay without the selected random tile

drawTile(_, [tile(X,Y,Z)], Hand, [], NewHand2) :- append(Hand,[tile(X,Y,Z)],NewHand),sort1(NewHand, NewHand2).

drawTile(1, TilesInPlay, AI, NewTilesInPlay, NewAI2) :- length(TilesInPlay, Num), Num > 1, 
														is(N, random(Num)), nth0(N, TilesInPlay, C), 
														append(AI,[C],NewAI),
														sort1(NewAI, NewAI2),
														delete(TilesInPlay, C, NewTilesInPlay). 													
														


drawTile(0, TilesInPlay, P1, NewTilesInPlay, NewP2, C) :- length(TilesInPlay, Num), Num > 1, 
																 is(N, random(Num)), nth0(N, TilesInPlay, C),
																 append(P1,[C],NewP1), 
																 sort1(NewP1, NewP2),
																 delete(TilesInPlay, C, NewTilesInPlay).		
																					



% pickTile(AI, Tile, Discarded) is true if Tile is equal to the best tile in the AIs hand "AI" to discard based on the tiles already discarded in Discarded
% checks to see how many patterns it can make, and removes those from the list (max 4 patterns)
% It then checks to see how many pairs it can make

pickTile(AI, Tile, Discarded) :- pattern(AI, ListToUse),pattern(ListToUse, ListToUse1),pattern(ListToUse1, ListToUse2),pattern(ListToUse2, ListToUse3), 
								pickTilePair(ListToUse3, Discarded, 0, FinalList),getLeast(FinalList, Tile, Discarded).
pickTile(AI, Tile, Discarded) :- pattern(AI, ListToUse),pattern(ListToUse, ListToUse1),pattern(ListToUse1, ListToUse2),\+pattern(ListToUse2, _), 
								pickTilePair(ListToUse2, Discarded,0, FinalList),getLeast(FinalList, Tile, Discarded).
pickTile(AI, Tile, Discarded) :-pattern(AI, ListToUse),pattern(ListToUse, ListToUse1),\+pattern(ListToUse1, _), nl, nl,pickTilePair(ListToUse1, Discarded,0, FinalList),getLeast(FinalList, Tile, Discarded).
pickTile(AI, Tile, Discarded) :- pattern(AI, ListToUse),\+pattern(ListToUse, _), write(ListToUse),pickTilePair(ListToUse, Discarded, 0, FinalList),getLeast(FinalList, Tile, Discarded).
pickTile(AI, Tile, Discarded) :- \+pattern(AI, _), pickTilePair(AI, Discarded,0, FinalList),getLeast(FinalList, Tile, Discarded).


%pickTilePair(AI, Tile, Discarded, Turn) removes tiles from the possible discard list given that they form a pair, and given a pair where the rest of the tiles are
%already discarded, will only remove it from potential discard if there are no other pairs in this situation


pickTilePair(AI, Discarded, Turn, FinalList) :- pair(AI, ListToUse, Tile1), count(Discarded, Tile1, X), X<2, pickTilePair(ListToUse, Discarded, Turn, FinalList).
pickTilePair(AI, Discarded, 0, FinalList) :- pair(AI, ListToUse, Tile1), count(Discarded, Tile1, 2), pickTilePair(ListToUse, Discarded,1, FinalList).
pickTilePair(AI, Discarded, 1, AI) :- pair(AI, _, Tile), count(Discarded, Tile, 2).
pickTilePair(AI, Discarded, _, AI) :- \+pair(AI, Discarded, _).



%AI=(1,3,5)
%(3,1,0)-findMin get Index (2)
%Tile=Array[2] = 5, Til

% Checks discarded list to see which tiles appear the most in order to make a decision of which tile in hand would be best to discard.
getLeast([tile(X,Y,Z)],tile(X,Y,Z), _).
getLeast(AI,Tile, []):- length(AI, Num), Num>1, is(N, random(Num-1)), nth0(N, AI, Tile).
getLeast(AI, Tile, Discarded) :- getLeastThree(AI, Tile, Discarded).
getLeast(AI, Tile, Discarded) :- getLeastTwo(AI, Tile, Discarded).
getLeast(AI, Tile, Discarded) :- getLeastOne(AI, Tile, Discarded).
getLeast(AI, Tile, Discarded) :- getLeastZero(AI, Tile, Discarded).

getLeastZero([Tile|_], Tile, Discarded) :- count(Discarded, Tile, 0).
getLeastZero([_|T], Tile, Discarded) :- getLeastZero(T, Tile, Discarded).

getLeastOne([Tile|_], Tile, Discarded) :- count(Discarded, Tile, 1).
getLeastOne([_|T], Tile, Discarded) :- getLeastOne(T, Tile, Discarded).

getLeastTwo([Tile|_], Tile, Discarded) :- count(Discarded, Tile, 2).
getLeastTwo([_|T], Tile, Discarded) :- getLeastTwo(T, Tile, Discarded).

getLeastThree([Tile|_], Tile, Discarded) :- count(Discarded, Tile, 3).
getLeastThree([_|T], Tile, Discarded) :- getLeastThree(T, Tile, Discarded).



%count(List, Element, Output)
count([],_,0).
count([tile(A,B,_)|Y],tile(A,B,_),N):- count(Y,tile(A,B,_),W),N is W + 1.
count([tile(A,_,_)|Y],tile(C,D,_),N):- count(Y,tile(C,D,_),N),A\=C.


win(Hand) :- pattern(Hand, Hand1), pattern(Hand1, Hand2), pattern(Hand2, Hand3), pattern(Hand3, Hand4), pair(Hand4, [], _). 
writeDiscarded(tile(X,Y,Z), Discarded, 1, DiscardedSorted) :- nl, nl, write("AI disdcarded the tile : "), atom_concat(X,Y,New), write(New), sleep(2), nl, nl, write("The total discarded tiles are: "), nl, 
sort1([tile(X,Y,Z)|Discarded], DiscardedSorted), writeNeat(DiscardedSorted), sleep(1). 


writeDiscarded(tile(X,Y,Z), Discarded, 0, DiscardedSorted) :- nl, nl, write("You disdcarded the tile : "), atom_concat(X,Y,New), write(New), sleep(2), nl, nl, write("The total discarded tiles are: "), nl, 
sort1([tile(X,Y,Z)|Discarded], DiscardedSorted), writeNeat(DiscardedSorted),nl, nl, sleep(1). 


pattern(X,Y) :- patternthr(X,Y).
pattern(X,Y) :- patternstr(X,Y).
patternthr([tile(X,Y,_), tile(X,Y,_), tile(X,Y,_)|T], T).


%Checks if triplets or straights exist in list
patternthr([tile(X,Y,Z), tile(A,B,C)|T], [tile(X,Y,Z)|T1]) :- patternthr([tile(A,B,C)|T], T1).
patternstr([tile(X, dots,_)|T], T2) :- X<8, X>0,is(X2, X+1), is(X3, X2+1), member(tile(X2,dots,_), T), member(tile(X3,dots,_), T), 
delete1(T, tile(X2,dots,_), T1), delete1(T1,tile(X3,dots,_),T2).
patternstr([tile(X,dots,Z)|T], [tile(X,dots,Z)|T1]) :- patternstr(T, T1).

delete1([tile(X,Y,Z)|T], tile(X,Y,Z), T).
delete1([H|T], Tile, [H|T1]) :- delete1(T, Tile, T1).


% Checks for pairs in a list
pair([tile(X,Y,Z), tile(X,Y,_)|T], T, tile(X,Y,Z)).
pair([tile(X,Y,Z), tile(A,B,C)|T], [tile(X,Y,Z)|T1], Tile) :- pair([tile(A,B,C)|T], T1, Tile).



%[tile(1,dots,b),tile(1,dots,c), tile(1,dots,a),tile(2,dots,b), tile(2,dots,a),tile(7,dots,b),tile(7,dots,c), tile(7,dots,a),tile(green,sp,b),tile(green,sp,c), tile(green,sp,a),tile(white,sp,a),tile(white,sp, a), tile(white,sp,b)]


% AI has a chance to win or its a draw. (checks win condition and tie condition)
% play(0, [tile(1,dots,b), tile(1,dots,c), tile(1,dots,d), tile(2,dots,a), tile(2,dots,b), tile(2,dots,c), tile(7,dots,a), tile(7,dots,d), tile(green,sp, a), tile(green,sp,b), tile(white,sp,a), tile(white,sp,b), tile(white,sp,d)], [tile(3,dots,a), tile(3,dots,b), tile(3,dots,c), tile(5,dots,b), tile(5,dots,d), tile(east, sp, a), tile(east,sp,c), tile(east,sp,d), tile(red,sp,b), tile(red,sp,d), tile(west,sp,a), tile(west,sp,b), tile(west,sp,c)], [tile(east,sp,b)], [tile(red,sp,c), tile(9,dots,d)]).

% You have a chance to win or its a draw. (checks win condition and tie condition) 
%  play(0, [tile(1,dots,b), tile(1,dots,c), tile(1,dots,d), tile(2,dots,a), tile(2,dots,b), tile(2,dots,c), tile(7,dots,a), tile(7,dots,d), tile(green,sp, a), tile(green,sp,b), tile(white,sp,a), tile(white,sp,b), tile(white,sp,d)], [tile(3,dots,a), tile(3,dots,b), tile(3,dots,c), tile(5,dots,b), tile(5,dots,d), tile(east, sp, a), tile(east,sp,c), tile(east,sp,d), tile(red,sp,b), tile(red,sp,d), tile(west,sp,a), tile(west,sp,b), tile(west,sp,c)], [tile(east,sp,b)], [tile(red,sp,c), tile(green,sp,d)]).

% Player receives a hand of random 14 tiles which is sorted
%start.
%reset.
%start.

% Shared drawable deck between player and AI becomes empty. Game should tell you that game is over
% play(1, _, AI, _, TilesInPlay).

% AI decides to drop non-straight tile
% pickTile([tile(1, dots, a),tile(2, dots, a),tile(3, dots, a),tile(north, sp, a)],tile(north, sp, a),[]).

% AI decides to drop non-triplet tile
% pickTile([tile(1, dots, a),tile(1, dots, b),tile(1, dots, c),tile(north, sp, a)],tile(north, sp, a),[]).

% AI decides to drop non-pair tile
% pickTile([tile(1, dots, a),tile(1, dots, b),tile(3, dots, a)],tile(3, dots, a),[]).

% AI decides to drop tile that has all of its sisters in the discarded tile list
% pickTile([tile(1, dots, a),tile(5, dots, b),tile(3, dots, a)],tile(3, dots, a),[tile(3, dots, b),tile(3, dots, c),tile(3, dots, d)]).

% AI decides to drop tile that has only 1 of its sisters in the discarded tile list 
% pickTile([tile(1, dots, a),tile(5, dots, b),tile(3, dots, a)],tile(3, dots, a),[tile(3, dots, d)]).

% (Complex) AI chooses correct pattern sequence to drop correct tile which is choose 2 triplets pattern instead of 1 straight pattern.
%  pickTile([tile(1, dots, a),tile(1, dots, b),tile(1, dots, c),tile(2, dots, a),tile(3, dots, a),tile(3, dots, b),tile(3, dots, c)],tile(2, dots, a),[]).

% AI chooses to delete a tile when it only has 2 pairs but both of them have all there sisters in the removed list
% pickTile([tile(1, dots, a),tile(1, dots, b),tile(3, dots, a),tile(3, dots,b)],tile(3, dots, _),[tile(1, dots, c),tile(1, dots, d),tile(3, dots, c),tile(3, dots,d)]).

% (Complex) AI chooses correct pattern sequence to drop correct tile which is 2 straights instead of 1 triplet.
% pickTile([tile(1, dots, a),tile(2, dots, a),tile(7, dots, a)],tile(7, dots, a), []).

% Should pick tile(7,dots,a)
% pickTile([tile(1,dots,b), tile(1,dots,d), tile(2,dots,a), tile(2,dots,b), tile(3,dots,a), tile(3,dots,b), tile(3, dots,c), tile(7,dots,a)], Tile, [tile(7,dots,b)]).

%[tile(2,dots,a),tile(2,dots,b),tile(3,dots,b),tile(4,dots,b),tile(7,dots,a),tile(7,dots,b),tile(7,dots,d),tile(9,dots,a),tile(9,dots,b),tile(east,sp,b),tile(north,sp,c),tile(red,sp,c),tile(south,sp,a),tile(south,sp,d)]

/*
pickTile([tile(2,dots,a), tile(2,dots,b), tile(3,dots,c), tile(4,dots,d), tile(green,sp,a)], T, []).
T = tile(2, dots, b) . IMPLEMENT BETTER CHOICE
*/







