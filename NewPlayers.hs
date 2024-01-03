
module NewPlayers where
    import DomsMatch
    import System.Random
    import Data.List
    import Debug.Trace
    import Data.Maybe

{- Using tacticOpener, Maximise, and Capitalise -}
    betterPlayer :: DomsPlayer
    betterPlayer dominos board player scores
      | board == InitBoard = tacticOpener dominos board player scores
      {-  To start the function it is given, an invalid domino and a negative score so it can start looking for
                    a better domino which will be any playable domino to begin with  -}
      | otherwise = tacticMaximise ((-1),(-1)) L (-1) hand board player scores
      where
      {-  Will either pass the hand unfiltered or a selection of dominos that'll keep the other player knocking
                    tacticMaximise will then select the highest scoring  -}
        hand = tacticCapitalise player board dominos

{- Using tacticMaximise -}
    maxPlayer :: DomsPlayer
    maxPlayer dominos board player scores = tacticMaximise ((-1),(-1)) L (-1) dominos board player scores

{- Using tacticOpener, Capitalise, and Random -}
    simplePlayer :: DomsPlayer
    simplePlayer dominos board player scores
        | board == InitBoard = tacticOpener dominos board player scores
        | otherwise = tacticRandom hand board player scores
        where
          hand = tacticCapitalise player board dominos

{-  Checks if the other player is knocking, then tries to produce a list of dominos from their hand
            that'll keep the other player knocking  -}
    tacticCapitalise :: Player -> DominoBoard -> Hand -> Hand
    tacticCapitalise _ InitBoard hand = hand
    tacticCapitalise player (Board (n, _) (_, m) history) hand
      | (player == lastPlayer && dominos /= [] ) = dominos
      | otherwise = hand
      where
        (_, lastPlayerR, moveNumR) = last history -- gets the relevant info from the right side
        (_, lastPlayerL, moveNumL) = head history -- gets the relevant info from the left side
        lastPlayer = if moveNumR > moveNumL then lastPlayerR else lastPlayerL -- compares the ends to find which was played last
        {- Uses list comprehension to filter out the invalid moves according to the capitalisePatterns function -}
        dominos = [ x | x <- hand, capitalisePatterns x (n,m)]

{- Used in the list comprehension for the dominos that'll keep the other player knocking -}
    capitalisePatterns :: Domino -> (Int, Int) -> Bool
    capitalisePatterns (x, y) (a, b)
      | (( x == a || x == b ) && ( y == a || y == b )) = True
      | otherwise = False


{- A recursive function that'll find the highest scoring or winning domino to play -}
    tacticMaximise :: Domino -> End -> Int -> DomsPlayer
    tacticMaximise topDomino end _ [] _ _ _ = (topDomino, end) -- end of hand base case
    tacticMaximise topDomino end score (domino:rest) board player (ps1, ps2)
      | (playerScore + score) == 61 = (topDomino, end) -- If the last topDomino would win the game play it
      {- replaces the topDomino with the current domino if it can score higher, then move onto the rest of the hand -}
      | (leftScore > score && leftBoard /= Nothing )  = tacticMaximise domino L leftScore rest board player (ps1, ps2)
      | (rightScore > score && rightBoard /= Nothing ) = tacticMaximise domino R rightScore rest board player (ps1, ps2)
      | otherwise = tacticMaximise topDomino end score rest board player (ps1, ps2)
      where
        playerScore = if player == P1 then ps1 else ps2 -- selects the right players score
        leftBoard = playDom player domino board L
        rightBoard = playDom player domino board R
        leftScore = maybe 0 scoreBoard $ leftBoard -- if the domino is unplayable it'll default to 0
        rightScore = maybe 0 scoreBoard $ rightBoard

{- Checks if players hand contains a 5,4 domino to play -}
    tacticOpener :: DomsPlayer
    tacticOpener (domino:rest) board player scores
      | domino == (5, 4) = (domino, R)
      | domino == (4, 5) = (domino, R)
      | rest == [] = (domino, R)
      | otherwise = tacticOpener rest board player scores

{- A recursive function that'll play the first domino it can -}
    tacticRandom :: DomsPlayer
    tacticRandom (domino:rest) board player scores
      | leftBoard /= Nothing = (domino,L)
      | rightBoard /= Nothing = (domino,R)
      | otherwise = tacticRandom rest board player scores
      where
        leftBoard = playDom player domino board L
        rightBoard = playDom player domino board R
