{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE ParallelListComp #-}
module Main where

import Prelude hiding (lookup)
import Lib
import Graphics.Gloss
    ( line,
      rectangleSolid,
      pictures,
      play,
      scale,
      text,
      translate,
      color,
      makeColorI,
      Display(InWindow),
      Picture(Circle, ThickCircle),
      Color )
import Graphics.Gloss.Interface.IO.Interact
import Data.Sequence as Seq hiding (reverse)
import Data.Maybe
import GHC.Base (Float, RealWorld)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Fixed (mod')
import Debug.Trace



-- TYPES
-- ==============================================

data Player = Red | Blue
    deriving (Eq, Show)

data State = Title | Play | Pause | End
    deriving (Eq, Show)

-- Board uses sequences over lists as they provide logarithmic lookup and update rather than linear.
-- Each square is occupied by a Maybe Player. This is because a square could have a blue token, a red token,
-- or none at all.
type Board = (Seq (Seq (Maybe Player)))

data Game = Game {
    board :: Board,
    player :: Player,
    state :: State,
    scores :: (Int, Int),
    titleFlash :: Float
}
    deriving (Eq, Show)

-- COLOURS
-- =============================================================================================

backgroundColor :: Color
backgroundColor = makeColorI 252 252 252 255

backgroundRectColor :: Color
backgroundRectColor = makeColorI 238 238 238 255

backgroundRectColorTransparent :: Color
backgroundRectColorTransparent = makeColorI 238 238 238 150

backgroundBlueTransparent :: Color
backgroundBlueTransparent = makeColorI 60 138 216 50

backgroundRedTransparent :: Color
backgroundRedTransparent = makeColorI 255 125 160 50

gridItemColor :: Color
gridItemColor = makeColorI 55 71 79 80

gridOutlineColor :: Color
gridOutlineColor = makeColorI 55 71 79 200

redCircleColor :: Color
redCircleColor = makeColorI 252 75 88 255

blueCircleColor :: Color
blueCircleColor = makeColorI 60 138 216 255

-- USEFUL DIMENSIONS
-- =============================================================================================

screenWidth :: Int
screenWidth = 700

screenHeight :: Int
screenHeight = 600

cellDimension :: Float
cellDimension = 100

-- STATIC PICTURES
-- =============================================================================================

blueCircle :: Picture
blueCircle = color blueCircleColor $ ThickCircle 10 40

redCircle :: Picture
redCircle = color redCircleColor $ ThickCircle 10 40

generateTitle :: Picture
generateTitle = translate (-330) 0 $ color blueCircleColor $ text "Connect 4"

generateSubTitle :: Picture
generateSubTitle = translate (-180) (-100) $ scale 0.2 0.2 $ color redCircleColor $ text "Left mouse button to start"

-- Scores are stored as a pair where the first int is the red score, and the second is the blue score
score :: (Int, Int)
score = (0, 0)

-- INITIAL STATE
-- =============================================================================================

-- Starts with the grid full of Nothings, using the replicate function to automatically fill
-- the sequences. Starts at the title and with the scores zeroed.
initialState :: Game
initialState = Game {
    board = Seq.replicate 6 (Seq.replicate 7 Nothing),
    player = Red,
    state = Title,
    scores = score,
    titleFlash = 0
}

-- GLOSS GAME FUNCTIONS
-- =============================================================================================

-- Entry point
main :: IO ()
main = play (InWindow "Connect 4" (800, 900) (1, 1))
            backgroundColor
            20
            initialState
            gameToPicture
            inputHandler
            updateGame

-- Takes a current game and converts it to a set of images based on what the current state of the game is. All states have
-- the grey background rectangle as a base layer. Title displays the name of the game and a subheading, which depending
-- on how much time has passed either displays or doesn't to give it a flashing effect. Pictures displays the current board
-- and who's turn it is. Pause simply brings up the pause screen, and End overlays the board with the color of the player that
-- just won the game.
gameToPicture :: Game -> Picture
gameToPicture game = pictures [ color backgroundRectColor (rectangleSolid 735 630) ,
                                case state game of
                                    Title -> pictures [generateTitle, if titleFlash game `mod'` 0.7 < 0.5 then
                                                                            generateSubTitle
                                                                      else
                                                                            pictures []]
                                    Play -> pictures [translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) (playingBoard (board game)),
                                                      translate (-70) (-350) $ scale 0.2 0.2 $ text (show (player game) ++ "'s turn")]
                                    Pause -> pauseScreen game
                                    End -> pictures [translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) (playingBoard (board game)),
                                                     case player game of
                                                        -- Blue pattern displays red because as soon as the move is played,
                                                        -- the current player changes. So it will be the previous player who
                                                        -- got the win.
                                                        Blue -> color backgroundRedTransparent (rectangleSolid 735 630)
                                                        Red -> color backgroundBlueTransparent (rectangleSolid 735 630)
                                                     ]
                              ]

-- Based on input given, decide what to do with the game. Pattern matched against which input is given. 
inputHandler :: Event -> Game -> Game
-- If the input is a left mouse button click and it's on the title screen, move to the game by changing the state of the game
-- to Play. If it's on Play, a click is counted as a move being made. The cursor position can be accessed from the event,
-- this is converted to grid coordinates and a move is attempted using these coordinates. Nothing happens when clicked while
-- paused. Clicking at the end screen restarts the game with an empty board.
inputHandler (EventKey (MouseButton LeftButton) Up _ position) game = case state game of
                                                                        Title -> initialState { state = Play }
                                                                        Play -> makeMove game (positionAsCellLocation position)
                                                                        Pause -> game
                                                                        End -> makeMove (game {state = Play,
                                                                                               board = Seq.replicate 6 (Seq.replicate 7 Nothing)})
                                                                                        (positionAsCellLocation position)
-- If space bar is pressed, effects only happen in Pause or Play state. If playing, pressing space will cause the pause
-- screen to be displayed. Pressing it again will go back to the game
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) game = case state game of
                                                                    Play -> game {state = Pause}
                                                                    Pause -> game {state = Play}
                                                                    _ -> game
-- No other user inputs should be considered
inputHandler _ game = game

-- Gets called 20x a second, just used for flashing subtitle. Doesn't affect the game in any other state.
updateGame :: Float -> Game -> Game
updateGame time game = case state game of
                Title -> game {titleFlash = titleFlash game + time}
                _ -> game



-- RENDERING IMAGES
-- ==============================================================================================================================



-- Drawing displayed when the game is paused. Shows that the game is paused, and also displays the scores of both colors.
-- Scores are part of the game data structure, so they can be accessed from the parameter. The red score is the first int of the
-- pair, while the blue score is the second.
pauseScreen :: Game -> Picture
pauseScreen game = pictures [
                             translate (-210) 0 $ text "Paused",
                             translate (-75) (-70) $ color redCircleColor $  scale 0.2 0.2 $ text ("Red wins: " ++ show (fst (scores game))),
                             translate (-77) (-110) $ color blueCircleColor $  scale 0.2 0.2 $ text ("Blue wins: " ++ show (snd (scores game)))
                             ]


-- Render the current game as firstly the grid, followed by any moves that have been made. The function renderImage
-- will draw whatever is at some set of coordinates on the board, so we need to apply this function to every 
-- point to get the full game displayed.
playingBoard :: Board -> Picture
playingBoard board = pictures ([color gridItemColor $ grid, color gridOutlineColor $ gridOutline]
                                ++ [renderImage board (x, y) | x <- [0..6], y <- [0..5]])


-- Given the coordinates of a square, determine whether nothing, a red circle or a blue circle should be drawn there.
-- Found by getting firstly the row of the board from the y value. Lookup returns type maybe depending on whether
-- it found what it was looking for (in this case the row). It is returning a Maybe Sequence. This means we need to use 
-- fromMaybe every time we use lookup, and I have given it the default value of empty here. This value doesn't really matter 
-- as this function will never be passed coordinates that are out range. Then to find the value of the square at the x 
-- coordinate of this row, we have to use lookup again, but because of how lookup works it gives us a value of type
-- Maybe (Maybe Player). We want the (Maybe Player) part, so fromMaybe is used again. Then, depending on what this 
-- value is, we either draw the equivalent of nothing, being a circle with no radius, a blue circle, or a red circle.
renderImage :: Board -> (Int, Int) -> Picture
renderImage board (x, y) = let row = fromMaybe empty (lookup y board)
                               cell = fromMaybe Nothing (lookup x row)
                                  in case cell of
                                      Nothing -> Circle 0
                                      Just Red -> translate (fromIntegral x * cellDimension + cellDimension * 0.5) (fromIntegral y * cellDimension + cellDimension * 0.5) redCircle
                                      Just Blue -> translate (fromIntegral x * cellDimension + cellDimension * 0.5) (fromIntegral y * cellDimension + cellDimension * 0.5) blueCircle


-- Render the grid as a series of vertical and horizontal lines. Only renders 'internal' lines. Places one line
-- vertically every 100px 6 times, and does the same but horizontally 5 times. Line constructor takes a start and
-- end point, so the lambda functions change the coordinates of this start and end point depending on which line 
-- is being drawn.
grid :: Picture
grid = pictures $ map (\n -> line [(n * cellDimension, 0.0), (n * cellDimension, fromIntegral screenHeight)]) [1..6]
                  ++ map (\n -> line [(0.0, n * cellDimension), (fromIntegral screenWidth, n * cellDimension)]) [1..5]

-- Left, right and bottom border rendered in a darker color
gridOutline :: Picture
gridOutline = pictures $ map (\n -> line [(n * cellDimension, 0.0), (n * cellDimension, fromIntegral screenHeight)]) [0, 7]
                ++ [line [(0.0, 0.0), (fromIntegral screenWidth, 0.0)]]



-- GAME LOGIC
-- =============================================================================================================================



-- Convert mouse position into grid coordinates. (0,0) is center of board so adding half the board
-- heigh/width and then dividing by the square dimension gives the mouse coordinates recalibrated 
-- to be between 0-6 (6 exclusive) and 0-7 (7 exclusive) vertically and horizontally. To find the 
-- corresponding square coordinates the value is floored.
positionAsCellLocation :: (Float, Float) -> (Int, Int)
positionAsCellLocation (x, y) = (floor ((x + (fromIntegral screenWidth * 0.5)) / cellDimension),
                                 floor ((y + (fromIntegral screenHeight * 0.5)) / cellDimension))


-- Update the current game when making a move. Takes the current game and coordinates of proposed move.
-- If the move is valid, the current board is updated to include the move that was just made - the square
-- at the given coordinates is changed from Nothing to Just the player that moved there. Then, this updated
-- board is checked to see if the move just made has caused a win to occur. If the move is not valid, 
-- nothing is updated and the game remains unchanged
makeMove :: Game -> (Int, Int) -> Game
makeMove game (x,y) =   if validMove (board game) (x, y) then
                            -- Get the row of the board where the update will happen and update it at the x coordinate
                            -- It is inserted back into the main board at the y coordinate at the end of the function
                            let queryRow = fromMaybe empty (lookup y (board game))
                                newRow = update x (Just (player game)) queryRow
                            -- Once the move is made, update board to reflect it and switch the current player
                            in checkForWinner (x, y) (player game) $ game {board = update y newRow (board game), player = case player game of
                                                                                   Red -> Blue
                                                                                   Blue -> Red}
                        else
                            game


-- Check if a proposed move is a possible move to make. Takes the current board and coordinates of proposed move. 
-- Proposed moves should be in an available square, and should be supported from underneath. If not, the function returns false.
-- If valid, true is returned.
validMove :: Board -> (Int, Int) -> Bool
validMove board (x, y) = let row = fromMaybe empty (lookup y board)
                             cell = fromMaybe Nothing (lookup x row) -- Square value found from coordiantes
                                  -- Determines whether square is free
                                  in case cell of
                                        -- If not, move must be on bottom row or above another move
                                        Nothing ->  if y - 1 == -1 then 
                                                        True
                                                    else
                                                        -- If not bottom row, the square on the same column but row below
                                                        -- must have a move there for the proposed move to be valid
                                                        let row' = fromMaybe empty (lookup (y-1) board)
                                                            cell' = fromMaybe Nothing (lookup x row')
                                                        in case cell' of
                                                            Nothing -> False
                                                            _ -> True
                                        _ -> False

-- Check the row, column and diagonals associated with the move that was just made to see if 4 have been connected.
-- Takes the grid coordinates of the move just made, the current player, and the current game. Firstly, 4 lists are built
-- to correspond to the row, column and diagonals that pass through the given point. These are lists, not sequences, because
-- list has a function which naturally lends itself to this problem - isInfixOf. This checks whether one list is entirely
-- contained, in it's order, within the second list. We can see if four consecutive moves are contained in any of the 
-- four directions, and if it is true then it means the player has won the game. 
checkForWinner :: (Int, Int) -> Player -> Game ->  Game
checkForWinner (x, y) player game = let row = fromMaybe empty $ lookup y (board game)
                                        -- The row where the move was played. Found by getting the subsequence corresponding
                                        -- to the y value of the given point
                                        listRow = toList row

                                        -- The column where the move was played. Found by getting the given x value from every
                                        -- row in the board, and a list comprehension is used to do this concisely and leave it
                                        -- in a list format as required 
                                        listColumn = [fromMaybe Nothing $ lookup x $ fromMaybe empty $ lookup n (board game) | n <- [0..5]]

                                        -- The positive diagonal. Found by using parallel list comprehensions. We take the current square, and then 
                                        -- move one up and one to the right at the same time. This continues until a border is reached. The P1 list
                                        -- contains every square on the diagonal including and to the right of the given coordinates. P2 is similar but
                                        -- has every square on the diagonal to the left of the current one.
                                        -- To get this in an order which can be used to check for a winner, the left half is reversed and then the right half
                                        -- is appended to it.
                                        rightDiagP1 = [fromMaybe Nothing $ lookup a $ fromMaybe empty $ lookup b (board game) | a <- [x..6] | b <- [y..5]]
                                        rightDiagP2 = [fromMaybe Nothing $ lookup a $ fromMaybe empty $ lookup b (board game) | a <- [(x-1),(x-2)..0] | b <- [(y-1),(y-2)..0]]
                                        rightDiag = reverse rightDiagP2 ++ rightDiagP1

                                        -- The negative diagonal. Found using an identical method to the positive diagonal
                                        leftDiagP1 = [fromMaybe Nothing $ lookup a $ fromMaybe empty $ lookup b (board game) | a <- [x,(x-1)..0] | b <- [y..5]]
                                        leftDiagP2 = [fromMaybe Nothing $ lookup a $ fromMaybe empty $ lookup b (board game) | a <- [(x+1)..6] | b <- [(y-1),(y-2)..0]]
                                        leftDiag = reverse leftDiagP2 ++ leftDiagP1

                                        thePlayer = player
                                        theScore = scores game
                                    
                                    -- Check if 4 of the current player's moves exist in any of the 4 directions found above. If so, game ends and the scores
                                    -- are updated
                                    in if or [Prelude.replicate 4 (Just thePlayer) `isInfixOf` a | a <- [listRow, listColumn, rightDiag, leftDiag]]
                                        then
                                            game {state = End, scores = case thePlayer of
                                                                            Red -> (\(a,b) -> (a+1,b)) theScore
                                                                            Blue -> (\(a,b) -> (a,b+1)) theScore}
                                        else
                                            game
