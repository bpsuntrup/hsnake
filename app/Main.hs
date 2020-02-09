module Main where

import Lib --am I going to use Lib?????
import UI.NCurses
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random
import Lens.Micro
import Control.Monad.IO.Class
import Data.Foldable (mapM_)

--TODO: check if the terminal is too small. Display error message and exit.
--TODO: display instructions (use arrow keys. legend for what each item does. Don't eat yourself)
--TODO: title screen with menu
--TODO: tutorial mode
--TODO: more item types
--TODO: score
--TODO: levels
--TODO: magic!!!!! GLITTER!!! Fireworks!
--TODO: make this internet.
--TODO: add mice that run away from the snake
--TODO: save games and pause games.... no. You can't pause a game until you've made a checkpoint,,, increased a level or something

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    setCursorMode CursorInvisible
    updateWindow w $ do
        moveCursor 1 10
        drawString "Snake!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    menuLoop w -- ideally, I guess this would create an initial state object and pass it to playGame... could even have funcitonality for pausing and saving games
    playGame w

menuLoop :: Window -> Curses ()
menuLoop w = do
    ev <- getEvent w (Nothing)
    case ev of
        Nothing  -> menuLoop w
        Just ev' -> case ev' of 
            EventCharacter 'Q' -> render
            _                  -> render

--TODO: add color
generateDefaultBoundary :: (Integer, Integer) -> Set.Set (Integer, Integer)
generateDefaultBoundary (y, x) = top `Set.union` bottom `Set.union` left `Set.union` right
    where top    = Set.fromList $ map (\e -> (1,e)) [1..x-1]
          bottom = Set.fromList $ map (\e -> (y-1,e)) [1..x-1]
          left   = Set.fromList $ map (\e -> (e,1)) [1..y-1]
          right  = Set.fromList $ map (\e -> (e,x-1)) [1..y-1]

playGame :: Window -> Curses ()
playGame w = do
    --TODO: display game over message, maybe returning Curses (ExitScreen) or something
    screenSizeYX@(maxy, maxx) <- screenSize
    let mySnake = [SnakeSegment { coords=(4,10), colorId=Blue}]
    let myBoundary = generateDefaultBoundary screenSizeYX
    g <- liftIO getStdGen
    let (myItems, initRands) = (over _1 Set.fromList) (randItems (maxy * maxx `div` 100) screenSizeYX $ randoms g) -- This number 100 can be increased to decrease the amount of items. I quite like this number though
    gcmap <- getColorID
    --TODO: draw borders
    let loop st = do updateWindow w clear
                     updateWindow w (drawItems gcmap $ Set.toList $ items st)
                     updateWindow w (drawSnake gcmap $ currentSnake st)
                     updateWindow w $ drawBoundary gcmap $ st boundary
                     render
                     ev <- getEvent w (Just $ speed st)
                     case ev of
                         Nothing  -> loop $ evolve st $ movingDirection st
                         Just ev' -> case ev' of
                             EventCharacter 'q' -> render --add exit screen here
                             EventCharacter 'Q' -> render
                             EventSpecialKey KeyUpArrow    -> loop $ evolve st North
                             EventSpecialKey KeyLeftArrow  -> loop $ evolve st West
                             EventSpecialKey KeyDownArrow  -> loop $ evolve st South
                             EventSpecialKey KeyRightArrow -> loop $ evolve st East
                             _   -> loop $ evolve st $ movingDirection st
    loop GameState { currentSnake    = mySnake
                   , movingDirection = South
                   , score           = 0 --TODO: display this somehow
                   , items           = myItems
                   , speed           = 200
                   , gameover        = False
                   , boundary        = myBoundary
                   , rands           = initRands
                   , yxSize          = screenSizeYX
                   }

evolve :: GameState -> Direction -> GameState

evolve st d = case snakeStatus of 
    StatusBoundary   -> st { gameover = True }
    StatusCannibal   -> st { gameover = True }
    StatusEmptySnake -> st { gameover = True }
    StatusNone       -> newState
    StatusItem i     -> consumeNow st d i
    where newSnake = advanceSnake d (currentSnake st)
          consumeNow st d i = case itemType i of
              Food      -> newState { items = newItems i, currentSnake = growSnake (color i) d (currentSnake st) }
              Poison    -> newState { items = newItems i, currentSnake = popTail newSnake } -- advance snake, but pop tail off... 
              Booster   -> newState { items = newItems i, speed = max 1 ((speed st) - 10) }
              Retardant -> newState { items = newItems i, speed = (speed st) + 10 }
          snyx@(sn_y, sn_x) = coords $ head $ currentSnake st
          boundaryCollisions = filter (snyx==) $ Set.toList $ boundary st -- TODO, use a set filter function here
          snakeCollisions = filter (snyx==) $ map coords $ tail $ currentSnake st
          itemCollisions = filter (\x -> snyx == position x) $ Set.toList $ items st --- and here
          (newItem, newRands) = (randItem . yxSize) st $ rands st
          newState = st { currentSnake    = newSnake
                        , movingDirection = d
                        , rands           = newRands
                        }
          newItems i = Set.insert newItem (Set.delete i $ items st)
          --TODO: think harder about how to make the following prettier
          --snakeStatus
          --      | boundaryCollisions /= [] = StatusBoundary
          --      | snakeCollisions /= []    = StatusCannibal
          --      | itemCollisions /= []     = StatusNone
          --      | otherwise                = StatusItem $ head itemCollisions
          snakeStatus =
              if boundaryCollisions == []
              then if snakeCollisions == []
                   then if itemCollisions == []
                        then StatusNone
                        else StatusItem $ head itemCollisions
                   else StatusCannibal
              else StatusBoundary

--TODO: add a score, and score effects for each type of event
data GameState = GameState { currentSnake    :: Snake
                           , movingDirection :: Direction
                           , items           :: Set.Set Item
                           , speed           :: Integer
                           , gameover        :: Bool
                           , boundary        :: Set.Set (Integer, Integer)
                           , rands           :: [Integer]
                           , yxSize          :: (Integer, Integer)
                           , score           :: Integer
                           }

type Snake = [SnakeSegment]
data SnakeStatus = StatusBoundary | StatusCannibal | StatusNone | StatusItem Item | StatusEmptySnake
data SnakeSegment = SnakeSegment { coords  :: (Integer, Integer)
                                 , colorId :: GameColor
                                 }
data Direction = North | East | South | West
-- TODO: add potions for things like eating boundaries, eating yourself, changing color, clearing an item type, etc.
data ItemType = Food | Poison | Booster | Retardant deriving (Eq, Ord)
data GameColor = Red | Yellow | Green | Cyan | Blue | Magenta deriving (Eq, Ord)
--TODO: make items have a "shelf life" and expire after a certain amount of time
data Item = Item { itemType :: ItemType
                 , color    :: GameColor
                 , position :: (Integer, Integer)
                 } deriving (Eq, Ord)
type GameColorMap = GameColor -> ColorID
type Boundary = Set.Set (Integer, Integer)


-- | Takes (height, width) of terminal, infinite list of randoms, and returning a random Item. May someday take
-- object representing weights for fields of Item
-- TODO: the randomness should be handled in a monad
randItem :: (Integer, Integer) -> [Integer] -> (Item, [Integer])
randItem (y,x) (riit:ric:rx:ry:rs) = 
    (Item { itemType = randit
          , color    = randgc
          , position = (ry `mod` y, rx `mod` x)
          }
    , rs)
    where randit | riit `mod` 100 < 75 = Food
                 | riit `mod` 100 < 90 = Booster
                 | riit `mod` 100 < 95 = Retardant
                 | otherwise           = Poison
          randgc | ric `mod` 60 < 10 = Red
                 | ric `mod` 60 < 20 = Yellow
                 | ric `mod` 60 < 30 = Green
                 | ric `mod` 60 < 40 = Cyan
                 | ric `mod` 60 < 50 = Blue
                 | otherwise         = Magenta

randItems :: Integer              -- ^ number of items
          -> (Integer, Integer)   -- ^ screen size
          -> [Integer]            -- ^ infinite random list
          -> ([Item], [Integer])
randItems 0 yx rs = ([], rs)
randItems n yx rs = let (it, newrands) = randItem yx rs
                    in  over _1 (it:) (randItems (n-1) yx newrands)

--TDOO: this doesn't work
drawBoundary :: GameColorMap -> Boundary -> Update ()
drawBoundary gcm b = setColor c >>
                     mapM_ (\(y,x) -> moveCursor y x >> drawString " ") b
                     where c = gcm Green -- todo: make this a parameter

drawSnake :: GameColorMap -> Snake -> Update ()
drawSnake _ [] = return ()
drawSnake gcm (head:rest) = setColor color >>
                      moveCursor y x   >>
                      drawString " "   >>
                      drawSnake gcm rest
                      where (y,x) = coords head
                            color = gcm $ colorId head

drawItems :: (GameColor -> ColorID) -> [Item] -> Update ()
drawItems _ [] = return ()
drawItems gcm (i:is) = setColor col >>
                       windowSize >>= \(max_y, max_x) ->
                       moveCursor (mod y max_y) (mod x max_x) >>
                       drawString itemString >>
                       drawItems gcm is
                       where (y,x) = position i
                             col = gcm $ (color i)
                             itemString = case (itemType i) of
                                               Food      -> "*"
                                               Poison    -> "x"
                                               Booster   -> "^"
                                               Retardant -> "v"
                      
getColorID :: Curses (GameColor -> ColorID)
getColorID = do
    redID    <- newColorID ColorWhite ColorRed  21
    yellowID <- newColorID ColorBlack ColorYellow 22
    greenID  <- newColorID ColorBlack ColorGreen 23
    blueID   <- newColorID ColorWhite ColorBlue 24
    magentaID <- newColorID ColorWhite ColorMagenta 25
    cyanID <- newColorID ColorBlack ColorCyan 26
    let map = \gc -> case gc of
                        Red     -> redID
                        Yellow  -> yellowID
                        Green   -> greenID
                        Blue    -> blueID
                        Cyan    -> cyanID
                        Magenta -> magentaID
    return map

advanceSnake :: Direction -> Snake -> Snake
advanceSnake d snake@(head:tail) = case d of
    North ->  newSnake (y-1) x
    East  ->  newSnake y (x+1)
    South ->  newSnake (y+1) x
    West  ->  newSnake y (x-1)
    where (y,x) = coords head
          color = colorId head
          newSnake z w = (SnakeSegment { coords=(z,w), colorId=color}):(popTail $ shiftColors snake)

--Shifts colors forward in snake from tail to head, tail remaining its original color, and 
-- the head's color "shifting" off the front
shiftColors :: Snake -> Snake
shiftColors [] = []
shiftColors (h:[]) = h:[]
shiftColors (h:hs) = SnakeSegment { coords=(coords h), colorId=nextColor } : (shiftColors hs)
    where nextColor = colorId (head hs)

growSnake :: GameColor -> Direction -> Snake -> Snake
growSnake c d s@(head:tail) = (SnakeSegment {coords=newHeadCoords, colorId=c}):s
    where (y,x) = coords head
          newHeadCoords = case d of
              North -> (y-1,x)
              East  -> (y,x+1)
              South -> (y+1,x)
              West  -> (y,x-1)

--TODO: use init instead
popTail :: [a] -> [a]
popTail [] = []
popTail (x:xs) = case xs of
    [] -> []
    _  -> x:(popTail xs)


