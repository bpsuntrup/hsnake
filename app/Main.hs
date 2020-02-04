module Main where

import Lib
import UI.NCurses
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random

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

main :: IO ()
main = newStdGen >>= \g -> runCurses $ do
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
    playGame w $ randoms g

playGame :: Window -> [Integer] -> Curses ()
playGame w rs = do
    let mySnake = [SnakeSegment { coords=(4,10), colorId=Blue}]
    let myBoundary = [] -- TODO: make this match the window size or something, also create a function to draw the boundary
    let myItems = (Set.fromList [Item { itemType=Food
                                      , color=Red
                                      , position=(10,10) 
                                      }
                                ,Item { itemType = Food
                                      , color = Magenta
                                      , position = (10,20)
                                      }
                                ,Item { itemType = Food
                                      , color = Magenta
                                      , position = (10,30)
                                      }
                                ,Item { itemType = Food
                                      , color = Cyan
                                      , position = (10,40)
                                      }
                                ,Item { itemType = Food
                                      , color = Yellow
                                      , position = (10,50)
                                      }
                                ,Item { itemType = Food
                                      , color = Green
                                      , position = (10,60)
                                      }
                                ,Item { itemType = Poison
                                      , color = Green
                                      , position = (20,10)
                                      }
                                ,Item { itemType = Booster
                                      , color = Blue
                                      , position = (20,20)
                                      }
                                ])
    gcmap <- gameColorMap
    --TODO: draw borders
    --TODO: spawn and remove items here too, or maybe in the evolve function
    let loop st = updateWindow w clear >>
                  updateWindow w (drawItems gcmap $ Set.toList $ items st) >>
                  updateWindow w (drawSnake gcmap $ currentSnake st) >>
                  render >>
                  getEvent w (Just $ speed st) >>= \ev ->
                  case ev of
                      Nothing  -> loop $ evolve st $ movingDirection st
                      Just ev' -> case ev' of
                          EventCharacter 'q' -> render
                          EventCharacter 'Q' -> render
                          EventSpecialKey KeyUpArrow    -> loop $ evolve st North
                          EventSpecialKey KeyLeftArrow  -> loop $ evolve st West
                          EventSpecialKey KeyDownArrow  -> loop $ evolve st South
                          EventSpecialKey KeyRightArrow -> loop $ evolve st East
                          _   -> loop $ evolve st $ movingDirection st
    loop GameState { currentSnake    = mySnake
                   , movingDirection = South
                   , someInt         = 0
                   , items           = myItems
                   , speed           = 200
                   , gameover        = False
                   , boundary        = myBoundary
                   , rands           = rs
                   }

evolve :: GameState -> Direction -> GameState

-- TODO: maybe this is where items should be spawned and removed as well, rather than in the main loop. 
evolve st d = case snakeStatus st of 
    StatusBoundary   -> st { gameover = True }
    StatusCannibal   -> st { gameover = True }
    StatusEmptySnake -> st { gameover = True }
    StatusNone       -> st { currentSnake = newSnake 
                           , movingDirection = d }
    StatusItem i     -> consumeNow st d i
    where newSnake = advanceSnake d (currentSnake st)
          --TODO: make this update items with new random positions
          consumeNow st d i = case itemType i of
              Food      -> st { currentSnake = growSnake (color i) d (currentSnake st)
                              , movingDirection = d
                              , items = Set.insert (i { position = (mod (rands st !! 0) 50, mod (rands st !! 1) 50)}) (Set.delete i $ items st)  --whoops. I guess 50 by 50 is as close as I'm going to get since window size is trapped in Update.
                              , rands = tail $ tail $ rands st}
              Poison    -> st { currentSnake = popTail newSnake -- advance snake, but pop tail off... 
                              , movingDirection = d 
                              , items = Set.insert (i { position = (mod (rands st !! 0) 50, mod (rands st !! 1) 50)}) (Set.delete i $ items st)  --whoops. I guess 50 by 50 is as close as I'm going to get since window size is trapped in Update.
                              , rands = tail $ tail $ rands st}
              Booster   -> st { speed = max 1 ((speed st) - 10)
                              , currentSnake = newSnake
                              , movingDirection = d 
                              , items = Set.insert (i { position = (mod (rands st !! 0) 50, mod (rands st !! 1) 50)}) (Set.delete i $ items st)  --whoops. I guess 50 by 50 is as close as I'm going to get since window size is trapped in Update.
                              , rands = tail $ tail $ rands st}
              Retardant -> st { speed = (speed st) + 10
                              , currentSnake = newSnake
                              , movingDirection = d 
                              , items = Set.insert (i { position = (mod (rands st !! 0) 50, mod (rands st !! 1) 50)}) (Set.delete i $ items st)  --whoops. I guess 50 by 50 is as close as I'm going to get since window size is trapped in Update.
                              , rands = tail $ tail $ rands st}
          snyx@(sn_y, sn_x) = coords $ head $ currentSnake st
          boundaryCollisions = filter (snyx==) $ boundary st
          snakeCollisions = filter (snyx==) $ map coords $ tail $ currentSnake st
          itemCollisions = filter (\x -> snyx == position x) $ Set.toList $ items st
          snakeStatus st =
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
                           , someInt         :: Int
                           , items           :: Set.Set Item
                           , speed           :: Integer
                           , gameover        :: Bool
                           , boundary        :: [(Integer, Integer)]
                           , rands           :: [Integer]
                           }

type Snake = [SnakeSegment]
data SnakeStatus = StatusBoundary | StatusCannibal | StatusNone | StatusItem Item | StatusEmptySnake
data SnakeSegment = SnakeSegment { coords  :: (Integer, Integer)
                                 , colorId :: GameColor
                                 }
data Direction = North | East | South | West
-- TODO: add potions for things like eating boundaries, eating yourself, changing color, clearing an item type, etc.
data ItemType = Food | Poison | Booster | Retardant deriving (Eq, Ord)
data GameColor = Red | Orange | Yellow | Green | Cyan | Blue | Violet | Magenta deriving (Eq, Ord)
--TODO: make items have a "shelf life" and expire after a certain amount of time
data Item = Item { itemType :: ItemType
                 , color    :: GameColor
                 , position :: (Integer, Integer)
                 } deriving (Eq, Ord)
type Boundary = [(Integer, Integer)]

drawSnake :: Map.Map GameColor ColorID -> Snake -> Update ()
drawSnake _ [] = return ()
drawSnake gcm (head:rest) = setColor color >>
                      moveCursor y x   >>
                      drawString " "   >>
                      drawSnake gcm rest
                      where (y,x) = coords head
                            color = gcm Map.! colorId head

drawItems :: Map.Map GameColor ColorID -> [Item] -> Update ()
drawItems _ [] = return ()
drawItems gcm (i:is) = setColor col >>
                       windowSize >>= \(max_y, max_x) ->
                       moveCursor (mod y max_y) (mod x max_x) >>
                       drawString itemString >>
                       drawItems gcm is
                       where (y,x) = position i
                             col = gcm Map.! (color i)
                             itemString = case (itemType i) of
                                               Food      -> "*"
                                               Poison    -> "x"
                                               Booster   -> "^"
                                               Retardant -> "v"
                      
gameColorMap :: Curses (Map.Map GameColor ColorID)
gameColorMap = do
    redID    <- newColorID ColorWhite ColorRed  21
    yellowID <- newColorID ColorBlack ColorYellow 22
    greenID  <- newColorID ColorBlack ColorGreen 23
    blueID   <- newColorID ColorWhite ColorBlue 24
    magentaID <- newColorID ColorWhite ColorMagenta 25
    cyanID <- newColorID ColorBlack ColorCyan 26
    let myMap = [ (Red, redID),
                  (Yellow, yellowID),
                  (Green, greenID),
                  (Blue, blueID),
                  (Cyan, cyanID),
                  (Magenta, magentaID) ]
    return $ Map.fromList myMap


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

popTail :: [a] -> [a]
popTail [] = []
popTail (x:xs) = case xs of
    [] -> []
    _  -> x:(popTail xs)


