import Control.Applicative
import Control.Monad
import Data.Time.Clock.POSIX
import Graphics.X11.Xlib
import qualified Graphics.X11.XTest as XT

data Direction = DUp | DDown | DLeft | DRight
type Boundary = Integer

getTime :: IO Time
getTime = floor <$> getPOSIXTime

display :: IO Display
display = openDisplay ""

hookKey :: IO KeyCode
hookKey = display >>= (\x -> keysymToKeycode x $ stringToKeysym "r")

getCursor :: Display -> Window -> IO (Integer, Integer)
getCursor d w = do
    (_,  _,  _, x, y, _, _, _) <- queryPointer d w
    return (fromIntegral x, fromIntegral y)

getBoundaries :: Screen -> (Boundary, Boundary)
getBoundaries s =
    let x = widthOfScreen s
        y = heightOfScreen s
    in (fromIntegral x, fromIntegral y)

-- Grabs keyboard upon invokation, gets max screen bounds, and passes to
-- searchHelper
search :: Display -> Window -> Screen -> XEventPtr -> IO ()
search d w s ptr = do
    t <- getTime
    grabKeyboard d w True grabModeSync grabModeSync t
    let (maxX, maxY) = getBoundaries s
    searchHelper d w s 0 maxX 0 maxY ptr
    newT <- getTime
    ungrabKeyboard d newT

-- Wrapper for warpPointer.
movePointer
  :: Display
  -> Window
  -> Direction
  -> Integer
  -> Integer
  -> Boundary
  -> Boundary
  -> Boundary
  -> Boundary
  -> IO ()
movePointer d w dir curX curY leftBound rightBound topBound botBound = do
    let warp = warpPointer d 0 0 0 0 0 0
    case dir of
        DUp ->
            warp 0 $ (fromIntegral topBound - fromIntegral curY) `div` 2
        DDown ->
            warp 0 $ (fromIntegral botBound - fromIntegral curY) `div` 2
        DLeft ->
            warp ((fromIntegral leftBound - fromIntegral curX) `div` 2) 0
        DRight ->
            warp ((fromIntegral rightBound - fromIntegral curX) `div` 2) 0

-- Called recursively on receiving input after keyboard is grabbed
-- to determine action (direction, click, reset, or exit)
searchHelper
  :: Display
  -> Window
  -> Screen
  -> Boundary
  -> Boundary
  -> Boundary
  -> Boundary
  -> XEventPtr
  -> IO ()
searchHelper d w s leftBound rightBound topBound botBound ptr = do
    nextEvent d ptr
    (_, _, _, badCurX, badCurY, _, _, _, _, _) <- get_KeyEvent ptr
    evType <- get_EventType ptr
    let curX = fromIntegral badCurX
        curY = fromIntegral badCurY
    (a, str) <- lookupString $ asKeyEvent ptr
    let move dir
            = movePointer
              d w dir curX curY
              leftBound rightBound topBound botBound
    let rec lb rb bb tp = searchHelper d w s lb rb bb tp ptr
    let recAsIs = searchHelper d w s leftBound rightBound topBound botBound ptr
    let (maxX, maxY) = getBoundaries s
    if evType == 2
        then case a of
            Just _ ->
                case str of
                    "q" -> pure ()
                    "l" -> do
                        move DRight
                        rec
                            ((rightBound + leftBound) `div` 2)
                            rightBound
                            topBound
                            botBound
                    "h" -> do
                        move DLeft
                        rec
                            leftBound
                            ((rightBound + leftBound) `div` 2)
                            topBound
                            botBound
                    "j" -> do
                        move DDown
                        rec
                            leftBound
                            rightBound
                            ((botBound + topBound) `div` 2)
                            botBound
                    "k" -> do
                        move DUp
                        rec
                            leftBound
                            rightBound
                            topBound
                            ((botBound + topBound) `div` 2)
                    "e" -> do
                        warpPointer d 0 w 0 0 0 0
                            (fromIntegral maxX `div` 2)
                            (fromIntegral maxY `div` 2)
                        searchHelper d w s 0 maxX 0 maxY ptr
                    "\r" -> XT.fakeButtonPress d 1 *> recAsIs
                    "n" -> XT.fakeButtonPress d 1 *> recAsIs
                    "m" -> XT.fakeButtonPress d 3 *> recAsIs
                    _ -> recAsIs
            Nothing -> recAsIs
        else recAsIs

main :: IO ()
main = do
    dsp <- display
    let screenNum = defaultScreen dsp
        scr = screenOfDisplay dsp screenNum
    win <- rootWindow dsp screenNum
    kc <- hookKey

    grabKey dsp 133 0 win False grabModeAsync grabModeAsync

    allocaXEvent $ \ptr -> do
        forever $ do
            nextEvent dsp ptr
            p <- get_EventType ptr
            case p of
                2 -> do
                    search dsp win scr ptr
                _ -> return ()
