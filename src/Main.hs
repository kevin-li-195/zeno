import Control.Monad
import Data.Time.Clock.POSIX
import Graphics.X11.Xlib

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

-- Grabs keyboard upon invokation, gets max screen bounds, and passes to searchHelper
search :: Display -> Window -> Screen -> XEventPtr -> IO ()
search d w s ptr = do
    t <- getTime
    grabKeyboard d w True grabModeSync grabModeSync t
    putStrLn "grabbed keyboard"
    let (maxX, maxY) = getBoundaries s
    searchHelper d w s 0 maxX 0 maxY ptr
    putStrLn "searchhelper terminated"
    newT <- getTime
    ungrabKeyboard d newT

-- Wrapper for warpPointer.
movePointer :: Display -> Window -> Direction -> Integer -> Integer -> Boundary -> Boundary -> Boundary -> Boundary -> IO ()
movePointer d w dir curX curY leftBound rightBound topBound botBound = do
    case dir of
        DUp -> warpPointer d 0 0 0 0 0 0 0 $ negate $ (fromIntegral curY - fromIntegral topBound) `div` 2
        DDown -> warpPointer d 0 0 0 0 0 0 0 $ (fromIntegral botBound - fromIntegral curY) `div` 2
        DLeft -> warpPointer d 0 0 0 0 0 0 (negate $ (fromIntegral curX - fromIntegral leftBound) `div` 2) 0
        DRight -> warpPointer d 0 0 0 0 0 0 ((fromIntegral rightBound - fromIntegral curX) `div` 2) 0

-- Called recursively on receiving input after keyboard is grabbed
-- to determine action (direction, click, reset, or exit)
searchHelper :: Display -> Window -> Screen -> Boundary -> Boundary -> Boundary -> Boundary -> XEventPtr -> IO ()
searchHelper d w s leftBound rightBound topBound botBound ptr = do
    print "waiting for key event"
    nextEvent d ptr
    print "finished waiting for key event"
    (_, _, _, badCurX, badCurY, _, _, _, _, _) <- get_KeyEvent ptr
    print "got key event"
    let curX = fromIntegral badCurX
        curY = fromIntegral badCurY
    (a, str) <- lookupString $ asKeyEvent ptr
    print str
    case a of
        Just a ->
            case str of
                "l" -> do
                    putStrLn "moving right"
                    movePointer d w DRight curX curY leftBound rightBound topBound botBound
                    searchHelper d w s (leftBound `div` 2) rightBound topBound botBound ptr
                "h" -> do 
                    putStrLn "moving left"
                    movePointer d w DLeft curX curY leftBound rightBound topBound botBound
                    searchHelper d w s leftBound (rightBound `div` 2) topBound botBound ptr
                "j" -> do
                    putStrLn "moving down"
                    movePointer d w DDown curX curY leftBound rightBound topBound botBound
                    searchHelper d w s leftBound rightBound (topBound `div` 2) botBound ptr
                "k" -> do
                    putStrLn "moving up"
                    movePointer d w DUp curX curY leftBound rightBound topBound botBound
                    searchHelper d w s leftBound rightBound topBound (botBound `div` 2) ptr
                "c" -> do
                    putStrLn "resetting"
                    let (maxX, maxY) = getBoundaries s
                    warpPointer d 0 w 0 0 0 0 (fromIntegral $ maxX `div` 2) (fromIntegral maxY `div` 2)
                    searchHelper d w s 0 maxX 0 topBound ptr
                "q" -> do
                    putStrLn "quitting"
                    return ()
                "\n" -> do
                    putStrLn "click"
                _ -> do
                    putStrLn "didn't get referenced key"
                    searchHelper d w s leftBound rightBound topBound botBound ptr
        Nothing -> do
            putStrLn "didn't get right symbol"
            searchHelper d w s leftBound rightBound topBound botBound ptr

main :: IO ()
main = do
    dsp <- display
    let screenNum = defaultScreen dsp
        scr = screenOfDisplay dsp screenNum
    win <- rootWindow dsp screenNum
    kc <- hookKey

    grabKey dsp kc mod4Mask win False grabModeAsync grabModeAsync

    allocaXEvent $ \ptr -> do
        forever $ do
            nextEvent dsp ptr
            p <- get_EventType ptr
            case p of
                2 -> do
                    print "got hook key" 
                    search dsp win scr ptr
                _ -> return ()
