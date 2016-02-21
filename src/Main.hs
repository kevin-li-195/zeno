import Graphics.X11.Xlib
import Control.Monad

display :: IO Display
display = openDisplay ""

hookKey :: IO KeyCode
hookKey = display >>= (\x -> keysymToKeycode x $ stringToKeysym "r")

getCursor :: Display -> Window -> IO (Int, Int)
getCursor d w = do
    (_ _ _ x y _ _ _) <- queryPointer d w
    return (fromIntegral x, fromIntegral y)

getBoundaries :: Screen -> IO (Integer, Integer)
getBoundaries s = do
    x <- widthOfScreen s
    y <- heightOfScreen s
    return (fromIntegral x, fromIntegral y)

search :: Display -> Screen -> Window -> IO ()
search d s w = do
    grabKeyboard d w False GrabModeAsync GrabModeAsync 
    -- Ungrab global hook key
    -- Grab keyboard
    -- Do search
    -- Grab global hook key

main :: IO ()
main = do
    dsp <- display
    let scr = defaultScreen dsp
    win <- rootWindow dsp scr
    kc <- hookKey

    grabKey dsp kc mod4Mask win False grabModeSync grabModeSync

    allocaXEvent $ \ptr -> do
        forever $ do
            nextEvent dsp ptr
            p <- get_EventType ptr
            case p of
                2 -> putStrLn "things happening"
                _ -> return ()
