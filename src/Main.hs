import Graphics.X11.Xlib
import Foreign.C.Types
import Control.Monad

main :: IO ()
main = do
    dsp <- openDisplay ""
    let scr = defaultScreen dsp
    win <- rootWindow dsp scr
    -- grabKey dsp (fromIntegral (fromEnum 'r')) button1Mask win False grabModeAsync grabModeAsync
    print button4Mask
    kc <- keysymToKeycode dsp $ stringToKeysym "r"
    grabKey dsp kc mod4Mask win False grabModeAsync grabModeAsync
    selectInput dsp win 1
    allocaXEvent $ \ptr -> do
        forever $ do
            putStrLn "things"
            nextEvent dsp ptr -- this is where things break
            putStrLn "2things"
            p <- get_EventType ptr
            case p of
                2 -> putStrLn "things happening"
                _ -> return ()
