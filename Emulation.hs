{-# OPTIONS -fno-warn-unused-do-bind #-}

module Emulation where

import Prelude hiding (span, div)
import Control.Concurrent
import Graphics.UI.Threepenny hiding (coords, map)
import Control.Monad
import Data.IORef
import Common
import CSS

type Command = (Coord,Bool)

-- Self Proxying Test Entry Point
main :: IO ()
main = do
  chanIn    <- newChan
  chanOut   <- newChan
  pause     <- newIORef False
  initialFn <- newIORef "21 + x + 8 * y"
  forkIO $ getChanContents chanOut >>= writeList2Chan chanIn
  startGUI defaultConfig (setup initialFn pause chanIn chanOut)

-- Library Entry Point
runGUI :: IORef String -> IORef Bool -> Chan Command -> Chan Command -> IO ()
runGUI initialFn pause chanIn chanOut =
  startGUI defaultConfig (setup initialFn pause chanIn chanOut)

titleText, stopSymbol, playSymbol :: String
titleText  = "Launchpad Emulation"
stopSymbol = " ◼  "
playSymbol = " ▶ "

setup :: IORef String -> IORef Bool -> Chan Command -> Chan Command -> Window -> UI ()
setup initialFn pause chanIn chanOut window = do
    set title titleText (return window)
    addCSS $ getHead window
    addH1 $ getBody window
    addControls initialFn pause chanOut (getBody window)
    cells <- addPad chanOut $ getBody window
    void $ liftIO $ forkIO $ (getChanContents chanIn) >>= mapM_ (runUI window . updateCells cells)

addControls :: IORef String -> IORef Bool -> Chan Command -> UI Element -> UI ()
addControls audioFnText pause chanOut parent = do
  nextSymbol <- liftIO $ newIORef playSymbol
  box        <- h2 # set text stopSymbol
  initialFn  <- liftIO $ readIORef audioFnText
  myInput    <- input # set value initialFn
  parent #+ [ return box, return myInput ]

  on keyup myInput $ const $ do
    val <- get value myInput
    liftIO $ writeIORef audioFnText val

  on click box $ const $ do

    ns <- liftIO $ readIORef nextSymbol

    liftIO $
      if ns == stopSymbol
        then writeIORef nextSymbol playSymbol >> writeIORef pause False
        else writeIORef nextSymbol stopSymbol >> writeIORef pause True

    return box # set text ns -- TODO: Set this on a hook from chan events, rather than manually

    liftIO $ do
      writeChan chanOut ((8,4),True)
      putStrLn "Emulator: Pause button hit"

addCSS :: UI Element -> UI Element
addCSS = append [ mkElement "style" # set text css # set (attr "type") "text/css" ]

addH1 :: UI Element -> UI Element
addH1 = append [ h1 #+ [ string titleText ] ]

addPad :: Chan Command -> UI Element -> UI [(Coord, Element)]
addPad chanOut c = do
  cells <- mapM (cell chanOut) coords
  c #+ [ div #. "bar" #+ map return cells ]
  return (zip coords cells)

cell :: Chan Command -> Coord -> UI Element
cell chanOut coord = do
  item <- span
  on click item (const $ liftIO $ respond chanOut coord)
  return item

respond :: Chan Command -> Coord -> IO ()
respond chanOut coord = do
  putStrLn $ "Emulator item clicked: " ++ show coord
  writeChan chanOut (coord,True)

append :: [UI Element] -> UI Element -> UI Element
append = flip (#+)

-- In place cell interaction
--
turnOff :: UI Element -> UI Element
turnOff e = e # set (attr "class") "off"

turnOn :: UI Element -> UI Element
turnOn e = e # set (attr "class") "on"

flick :: Bool -> UI Element -> UI Element
flick True  = turnOn
flick False = turnOff

updateCells :: Eq a => [(a, Element)] -> (a, Bool) -> UI ()
updateCells cells (coord,b) = do
  case lookup coord cells
    of Just c -> void (flick b (return c))
       _      -> return ()
