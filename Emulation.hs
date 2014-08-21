{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Emulation where

import Prelude hiding (span, div)
import Control.Concurrent
import Text.InterpolatedString.Perl6 (q)
import Graphics.UI.Threepenny hiding (coords, map)
import Control.Monad
import Data.IORef

import Common

css :: String
css = [q|
  body {
    margin: 0;
    padding: 0;
  }
  h1, h2 {
    text-align: center;
  }
  h2 {
    height: 50px;
    font-size: 50px;
    cursor: pointer;
  }
  input {
    text-align: center;
    width: 300px;
    font-size: 200%;
    display: block;
    margin: 1em auto;
  }
  .bar {
    width: 900px;
    margin: 0 auto;
    padding: 10px 0 0 10px;
  }
  .bar span {
    display: inline-block;
    margin: 0 10px 10px 0;
    padding: 0px;
    width: 98px;
    height: 98px;
    border: 1px solid black;
    cursor: pointer;
  }
  .bar span.on {
    background: #FA0;
  }
|]

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

titleText :: String
titleText = "Launchpad Emulation"

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
  let stopSymbol = " ◼  "
      playSymbol = " ▶ "

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
