{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Emulation where

import Prelude hiding (span, div)
import Control.Concurrent
import Text.InterpolatedString.Perl6 (q)
import Graphics.UI.Threepenny hiding (coords, map)
import Control.Monad.IO.Class
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

setup :: IORef String -> IORef Bool -> Chan Command -> Chan Command -> Window -> IO ()
setup initialFn pause chanIn chanOut window = do
    set title titleText (return window)
    addCSS $ getHead window
    addH1 $ getBody window
    addControls initialFn pause chanOut (getBody window)
    cells <- addPad chanOut $ getBody window
    forkIO $ getChanContents chanIn >>= mapM_ (updateCell cells)
    return ()

addControls :: IORef String -> IORef Bool -> Chan Command -> IO Element -> IO ()
addControls audioFnText pause chanOut parent = do
  let stopSymbol = " ◼  "
      playSymbol = " ▶ "

  nextSymbol <- newIORef playSymbol
  box        <- h2 # set text stopSymbol
  initialFn  <- readIORef audioFnText
  myInput    <- input # set value initialFn
  parent #+ [ return box, return myInput ]

  on keyup myInput $ const $ do
    val <- get value myInput
    writeIORef audioFnText val

  on click box $ const $ do
    ns <- readIORef nextSymbol
    if ns == stopSymbol
      then writeIORef nextSymbol playSymbol >> writeIORef pause False
      else writeIORef nextSymbol stopSymbol >> writeIORef pause True

    return box # set text ns -- TODO: Set this on a hook from chan events, rather than manually
    writeChan chanOut ((8,4),True)
    putStrLn "Emulator: Pause button hit"

addCSS :: IO Element -> IO Element
addCSS = append [ mkElement "style" # set text css # set (attr "type") "text/css" ]

addH1 :: IO Element -> IO Element
addH1 = append [ h1 #+ [ string titleText ] ]

addPad :: Chan Command -> IO Element -> IO [(Coord, Element)]
addPad chanOut c = do
  cells <- mapM (cell chanOut) coords
  c #+ [ div #. "bar" #+ map return cells ]
  return (zip coords cells)

cell :: Chan Command -> Coord -> IO Element
cell chanOut coord = do
  item <- span
  on click item (const $ respond chanOut coord)
  return item

respond :: Chan Command -> Coord -> IO ()
respond chanOut coord = do
  putStrLn $ "Emulator item clicked: " ++ show coord
  writeChan chanOut (coord,True)

append :: [IO Element] -> IO Element -> IO Element
append = flip (#+)

-- In place cell interaction
--
turnOff :: MonadIO m => m Element -> m Element
turnOff e = e # set (attr "class") "off"

turnOn :: MonadIO m => m Element -> m Element
turnOn e = e # set (attr "class") "on"

flick :: MonadIO m => Bool -> m Element -> m Element
flick True  = turnOn
flick False = turnOff

updateCell :: (Eq a, Functor f, MonadIO f) => [(a, Element)] -> (a, Bool) -> f ()
updateCell cells (coord,b) = case lookup coord cells
  of Just c -> void (flick b (return c))
     _      -> return ()
