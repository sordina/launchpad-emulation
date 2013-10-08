{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

import Prelude hiding (span, div)
import Control.Concurrent
import Text.InterpolatedString.Perl6 (q)
import Graphics.UI.Threepenny hiding (coords, map)
import Control.Monad.IO.Class

import Common

css :: String
css = [q|
  body {
    margin: 0;
    padding: 0;
  }
  h1 {
    text-align: center;
  }
  .bar {
    width: 880px;
    height: 880px;
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
  chanIn  <- newChan
  chanOut <- newChan
  forkIO $ getChanContents chanOut >>= writeList2Chan chanIn
  startGUI defaultConfig (setup chanIn chanOut)

-- Library Entry Point
runGUI :: Chan Command -> Chan Command -> IO ()
runGUI chanIn chanOut = startGUI defaultConfig (setup chanIn chanOut)

titleText :: String
titleText = "Launchpad Emulation"

setup :: Chan Command -> Chan Command -> Window -> IO ()
setup chanIn chanOut window = do
    set title titleText (return window)
    addCSS $ getHead window
    addH1  $ getBody window
    cells <- addPad chanOut $ getBody window
    forkIO $ getChanContents chanIn >>= mapM_ (updateCell cells)
    return ()

addCSS :: IO Element -> IO Element
addCSS = append [ mkElement "style" # set text css # set (attr "type") "text/css" ]

addH1 :: IO Element -> IO Element
addH1 = append [ h1 #+ [ string titleText ] ]

addPad :: Chan Command -> IO Element -> IO [(Coord, Element)]
addPad chanOut c = do
  cells <- mapM (cell chanOut) coords
  c #+ [ div #. "bar" #+ (map return cells) ]
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

updateCell :: (Eq a, MonadIO m) => [(a, Element)] -> (a, Bool) -> m ()
updateCell cells (coord,b) = case lookup coord cells
  of Just c -> flick b (return c) >> return ()
     _      -> return ()
