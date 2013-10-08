import System.MIDI
import System.MIDI.Utility
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.Tuple
import Data.Map hiding (filter, map)
import Prelude hiding (lookup, null)

import Common

main :: IO ()
main = do
  launchpadOut         <- selectOutputDevice (Just "Launchpad")
  audioOut             <- selectOutputDevice (Just "Bus 1")
  launchpadIn          <- selectInputDevice  (Just "Launchpad")

  launchpadSource      <- openSource      launchpadIn  Nothing
  launchpadDestination <- openDestination launchpadOut
  audioDestination     <- openDestination audioOut

  start launchpadSource

  state <- newIORef empty

  chanA <- newChan
  chanB <- newChan
  chanC <- mergeChans [chanA, chanB]
  chanD <- dupChan chanC
  chanE <- dupChan chanD
  chanF <- dupChan chanE

  mapM_ forkIO [ processA chanA launchpadSource
               , processB chanB state
               , processC chanC launchpadDestination
               , processD chanD state
               , processE chanE audioDestination
               , processF chanF
               ]

  putStrLn "Hit ENTER to quit..."
  void  getLine

  stop  launchpadSource
  close launchpadSource
  close launchpadDestination
  close audioDestination

processA :: Chan MidiEvent -> Connection -> IO ()
processA c s = do
  es <- getEvents s
  writeList2Chan c (filter isOnEvent es)
  threadDelay oneSplitSecond
  processA c s

processB :: Chan MidiEvent -> IORef State -> IO ()
processB chanB state = do
  previousState <- readIORef state
  mapM_ (writeChan chanB) (coords >>= processB_cell previousState)
  threadDelay lifeDelay
  processB chanB state

processB_cell :: State -> Coord -> [MidiEvent]
processB_cell previousState coord@(x,y) = if nextState == b then [] else [encodeEvent coord nextState]
  where
  nextState     = rules b s
  b             = fromMaybe False $ lookup coord previousState
  s             = sum $ map (b2i . fromMaybe False . flip lookup previousState) neighbourhood
  neighbourhood = [(mod (x+dx) limx , mod (y+dy) limy) | dx <- [-1..1], dy <- [-1..1]]

processC :: Chan MidiEvent -> Connection -> IO ()
processC chanC launchpadDestination = mapChan chanC (send launchpadDestination . getMessage)

processD :: Chan MidiEvent -> IORef State -> IO ()
processD chanD state = mapChan chanD (updateFromMessage . getMessage)
  where
  updateFromMessage m = modifyIORef state (insert `uncurry` decodeMessage m)

processE :: Chan MidiEvent -> Connection -> IO ()
processE chanE audioDestination = mapChan chanE (send audioDestination . audify . getMessage)

processF :: Chan MidiEvent -> IO ()
processF chanF = mapChan chanF print

-- Helpers

getMessage :: MidiEvent -> MidiMessage
getMessage (MidiEvent _ m) = m

encodeEvent :: Coord -> Bool -> MidiEvent
encodeEvent (x,y) True  = MidiEvent 0 $ MidiMessage 1 (NoteOn  (num x y) 127)
encodeEvent (x,y) False = MidiEvent 0 $ MidiMessage 1 (NoteOff (num x y) 64 )

isOnEvent :: MidiEvent -> Bool
isOnEvent (MidiEvent _ (MidiMessage _ (NoteOn _ _))) = True
isOnEvent _                                          = False

num :: Int -> Int -> Int
num x y = 16 * y + x

pos :: Int -> (Int, Int)
pos n = swap $ divMod n 16

decodeMessage :: MidiMessage -> (Coord,Bool)
decodeMessage (MidiMessage _ (NoteOn  n _)) = (pos n, True)
decodeMessage (MidiMessage _ (NoteOff n _)) = (pos n, False)
decodeMessage _                             = ((0,0), False)

oneSplitSecond :: Int
oneSplitSecond = 10000

lifeDelay :: Int
lifeDelay = 1000000

-- Life
--
rules :: Bool -> Int -> Bool
rules True  3 = True
rules True  4 = True
rules False 3 = True
rules _     _ = False

b2i :: Bool -> Int
b2i True = 1
b2i _    = 0

-- Chans
--
mergeChans :: [Chan x] -> IO (Chan x)
mergeChans l = do
  o <- newChan
  mapM_ (forkIO . (getChanContents >=> writeList2Chan o)) l
  return o

mapChan :: Chan a -> (a -> IO b) -> IO ()
mapChan chan f = getChanContents chan >>= mapM_ f

audify :: MidiMessage -> MidiMessage
audify m = message
  where
  i         = 21 + x * 2 + y * 5
  ((x,y),b) = decodeMessage m
  message   = case b of True  -> MidiMessage 1 (NoteOn  i 127)
                        False -> MidiMessage 1 (NoteOff i 127)
