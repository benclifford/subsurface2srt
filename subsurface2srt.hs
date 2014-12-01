{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | reads in a subsurface file on stdin and outputs
-- an srt subtitles file with dive computer readings
-- in the subtitle text.

import Prelude hiding (id)

import Control.Category (id)
import Control.Monad (forM_)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Text.ParserCombinators.Parsec
import Text.XML.HXT.Core

main = do
  progress "starting"
  [src, sOff] <- getArgs
  samples <- runX
       $ readDocument [withValidate no
                      ] src
     >>> selectSamples
     >>> sampleToTuple
 
  let parsedSamples = map parseSample samples
  let numberedSamples = parsedSamples `zip` [1..]
  let secondsOffset = fromRight $ parseTime sOff
  forM_ numberedSamples (printSample secondsOffset)
  return ()

selectSamples = deep isSample

sampleToTuple = proc sample -> do
  time <- selectTime -< sample
  depth <- selectDepth -< sample
  returnA -< (time, depth) 


isSample = isElem >>> hasName "sample"

selectTime = getAttrValue "time"
selectDepth = getAttrValue "depth"

progress = hPutStrLn stderr

parseSample (time, depth) = let
  timeSecs = fromRight $ parseTime time
  depthM = fromRight $ parseDepth depth
  in (timeSecs, depthM)

parseTime t = parseString t $ do
  mins <- many $ oneOf "0123456789"
  char ':'
  secs <- many $ oneOf "0123456789"
  string " min"
  return ((read mins) * 60 + (read secs))



parseDepth d = parseString d $ do
  metres <- many $ oneOf "0123456789."
  string " m"
  return ((read metres) :: Double)

parseString s p = parse p "(string)" s


fromRight (Right r) = r

printSample so ((time, depth),n) = do
  let startTime = formatTime (time + so)
  let endTime = formatEndTime (time + so + 19)
  print n
  putStr startTime
  putStr " --> "
  putStr endTime
  putStrLn ""
  putStrLn $ "Dive time: " ++ (formatHumanTime $ time) ++ " minutes"
  putStrLn $ "Depth: " ++ (show $ round depth) ++ " metres"
  putStrLn ""

formatTime (s :: Integer) = let
  seconds = s `rem` 60
  minutes = (s `quot` 60 ) `rem` 60
  hours = (s `quot` 60 ) `quot` 60
  in (twoDigit hours) ++ ":" ++ (twoDigit minutes) ++ ":" ++ (twoDigit seconds) ++ ",000"

formatEndTime (s :: Integer) = let
  seconds = s `rem` 60
  minutes = (s `quot` 60 ) `rem` 60
  hours = (s `quot` 60 ) `quot` 60
  in (twoDigit hours) ++ ":" ++ (twoDigit minutes) ++ ":" ++ (twoDigit seconds) ++ ",999"


formatHumanTime :: Integer -> String
formatHumanTime s = let
  minutes = s `quot` 60
  in (show minutes)

twoDigit :: Integer -> String
twoDigit n | n <= 9 = "0" ++ (show n)
twoDigit n = show n

