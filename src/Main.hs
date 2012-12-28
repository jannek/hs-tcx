{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime, formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Locale (defaultTimeLocale)
import System.Environment

data Activity = Activity [Lap]
  deriving (Show)

data Lap = Lap {
    lapDistance :: Float
  , lapTrackpoints :: [Trackpoint]
  } deriving (Eq, Show)

data Trackpoint = Trackpoint {
    tpTime :: Integer
  , tpBpm :: Integer
  } deriving (Eq, Show)

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

-- Note: the hardcoded .000 part is kludge but for my inputs this was
-- an easy way to get timestamps to parse.
readt :: String -> UTCTime
readt = readTime defaultTimeLocale "%FT%T.000%Z"

timeToInteger :: UTCTime -> Integer
timeToInteger = (fromInteger . round . utcTimeToPOSIXSeconds)

getTrackpoint :: ArrowXml a => a XmlTree Trackpoint
getTrackpoint = atTag "Trackpoint" >>>
  proc x -> do
    time <- text <<< atTag "Time" -< x
    bpm <- text <<< atTag "Value" <<< atTag "HeartRateBpm" -< x
    returnA -< Trackpoint (timeToInteger (readt time)) (read bpm)

getLap :: ArrowXml a => a XmlTree Lap
getLap = getChildren >>> isElem >>> hasName "Lap" >>>
  proc x -> do
    pts <- listA getTrackpoint <<< atTag "Track" -< x
    dist <- getChildren >>> isElem >>> hasName "DistanceMeters" >>> text -< x
    returnA -< Lap (read dist) pts

getActivity :: ArrowXml a => a XmlTree Activity
getActivity = atTag "Activity" >>>
  proc x -> do
    laps <- listA getLap -< x
    returnA -< Activity laps

getActivities :: ArrowXml a => a XmlTree [Activity]
getActivities = deep (isElem >>> hasName "TrainingCenterDatabase" /> hasName "Activities") >>>
  proc x -> do
    activities <- listA getActivity -< x
    returnA -< activities

testSegment :: Integer -> Integer -> (Trackpoint, Trackpoint) -> Bool
testSegment minbpm maxbpm (start, end) = testTrackpoint minbpm maxbpm start || testTrackpoint minbpm maxbpm end

testTrackpoint :: Integer -> Integer -> Trackpoint -> Bool
testTrackpoint minbpm maxbpm (Trackpoint time bpm) = (bpm>=minbpm && bpm<=maxbpm)

extractLapTrackpoints :: Activity -> [(Trackpoint, Trackpoint)]
extractLapTrackpoints (Activity laps) = concatMap extractTrackpoints laps

extractTrackpoints :: Lap -> [(Trackpoint, Trackpoint)]
extractTrackpoints (Lap distance trackpts) = zip trackpts (tail trackpts)

filterSegments :: Integer -> Integer -> [(Trackpoint, Trackpoint)] -> [(Trackpoint, Trackpoint)]
filterSegments minb maxb lst = filter (testSegment minb maxb) lst

timeInZone :: Integer -> Integer -> (Trackpoint, Trackpoint) -> Integer
timeInZone minb maxb (Trackpoint stime sbpm, Trackpoint etime ebpm)
  | sbpm < minb && ebpm > minb && ebpm <= maxb = round(fromIntegral(etime-stime)*(fromIntegral(ebpm-minb)/fromIntegral(abs(ebpm-sbpm))))
  | sbpm > maxb && ebpm < maxb && ebpm >= minb = round(fromIntegral(etime-stime)*(fromIntegral(maxb-ebpm)/fromIntegral(abs(ebpm-sbpm))))
  | ebpm < minb && sbpm > minb && sbpm <= maxb = round(fromIntegral(etime-stime)*(fromIntegral(sbpm-minb)/fromIntegral(abs(ebpm-sbpm))))
  | ebpm > maxb && sbpm < maxb && sbpm >= minb = round(fromIntegral(etime-stime)*(fromIntegral(maxb-sbpm)/fromIntegral(abs(ebpm-sbpm))))
  | sbpm < minb && ebpm > maxb = round(fromIntegral(etime-stime)*(fromIntegral((ebpm-maxb)+(minb-sbpm))/fromIntegral(abs(ebpm-sbpm))))
  | sbpm > maxb && ebpm < minb = round(fromIntegral(etime-stime)*(fromIntegral((sbpm-maxb)+(minb-ebpm))/fromIntegral(abs(ebpm-sbpm))))
  | sbpm >= minb && ebpm <= maxb = (etime-stime)
  | otherwise = 0

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("calc", calc)
           , ("dump", dump)
           ]

main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

calc :: [String] -> IO ()
calc [fileName, minb, maxb] = do
  let minBPM = read minb
  let maxBPM = read maxb
  activities <- runX (readDocument [withValidate no] fileName >>> getActivities)
  let trackpts = concatMap extractLapTrackpoints (head activities)
  let matchtrackpts = filterSegments minBPM maxBPM trackpts
  print (sum(map (timeInZone minBPM maxBPM) matchtrackpts))

dump :: [String] -> IO ()
dump [fileName, minb, maxb] = do
  let minBPM = read minb
  let maxBPM = read maxb
  activities <- runX (readDocument [withValidate no] fileName >>> getActivities)
  let trackpts = concatMap extractLapTrackpoints (head activities)
  let matchtrackpts = filterSegments minBPM maxBPM trackpts
  mapM_ (printStats minBPM maxBPM) matchtrackpts
  where
    printStats minBPM maxBPM (Trackpoint stime sbpm, Trackpoint etime ebpm) = do
      putStrLn (show stime ++ "," ++ show sbpm ++ " -> " ++ show etime ++ "," ++ show ebpm ++ " (" ++ show (timeInZone minBPM maxBPM (Trackpoint stime sbpm, Trackpoint etime ebpm)) ++ " seconds in zone)")
