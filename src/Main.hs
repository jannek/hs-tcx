{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime, formatTime)
import System.Locale (defaultTimeLocale)

data Activity = Activity [Lap]
  deriving (Show)

data Lap = Lap {
    lapDistance :: Float
  , lapTrackpoints :: [Trackpoint]
  } deriving (Eq, Show)

data Trackpoint = Trackpoint {
    tpTime :: UTCTime
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

showt :: UTCTime -> String
showt = formatTime defaultTimeLocale "%s"

getTrackpoint :: ArrowXml a => a XmlTree Trackpoint
getTrackpoint = atTag "Trackpoint" >>>
  proc x -> do
    time <- text <<< atTag "Time" -< x
    bpm <- text <<< atTag "Value" <<< atTag "HeartRateBpm" -< x
    returnA -< Trackpoint (readt time) (read bpm)

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

main :: IO ()
main = do
  activities <- runX (readDocument [withValidate no] "test-act.tcx" >>> getActivities)
  mapM_ printActivity (head activities)
  where
    printActivity (Activity laps) = do
      mapM_ printLaps laps

    printLaps (Lap distance trackpts) = do
      mapM_ printTrackseg (zip trackpts (tail trackpts))

    printTrackseg ((Trackpoint stime sbpm, Trackpoint etime ebpm)) = do
      putStrLn (showt stime ++ "," ++ show sbpm ++ " -> " ++ showt etime ++ "," ++show ebpm)