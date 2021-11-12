module Config ( frameRate
              , barColor
              , backgroundColor
              , tomatoWorkIntervalInSeconds
              , windowTitle
              , windowSize
              , windowPosition
              , halfMonitorWidthInPixels
              , halfBarHeightInPixels ) where

import Graphics.Gloss.Data.Color (Color, makeColor)

frameRate :: Int
frameRate = 2

barColor :: Color
barColor = orange

backgroundColor :: Color
backgroundColor = green

orange :: Color
orange = makeColor 0.96 0.40 0 noTransparency

green :: Color
green = makeColor 0.0 0.6 0.0 noTransparency -- green

noTransparency :: Float
noTransparency = 1

tomatoWorkIntervalInMinutes :: Float
tomatoWorkIntervalInMinutes = 25

tomatoWorkIntervalInSeconds :: Float
tomatoWorkIntervalInSeconds = tomatoWorkIntervalInMinutes * 60

windowTitle :: String
windowTitle = "tomato"

windowSize :: (Int, Int)
windowSize = (1912, 5)

windowPosition :: (Int, Int)
windowPosition = (0, 0)

halfBarHeightInPixels :: Float
halfBarHeightInPixels = 3

halfMonitorWidthInPixels :: Float
halfMonitorWidthInPixels = 960