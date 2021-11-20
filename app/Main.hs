module Main where

import Config
  ( backgroundColor,
    barColor,
    countdownLengthInSeconds,
    frameRate,
    halfBarHeightInPixels,
    halfMonitorWidthInPixels,
    windowPosition,
    windowSize,
    windowTitle,
  )
import Graphics.Gloss.Data.Picture (Picture (Pictures), color, polygon, scale)
import Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event, play)

main :: IO ()
main = play windowSpecs backgroundColor frameRate initialRunningTime render handleInputEvents updateRunningTimeOrExit

windowSpecs :: Display
windowSpecs = InWindow windowTitle windowSize windowPosition

type RunningTime = Float

type TimeDelta = Float

-- | render a bar ticking down towards the middle by defining the right half of the bar depending on
-- the running time, then mirroring it
render :: RunningTime -> Picture
render runningTime = Pictures [rightHalfBar, leftHalfBar]
  where
    rightHalfBar = color barColor $ fillingRectangle rightBoundaryXPosition
    leftHalfBar = scale flipYPosition leaveXPosition rightHalfBar
    rightBoundaryXPosition = max 0 (halfMonitorWidthInPixels - progressInPixels)
    progressInPixels = runningTime * pixelsPerSecond
    flipYPosition = -1
    leaveXPosition = 1

-- | draw a rectangle which
-- ... spans the entire y-dimenstion of the bar
-- ... has its left border at the center of the screen, which is the coordinate system origin as defined by the graphics package, gloss
-- ... has its width defined by the single parameter
fillingRectangle :: Float -> Picture
fillingRectangle width = polygon [upperLeftCorner, lowerLeftCorner, upperRightCorner, lowerRightCorner]
  where
    upperLeftCorner = (leftBorderXPosition, - halfBarHeightInPixels)
    lowerLeftCorner = (leftBorderXPosition, halfBarHeightInPixels)
    upperRightCorner = (rightBorderxPosition, halfBarHeightInPixels)
    lowerRightCorner = (rightBorderxPosition, - halfBarHeightInPixels)
    leftBorderXPosition = 0
    rightBorderxPosition = width

pixelsPerSecond :: Float
pixelsPerSecond = halfMonitorWidthInPixels / countdownLengthInSeconds

handleInputEvents :: Event -> RunningTime -> RunningTime
handleInputEvents _ runningTime = runningTime -- ignore all events

initialRunningTime :: Float
initialRunningTime = 0

updateRunningTimeOrExit :: RunningTime -> TimeDelta -> RunningTime
updateRunningTimeOrExit deltaTime runningTime
  | runningTime > countdownLengthInSeconds = error "not really an error, please yell at the dev to exit properly"
  | otherwise = runningTime + deltaTime
