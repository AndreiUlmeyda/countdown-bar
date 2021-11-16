module Main where

import Config
  ( backgroundColor,
    barColor,
    frameRate,
    halfBarHeightInPixels,
    halfMonitorWidthInPixels,
    countdownLengthInSeconds,
    windowPosition,
    windowSize,
    windowTitle,
  )
import Graphics.Gloss.Data.Picture (Picture, color, polygon)
import Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event, play)

main :: IO ()
main = play windowSpecs backgroundColor frameRate initialRunningTime render handleInputEvents updateRunningTimeOrExit

windowSpecs :: Display
windowSpecs = InWindow windowTitle windowSize windowPosition

type RunningTime = Float

type TimeDelta = Float

render :: RunningTime -> Picture
render runningTime = color barColor $ polygon [upperLeftCorner, lowerLeftCorner, upperRightCorner, lowerRightCorner]
  where
    progressInPixels = runningTime * pixelsPerSecond
    leftBoundaryXPosition = min 0 (- halfMonitorWidthInPixels + progressInPixels)
    rightBoundaryXPosition = max 0 (halfMonitorWidthInPixels - progressInPixels)
    upperLeftCorner = (leftBoundaryXPosition, - halfBarHeightInPixels)
    lowerLeftCorner = (leftBoundaryXPosition, halfBarHeightInPixels)
    upperRightCorner = (rightBoundaryXPosition, halfBarHeightInPixels)
    lowerRightCorner = (rightBoundaryXPosition, - halfBarHeightInPixels)

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
