module Main where

import Config
  ( backgroundColor,
    barColor,
    countdownLengthInSeconds,
    frameRate,
    halfBarHeightInPixels,
    initialBarWidth,
    windowPosition,
    windowSize,
    windowTitle,
  )
import Graphics.Gloss.Data.Picture (Picture (Pictures), color, polygon, rectangleSolid, scale, translate)
import Graphics.Gloss.Interface.Pure.Game (Display (InWindow), Event, play)

main :: IO ()
main = play windowSpecs backgroundColor frameRate initialRunningTime render handleInputEvents updateRunningTimeOrExit

windowSpecs :: Display
windowSpecs = InWindow windowTitle windowSize windowPosition

type RunningTime = Float

type TimeDelta = Float

-- | render a bar ticking down towards the middle by drawing a full width rectangle, then
-- scaling it down as time goes by
render :: RunningTime -> Picture
render runningTime = Pictures [rightHalfBar] --Pictures [rightHalfBar, leftHalfBar]
  where
    rightHalfBar = color barColor $ scale yScale 1 $ rectangleSolid initialBarWidth halfBarHeightInPixels --fillingRectangle rightBoundaryXPosition
    yScale = max 0 $ (countdownLengthInSeconds - runningTime) / countdownLengthInSeconds

handleInputEvents :: Event -> RunningTime -> RunningTime
handleInputEvents _ runningTime = runningTime -- ignore all events

initialRunningTime :: Float
initialRunningTime = 0

updateRunningTimeOrExit :: RunningTime -> TimeDelta -> RunningTime
updateRunningTimeOrExit deltaTime runningTime
  | runningTime > countdownLengthInSeconds = error "not really an error, please yell at the dev to exit properly"
  | otherwise = runningTime + deltaTime
