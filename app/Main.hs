module Main where

import Config
  ( backgroundColor,
    barColor,
    barHeightInPixels,
    countdownLengthInSeconds,
    frameRate,
    initialBarWidthInPixels,
    windowPosition,
    windowSize,
    windowTitle,
  )
import Graphics.Gloss.Data.Picture
  ( Picture (Pictures),
    color,
    polygon,
    rectangleSolid,
    scale,
    translate,
  )
import Graphics.Gloss.Interface.Pure.Game
  ( Display (InWindow),
    Event,
    play,
  )

main :: IO ()
main = play windowSpecs backgroundColor frameRate initialRunningTime render handleInputEvents updateRunningTimeOrExit

windowSpecs :: Display
windowSpecs = InWindow windowTitle windowSize windowPosition

type RunningTime = Float

type TimeDelta = Float

-- | render a bar ticking down towards the middle by drawing a full width rectangle, then
-- scaling it down in the y-direction as time goes by. Gloss' rectangleSolid function already
-- draws centered on the window, so no additional positioning is needed.
render :: RunningTime -> Picture
render runningTimeInSeconds = color barColor $ scale tickDownAlongYDimension leaveXDimensionUnchanged $ rectangleSolid initialBarWidthInPixels barHeightInPixels
  where
    tickDownAlongYDimension = max 0 $ (countdownLengthInSeconds - runningTimeInSeconds) / countdownLengthInSeconds :: Float
    leaveXDimensionUnchanged = 1 :: Float

handleInputEvents :: Event -> RunningTime -> RunningTime
handleInputEvents _ runningTime = runningTime -- ignore all events

initialRunningTime :: Float
initialRunningTime = 0

updateRunningTimeOrExit :: RunningTime -> TimeDelta -> RunningTime
updateRunningTimeOrExit deltaTime runningTime
  | runningTime > countdownLengthInSeconds = error "not really an error, please yell at the dev to exit properly"
  | otherwise = runningTime + deltaTime
