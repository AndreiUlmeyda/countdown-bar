-- | Configuration values like window dimensions, positions, colors, etc.
module Config
  ( frameRate,
    barColor,
    backgroundColor,
    countdownLengthInSeconds,
    windowTitle,
    windowSize,
    windowPosition,
    initialBarWidth,
    halfBarHeightInPixels,
  )
where

import Graphics.Gloss.Data.Color (Color, makeColor)

-- | Defines the number of frames to render per second
frameRate :: Int
frameRate = 2

-- | Defines the foreground color
barColor :: Color
barColor = orange

-- | Defines the background color
backgroundColor :: Color
backgroundColor = green

orange :: Color
orange = makeColor 0.96 0.40 0 noTransparency

green :: Color
green = makeColor 0.0 0.6 0.0 noTransparency -- green

noTransparency :: Float
noTransparency = 1

countdownLengthInMinutes :: Float
countdownLengthInMinutes = 25

-- | The number of seconds it will take the bar to shrink to zero size and the program to exit.
countdownLengthInSeconds :: Float
countdownLengthInSeconds = countdownLengthInMinutes * 60

-- |  The title of the application window. Certain window managers / compositors can be configured to set the window
--    position automatically depending on the window title.
windowTitle :: String
windowTitle = "countdown"

-- | The size of the application window.
windowSize :: (Int, Int)
windowSize = (1912, 5)

-- |  The default position of the application window. The actual window position will depend on the configuration
--      of your window manager / compositor.
windowPosition :: (Int, Int)
windowPosition = (0, 0)

-- |  Measures the vertical dimensions of the bar to be drawn in pixels. This should exceed the corresponding window
--      dimension and will be derived from those in later versions.
halfBarHeightInPixels :: Float
halfBarHeightInPixels = 6

-- |  Measures the horizontal dimensions of the bar to be drawn in pixels. This should exceed the corresponding window
--      dimension and will be derived from those in later versions.
initialBarWidth :: Float
initialBarWidth = 1900
