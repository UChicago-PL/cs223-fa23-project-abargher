module InterfaceUtils where

resolutionMap :: [(Int, Int)]
resolutionMap = 
  [
    (500, 500),
    (1000, 1000),
    (1920, 1080),
    (2560, 1440),
    (3840, 2160),
    (5120, 2880),
    (1920, 1200),
    (2560, 1600),
    (3840, 2400),
    (5120, 3200)
  ]

resolutionOptions :: String
resolutionOptions = 
  "Choose an image resolution:\n\
  \  1) 500x500 (1:1)\n\
  \  2) 1000x1000 (1:1)\n\
  \  3) 1920x1080 (16:9)\n\
  \  4) 2560x1440 (16:9)\n\
  \  5) 3840x2160 (16:9)\n\
  \  6) 5120x2880 (16:9)\n\
  \  7) 1920x1200 (16:10)\n\
  \  8) 2560x1600 (16:10)\n\
  \  9) 3840x2400 (16:10)\n\
  \  10) 5120x3200 (16:10)\n\
  \  11) custom resolution\n\
  \\n\
  \Type a number (1-11), or blank for default value (3): \
  \"