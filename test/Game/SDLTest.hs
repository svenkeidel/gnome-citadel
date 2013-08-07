{-# LANGUAGE ScopedTypeVariables #-}
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLImage

import Paths_gnome_citadel (getDataFileName)

loadImage file = SDLImage.load =<< getDataFileName ("tiles/" ++ file)

main = SDL.withInit [SDL.InitEverything] $ do

  surface <- SDL.setVideoMode 800 600 32 [SDL.HWSurface]
  SDL.setCaption "HSDwarf" ""

  -- load image from tile directory
  dwarfs <- loadImage "dwarves.png"

  -- draw image on screen
  SDL.blitSurface dwarfs Nothing surface Nothing

  -- the video surface is double buffered. This command switches the modified
  -- surface with the surface that is currently shown. This prevents
  -- flickering of the screen.
  SDL.flip surface

  -- allocated images have to be freed.
  SDL.freeSurface dwarfs

  SDL.delay 2000
