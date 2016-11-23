module Sketch
	(sketch1) where

import System.IO
import Data.Char

sketch1 :: IO()
sketch1 = hPutStr stderr "              ____________              \n\
						 \             |            |             \n\
                         \             |  THE MAD   |             \n\
						 \             |  WOLF DEN  |             \n\
						 \             |____________|             \n\
						 \  ____________________________________  \n\
						 \ |           |             |          | \n\
						 \ |   STONE   |  DESOLATE   |  BORING  | \n\
						 \ |   PEAK    |  FLATLANDS  |  PLAIN   | \n\
						 \ |___________|_____________|__________| \n\
						 \ |           |             |          | \n\
						 \ |  WESTEROS |   THE       | EASTEROS | \n\
						 \ |           |   SHIRE     |          | \n\
						 \ |___________|_____________|__________| \n\
						 \ |           |             |          | \n\
						 \ |  CAVE OF  |  WOODLANDS  |  HELL    | \n\
						 \ |  DOOM     |             |  SWAMP   | \n\
						 \ |___________|_____________|__________| \n"