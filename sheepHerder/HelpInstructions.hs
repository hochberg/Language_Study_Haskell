module HelpInstructions
	(helpInstruct) where

import System.IO
import Data.Char
	
helpInstruct :: IO()
helpInstruct = hPutStr stderr "HELP INSTRUCTIONS\n\
							\SHEEP HERDER operates by typing a command and then firing it by pressing ENTER.\n\
							\It does not matter if the command is upper-case or lower-case.\n\
							\The list of commands SHEEP HERDER responds to are as follows:\n\
							\[N] - To move your player North\n\
							\[E] - To move your player East\n\
							\[S] - To move your player South\n\
							\[W] - To move your player West\n\
							\[T] - To take the item in the current location\n\
							\[D] - To move the item from the current location\n\
							\[F] - To enter a fight (only applies to certain locations)\n\
							\[I] - To view your inventory\n\
							\[L] - To look around your current location\n\
							\[Q] - To quit the game\n\
							\[I] - To view your inventory\n\
							\[H] - To access these help instructions\n"
							