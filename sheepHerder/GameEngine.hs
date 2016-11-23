import System.IO
import Data.Char
import qualified Matrix as Ma
import qualified Sketch as Sk
import qualified HelpInstructions as He





--DATA TYPES--
--PLAYER
data Player = Player String Backpack String deriving (Show)
 
playerName :: Player -> String
playerName (Player playerName _ _) = playerName
 
playerBackpack :: Player -> Backpack
playerBackpack (Player _ playerBackpack _) = playerBackpack
 
playerDifficulty :: Player -> String
playerDifficulty (Player _ _ playerDifficulty) = playerDifficulty
						
						
--ITEM
data Item = Item String String | None deriving (Show, Eq)
 
itemName :: Item -> String
itemName (Item itemName _) = itemName
 
itemDescrip :: Item -> String
itemDescrip (Item _ itemDescrip) = itemDescrip
 
--LOCATION
data Location = Location Int String String Item 

locationID :: Location -> Int
locationID (Location locationID _ _ _) = locationID
 
locationName :: Location -> String
locationName (Location _ locationName _ _) = locationName
 
locationDescrip :: Location -> String
locationDescrip (Location _ _ locationDescrip _) = locationDescrip
 
locationItem :: Location -> Item
locationItem (Location  _ _ _ locationItem) = locationItem

instance Show Location where
	show (Location a b c d) = show a ++ b ++ c ++ show d
 
 
--DIFFICULTY
data Difficulty = Difficulty String deriving (Show)
 
 
--GAME STATE
data GameState = Normal Player Location Int Int | Terminated String deriving (Show)
 
gameStatePlayer :: GameState -> Player
gameStatePlayer (Normal gameStatePlayer _ _ _) = gameStatePlayer
 
gameStateLocation :: GameState -> Location
gameStateLocation (Normal _ gameStateLocation _  _ ) = gameStateLocation

gameStateMoves :: GameState -> Int
gameStateMoves (Normal _ _ gameStateMoves _ ) = gameStateMoves

gameStateSheep :: GameState -> Int
gameStateSheep (Normal _ _ _ gameStateSheep) = gameStateSheep

gameStateGameOver :: GameState -> String
gameStateGameOver (Terminated msg) = msg

--CONTAINER CLASS--
class Container c where
    empty :: c -> Bool
    contents :: c -> [Item]
    acquire :: Item -> c -> c
    release :: Item -> c -> c

--BACKPACK
data Backpack = Backpack [Item] deriving (Show)

instance Container Backpack where
    contents (Backpack i) = i
    empty chst = contents chst == []
    acquire i (Backpack xs) = Backpack (i:xs)
    release i (Backpack _) = Backpack []

 
 
--FUNCTIONS--
 
--Introduction String 
gameIntro :: IO ()
gameIntro = putStrLn "Welcome to SHEEP HERDER\n\nYou live a lonely isolated life in the middle of Nothing, Nowhere.\
					\ Your one purpose is to herd a flock of sheep. Recently, a force of merciless wolves have\
					\ inhabited your area and have been slaughtering your sheep. The longer you wait to deal\
					\ with this problem, the more sheep die. It's time to fight back!\n\
					\Type [H] for help instructions.\n\nYou are the SHEEP HERDER!"

--Game Ending String
gameCredits :: IO ()
gameCredits = putStrLn "GAME OVER\n\nCreated by Alex Hochberg. 2015."

--Sets player fields to custom inputs 
customize :: IO GameState
customize = do
	hPutStr stderr "Please enter your name: "
	name <- getLine
	hPutStr stderr "Choose a Difficulty [EASY, NORMAL, HARD]: "
	dif <- getLine
	if (not (((map toUpper dif) == "EASY") || ((map toUpper dif) == "NORMAL") || ((map toUpper dif) == "HARD"))) 
		then hPutStr stderr "INVALID COMMAND. Your Difficulty has been set to NORMAL.\n" 
		else return ()
	hPutStr stderr "Your Name is "
	putStr (map toUpper name)
	putStr " and your Difficulty is "
	if (not (((map toUpper dif) == "EASY") || ((map toUpper dif) == "NORMAL") || ((map toUpper dif) == "HARD"))) 
		then putStr "NORMAL" 
		else putStr (map toUpper dif)
	putStr ("." ++ "\n")
	if (not (((map toUpper dif) == "EASY") || ((map toUpper dif) == "NORMAL") || ((map toUpper dif) == "HARD"))) 
		then return $ Normal (Player (map toUpper name) (Backpack []) "NORMAL") 
					(Location 5 "Shire" "The Shire. Your stomping grounds. You see a crook laying on the ground." 
					(Item "Crook" "A sturdy yet unimposing hooked staff.")) 0 100
	    else return $ Normal (Player (map toUpper name) (Backpack []) (map toUpper dif)) 
					(Location 5 "Shire" "The Shire. Your stomping grounds. You see a crook laying on the ground." 
					(Item "Crook" "A sturdy yet unimposing hooked staff.")) 0 100
	

-- processes getLine commands and creates proper gamestate for each situation 
commander :: GameState -> String -> GameState
commander (Terminated _) _ = Terminated ""
commander st "" = st
commander st co
	| (toUpper (head co)) == 'N' = Normal (gameStatePlayer st)
						(navigatePlaces 0 (gameStateLocation st))
						(1+(gameStateMoves st))
						((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head co)) == 'E' = Normal (gameStatePlayer st)
						(navigatePlaces 1 (gameStateLocation st))
						(1+(gameStateMoves st))
						((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head co)) == 'S' = Normal (gameStatePlayer st)
						(navigatePlaces 2 (gameStateLocation st))
						(1+(gameStateMoves st))
						((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head co)) == 'W' = Normal (gameStatePlayer st)
						(navigatePlaces 3 (gameStateLocation st))
						(1+(gameStateMoves st))
						((gameStateSheep st) - (wolfDestroyer st))
						             --checks to see if player already has item
	| (toUpper (head co)) == 'T'  =  if (elem (locationItem(gameStateLocation st)) (contents(playerBackpack(gameStatePlayer st))))
										then Normal (Player(playerName(gameStatePlayer st))
										(acquire (locationItem (gameStateLocation st))(playerBackpack(gameStatePlayer st)))
										(playerDifficulty (gameStatePlayer st)))
										(Location (locationID(gameStateLocation st))
										(locationName(gameStateLocation st))
										("ALREADY HAVE ITEM")
										(locationItem(gameStateLocation st)) )
										(1+(gameStateMoves st))
										((gameStateSheep st) - (wolfDestroyer st))
									else  --checks to see if location has item
									if (locationItem(gameStateLocation st) == None)
									 then Normal (Player(playerName(gameStatePlayer st))
											(acquire (locationItem (gameStateLocation st))(playerBackpack(gameStatePlayer st)))
											(playerDifficulty (gameStatePlayer st)))
											(Location (locationID(gameStateLocation st))
											(locationName(gameStateLocation st))
											("NO ITEM TO PICK UP")
											(locationItem(gameStateLocation st)) )
											(1 +(gameStateMoves st))
											((gameStateSheep st) - (wolfDestroyer st))
									else
										Normal (Player(playerName(gameStatePlayer st))
										(acquire (locationItem (gameStateLocation st))(playerBackpack(gameStatePlayer st)))
										(playerDifficulty (gameStatePlayer st)))
										(Location (locationID(gameStateLocation st))
										(locationName(gameStateLocation st))
										("You are picking up "++(itemName(locationItem (gameStateLocation st))))
										(locationItem(gameStateLocation st)) )
										(1 +(gameStateMoves st))
										((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head co)) == 'D'  =  Normal (Player(playerName(gameStatePlayer st))
									(release (locationItem (gameStateLocation st))(playerBackpack(gameStatePlayer st)))
									(playerDifficulty (gameStatePlayer st)))
									(Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  ("You are dropping "++(itemName(locationItem (gameStateLocation st))))
								  (locationItem(gameStateLocation st)) )
									(1+(gameStateMoves st))
									((gameStateSheep st) - (wolfDestroyer st))
						
    | (toUpper (head co)) == 'L' =  Normal (gameStatePlayer st)
								  (Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  (locationName(gameStateLocation st))
								  (locationItem(gameStateLocation st)) )
											(1+(gameStateMoves st))
											((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head co)) == 'I' =  Normal (gameStatePlayer st)
								  (Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  ("Inventory:" ++ (inventReader(contents (playerBackpack(gameStatePlayer st)))))
								  (locationItem(gameStateLocation st)))
											((gameStateMoves st))
											(gameStateSheep st)
	| (toUpper (head co)) == 'M' =  Normal (gameStatePlayer st)
								  (Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  "Map"
								  (locationItem(gameStateLocation st)))
											((gameStateMoves st))
											(gameStateSheep st)
	| (toUpper (head co)) == 'H' =  Normal (gameStatePlayer st)
								  (Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  "Help"
								  (locationItem(gameStateLocation st)))
											((gameStateMoves st))
											(gameStateSheep st)										
	| (toUpper (head co)) == 'F' =  if (locationID(gameStateLocation st) == 7)
										then if (elem (Item "Crook" "A sturdy yet unimposing hooked staff.")(contents(playerBackpack(gameStatePlayer st))))
											then Normal (Player(playerName(gameStatePlayer st))
											(acquire (Item "RARFU'S FANG" "Deadly")(playerBackpack(gameStatePlayer st)))
											(playerDifficulty (gameStatePlayer st)))
											(Location (locationID(gameStateLocation st))
											(locationName(gameStateLocation st))
											"YOU HAVE DEFEATED RARFU WITH YOUR CROOK! You have obtained RARFU'S FANG."
											(locationItem(gameStateLocation st)) )
											(1+(gameStateMoves st))
											((gameStateSheep st) - (wolfDestroyer st))
										else (Terminated "RARFU GNAWS YOUR FACE. YOU HAD NOTHING TO FIGHT RARFU WITH!")
									else
									if (locationID(gameStateLocation st) == 9)
										then if (elem (Item "RARFU'S FANG" "Deadly")(contents(playerBackpack(gameStatePlayer st))))
											then Normal (Player(playerName(gameStatePlayer st))
											(acquire (Item "VAGMAT'S CLAWS" "Vicious")(playerBackpack(gameStatePlayer st)))
											(playerDifficulty (gameStatePlayer st)))
											(Location (locationID(gameStateLocation st))
											(locationName(gameStateLocation st))
											"YOU HAVE DEFEATED VAGMAT WITH RARFU'S FANG! You have obtained VAGMAT'S CLAWS."
											(locationItem(gameStateLocation st)) )
											(1+(gameStateMoves st))
											((gameStateSheep st) - (wolfDestroyer st))
										else (Terminated "VAGMAT RIPS OPEN YOUR THROAT. YOU NEED TO FIGHT EASIER FOES FIRST!")
									else
									if (locationID(gameStateLocation st) == 0)
										then if (elem (Item "VAGMAT'S CLAWS" "Vicious")(contents(playerBackpack(gameStatePlayer st))))
											--winning string, show how many sheep have survived
											then (Terminated ("YOU HAVE DONE IT " ++ (playerName(gameStatePlayer st)) ++ "! \
											\YOU HAVE DEFEATED THE MAD WOLF! Peace in your sanctuary will now be restored. "
											++ (show(gameStateSheep st)) ++ "SHEEP HAVE SURVIVED!" ))
										else (Terminated "THE MAD WOLF DECAPATATES YOU IN ONE SWIPE! FIGHT EASIER FOES FIRST")
									else Normal (gameStatePlayer st)
								  (Location (locationID(gameStateLocation st))
						          (locationName(gameStateLocation st))
								  "NO ONE TO FIGHT"
								  (locationItem(gameStateLocation st)) )
											(1+(gameStateMoves st))
											((gameStateSheep st) - (wolfDestroyer st))
	| (toUpper (head  co)) == 'Q'  = Terminated "You have quit SHEEP HEARDER"
	| otherwise = Normal (gameStatePlayer st)
						(Location (locationID(gameStateLocation st))
								  (locationName(gameStateLocation st))
								  ("Invalid command")
								  (locationItem(gameStateLocation st)) )
						(1+(gameStateMoves st))
						((gameStateSheep st) - (wolfDestroyer st))
						
-- map command reader
mapCommander :: GameState -> String -> IO()
mapCommander st co  
		| (toUpper (head co)) == 'M' = if (elem (Item "Map" "A comprehensive map of the area.")(contents(playerBackpack(gameStatePlayer st))))
										then Sk.sketch1
										else hPutStr stderr "You do not have a map!"
		| (toUpper (head co)) == 'H' = He.helpInstruct								
		| otherwise = return()
 
--controls the game loop	
gameLoop :: GameState -> IO ()
gameLoop (Terminated s) = do
	hPutStr stderr ((displayState (Terminated s)) ++ "\n") 
	return ()
gameLoop gs = 
	--ends game if all sheep die
	if ((gameStateSheep gs) <= 0)
		then do 
		hPutStr stderr ((displayState (Terminated "ALL OF YOUR SHEEP HAVE DIED")) ++ "\n") 
		return ()
	    else do
		hPutStr stderr ((displayState gs) ++ "\n")
		hPutStr stderr "Moves: "
		print (gameStateMoves gs)
		hPutStr stderr "Sheep Alive: "
		print (gameStateSheep gs)
		hPutStr stderr "Enter command "
		command <- getLine	
		result <- return $ commander gs command
		mapCommander gs command
		gameLoop result					 
															 
															 
--displays current state of the game
--set to show locationDescrip or Terminated msg															 
displayState :: GameState -> String
displayState (Terminated msg)  = show msg
displayState (Normal p (Location id n d it) i s) = show d

--IO that controls whole game
main:: IO()
main = do
	return gameIntro()
	gSt <- customize
	gameLoop gSt
	return gameCredits()
 
 
--used to navigate through the navmatrix after receiving 
-- int index 0-3, corresponding with north- south respectively 
navigate :: Int -> Location -> Ma.LocId
navigate n l
	| (n==0) =  Ma.north(navm !! (locationID l)) 
	| (n==1) =  Ma.east(navm !! (locationID l)) 
    | (n==2) =  Ma.south(navm !! (locationID l)) 
	| (n==3) =  Ma.west(navm !! (locationID l))
	| otherwise = (locationID l) 
	 where navm = Ma.navmatrix


--Creates locations based on LocId	 
places :: Ma.LocId -> Location
places id 
		| (id == 0) = Location 0  "THE MAD WOLF DEN" "THE MAD WOLF DEN. Home of THE MAD WOLF.\
								  \Type [F] if you dare to fight THE MAD WOLF" None
		| (id == 1) = Location 1  "Stone Peak" "Stone Peak. Just stones and a peak." (Item "Stone" "Not much to this.")
		| (id == 2) = Location 2  "Desolate Flatlands" "Desolate Flatlands. Travel North if you dare awaken the MAD WOLF." None
		| (id == 3) = Location 3  "Boring Plain" "Boring Plain. Boring..." None
		| (id == 4) = Location 4  "Westeros" "Westeros. You see a tattered piece of paper on the ground." (Item "Map" "A comprehensive map of the area.")
		| (id == 5) = Location 5  "The Shire" "The Shire. Your stomping grounds. You see a crook laying on the ground." (Item "Crook" "A sturdy yet unimposing hooked staff.")
		| (id == 6) = Location 6  "Easteros" "Easteros. You feel safe here. \
								  \Something shiny catches your eye sticking under a pile of leaves."
								  (Item "Gem of Life" "Gem of Life!")
		| (id == 7) = Location 7  "Cave of Doom" "Cave of Doom. The vicious wolf Rarfu inhabits this place. Type [F] to fight him." None
		| (id == 8) = Location 8  "Woodlands" "Woodlands. You are between to dangerous foes. Luckily, you have a forest of trees to shield you." None
		| (id == 9) = Location 9  "Hell Swamp" "Hell Swamp. Evil and Torturous Vagmat dwells in the swamps.\
								   \Type [F] to fight him. Hopefully, you have had some training." None
		| otherwise = Location id "" "Invalid Move" None

-- combines the helper functions navigate and place
-- and deals with the case of an invalid movement		
navigatePlaces :: Int -> Location -> Location
navigatePlaces n l = if ((navigate n l)== Ma.no) then (Location (locationID l)
								  (locationName l)
								  "Invalid move"
								  (locationItem l)) else 
								  (places(navigate n l))
								  

--Changes a Backpack into a String to view Inventory								  
inventReader :: [Item] -> String
inventReader [] = "."
inventReader bp = (itemName (head bp)) ++ "-" ++ (itemDescrip (head bp)) ++ "" ++ (inventReader (tail bp))

--assigns how many sheep die per turn by difficulty and what wolves are still alive
wolfDestroyer :: GameState -> Int
wolfDestroyer (Terminated _) = 0
wolfDestroyer st = if ((playerDifficulty(gameStatePlayer st)) == "EASY")
						then if (elem (Item "VAGMAT'S CLAWS" "Vicious")(contents(playerBackpack(gameStatePlayer st))))
							then 1
						else if (elem (Item "RARFU'S FANG" "Deadly")(contents(playerBackpack(gameStatePlayer st))))
							then 2
						else 3
					else
					if ((playerDifficulty(gameStatePlayer st)) == "NORMAL")
						then if (elem (Item "VAGMAT'S CLAWS" "Vicious")(contents(playerBackpack(gameStatePlayer st))))
							then 2
						else if (elem (Item "RARFU'S FANG" "Deadly")(contents(playerBackpack(gameStatePlayer st))))
							then 4
						else  6
					else --"HARD CASE"
						if (elem (Item "VAGMAT'S CLAWS" "Vicious")(contents(playerBackpack(gameStatePlayer st))))
							then 3
						else if (elem (Item "RARFU'S FANG" "Deadly")(contents(playerBackpack(gameStatePlayer st))))
							then 6
						else  9
					
				
	
		

	
	
