module Matrix
	(navmatrix, Connections, no, north, south, east, west, LocId) where

import Data.List

-- Transparent type for readability
type LocId = Int 


----- Navigation matrix --------------------------------------
 
data Connections = Connections LocId LocId LocId LocId deriving (Show,Eq)

north :: Connections-> LocId
north (Connections n _ _ _) = n

east :: Connections-> LocId
east (Connections _ e _ _) = e

south :: Connections-> LocId
south (Connections _ _ s _) = s

west :: Connections-> LocId
west (Connections _ _ _ w) = w
 
--a NO location
no = -1
 
	 
----- N E S W		 
navmatrix = [
	Connections no no 2 no,    --loc 0
	Connections no 2 4 no,    -- 1
	Connections 0 3 5 1,      --2
	Connections no no 6 2 ,  -- 3
	Connections 1 5 7 no, -- 4
	Connections 2 6 8 4,  -- 5
	Connections 3 no 9 5,  -- 6
	Connections 4 8 no no, -- 7
	Connections 5 9 no 7, -- 8
	Connections 6 no no 8 -- 9
	]
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



