{-
    Sudoku.hs - solver for sudoku puzzles using an "elegant brute" approach

    Test grid is a list of lists of digit characters, with '0' meaning blank.

    Hints:
        - In functional programs, map is equivalent to the "for each" concept
        - Built-in functions exist for many common tasks - filter, all, etc.
-}
-- ******** Data Types ********

-- A Row is a list of values of any type
type Row a = [a]
-- A Matrix is a list of Rows of any type
type Matrix a = [Row a]
-- A Digit is a single character
type Digit = Char
-- A Grid is a matrix of digits
type Grid = Matrix Digit
-- We represent available choices as a list of digits 
type Choices = [Digit]

-- ******** Top-Level Algorithm ********

{-
    Solve - brute force but elegant in the functional style with laziness
    1. Replace each blank cell by a list of all possible digits (choices)
    2. Generate a complete list of all possible grids given the choices (expand)
    3. Identify each valid grid (valid) and select out only such grids (filter)
    Example: head $ solveSuperSlow testPuzzle
-}
--solve :: 
solve = filter valid . expand . choices

{-
    For each row... For each cell in that row...
        If it is blank, substitute in its place a list of all digits
        Else it's a digit, substitute a singleton list containing that digit
   Example: head $ choices testPuzzle =
                ["6","123456789","3","7","1","9","4","5","8"]  -- first row
-}

choicehelp :: [Digit] -> [String]
choicehelp "" = []
choicehelp x = if (blank (head x)) then digits : choicehelp (tail x) 
else (take 1 x) : choicehelp (tail x)
 
choices :: Grid -> Matrix Choices
choices [] = [[]]
choices xs = choicehelp (head xs) : choices (tail xs)


{-
  Generate a complete list of all possible expansions of the grid given the
  choices for initially blank cells. Think of this as a... cartesian product!
  Note: Cells in the resulting grids will still be Choices, but each Choices
  list will have only one element at that point.
  Example: head $ expand $ choices testPuzzle = [ "613719458",
                                                  "718624931",
                                                  "491813276",
                                                  "569178342",
                                                  "837542169",
                                                  "141936587",
                                                  "314287195",
                                                  "975361814",
                                                  "286491713" ]
												 
												 head $ cp $ expand $ choices testPuzzle
-}
expandhelp:: Matrix Choices -> [Grid]
expandhelp [[]] = []
expandhelp xs = (cp (head xs): expandhelp (tail xs))

expand:: Matrix Choices -> [Grid]
expand [[]] = []
expand xs = cp(expandhelp xs)

{-
  A valid grid is one for which...
    each row contains no duplicates,
    each column contains no duplicates,
    each box (3x3) contains no duplicates
    Example: valid $ head $ expand $ choices testPuzzle = False
-}

validc ::[String] -> Bool
validc [] = True
validc x = if (nodups (head x)) then (validc (tail x)) else False

valid :: Grid -> Bool
valid g = validc (cols g) && validc (rows g) && validc (boxs g) 




-- ******** Auxiliary Functions ********

-- Just a list of all the valid digits.
digits = "123456789"

{-
  Simple predicate that tests if a cell is blank.
  Use point-free partial function application.
-}
blank :: Digit -> Bool
blank = \a -> a == '0'

{-
  Computes, as a list, the Cartesian product of lists. A Cartesian product of
  N sets is the set of N-tuples of values of each of the original sets.

  Example: cp [[1,2],[4,5]] = [[1,4],[1,5],[2,4],[2,5]]
  Tip: You can use a list comprehension for this.
-}

cp []= [[]]
cp (x:xs) = [ a:b | a <- x, b <- (cp xs)]


{-
  Grabs sub-lists of 3 elements at-a-time and builds a list of these groups.
  Example: group "603719458" = ["603","719","458"]
-}
group :: Choices -> Grid
group "" = []
group xs = (take 3 xs): (group (drop 3 xs))

{-
  Returns a list of "groups" to its original form.
  Example: ungroup ["603","719","458"] = "603719458"
  Tip: There is a pre-defined function that does exactly what we need.
-}
ungroup :: Grid -> Choices
ungroup xs = concat xs

{-
  Returns a list whose elements are rows of the original matrix.
  Tip: This one is trivial since a grid should already be a list of rows!
-}

rows :: Grid -> Grid
rows x = x

{-
  Returns a list whose elements are the columns from the original matrix.
  Tip: Apply your unparalleled mastery of recursion.
-}

chelp :: Grid -> Choices
chelp [] = ""
chelp xs = (take 1 (head xs)) ++ chelp (tail xs)

chelp2 :: Grid -> Grid
chelp2 [] = []
chelp2 xs = (drop 1(head xs)) : chelp2 (tail xs)

--cols :: Grid -> Grid
cols [] = []
cols [[]] =[]
cols xs = if (chelp xs == "") then [] else chelp xs : (cols (chelp2 xs)) 


{-
  Returns a list whose elements represent all of non-overlapping 3x3
  sub-grids from the original matrix, so that each sub-grid is a single
  list.
  Example: boxs textPuzzle = [ "603708491", ..., "095804713" ]
  Tip: Use point-free function compisition with only map, group, ungroup, cols
-}
boxhelp :: Grid -> Grid
boxhelp [] = []
boxhelp xs = if ((length xs)== 24) then boxhelp (drop 6 xs)
             else if ((length xs)== 15) then boxhelp (drop 6 xs)
			 else if ((length xs)== 6) then boxhelp []	 
			 else (head xs ++ head (drop 3 xs) ++ head (drop 6 xs)):boxhelp (tail xs)
			 
boxs :: Grid -> Grid
boxs [] = []
boxs xs = boxhelp (concat (map group xs))



{-
  Returns true provided no element of the list appears twice, false otherwise.
  Tip: Look up the pre-defined function all from Prelude.
-}
nodups ::(Eq a) => [a] -> Bool
nodups [] = True
nodups xs = if elem (head xs) (tail xs) then False else nodups (tail xs)

-- ***************************

testPuzzle = [
    "603719458",
    "708624931",
    "491803276",
    "569178342",
    "837542169",
    "140936587",
    "314287095",
    "975361804",
    "286490713"
    ]