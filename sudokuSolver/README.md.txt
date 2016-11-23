Data Types:
Row - Num a => [a]
Column - Num a => [a]
Sub-grid - Num a => [a]
Grid - Num a => [[a]]
Values - Num a => a
 
 
Solve:: (Num a) => [[a]] -> [[a]]
Solve grid = if (valid-grid grid) then if (check-solved grid) return grid otherwise
(random-num-generate gird)
 
 
Where:
valid-grid checks to make sure the initial given values within the sudoku
board can potentially give us a solved sudoku puzzle
 
check-solved checks to see if all the values of the rows columns and sub-grids
contain values from 0-9 (or are a solved sudoku puzzle)
 
random-num-generate fills in values that are not already given with random digits
from 0-9 (not-given sudoku values could be shown with a value of -1 when given 
a grid)
 

