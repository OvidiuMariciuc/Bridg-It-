--Bridg-It local - 2 players (human vs human)
--Author: Mariciuc Ovidiu
module BridgIt where
import GHC.IO.Encoding


data Field =  Null | Circle1 | Circle2 | HorizontalLine | VerticalLine deriving (Eq)
instance Show Field where
  show (Null) = " "
  show (Circle1) = "O" {-○-}
  show (Circle2) = "@" {-●-}
  show (HorizontalLine) = "-" {-│▬-}
  show (VerticalLine) = "|"
  
type Matrix a = [[a]]
data Board = Board (Matrix Field)
instance Show Board where
  show (Board (row:restRows)) = (showRow row) ++ "\n" ++ (show (Board restRows))
  show (Board []) = ""
showRow :: [Field] -> String
showRow [] = ""
showRow (head:tail) = show head ++ " " ++ showRow tail

testBoard1 = Board [[Null,Circle1,Null,Circle1,Null,Circle1,Null] , [Circle2,VerticalLine,Circle2,Null,Circle2,Null,Circle2], [Null,Circle1,HorizontalLine,Circle1,Null,Circle1,Null] ,
                   [Circle2,Null,Circle2,VerticalLine,Circle2,Null,Circle2], [Null,Circle1,Null,Circle1,Null,Circle1,Null], [Circle2,Null,Circle2,VerticalLine,Circle2,Null,Circle2], [Null,Circle1,Null,Circle1,Null,Circle1,Null]]

testBoard2 = Board [[Null,Circle1,Null,Circle1,Null,Circle1,Null] , [Circle2,HorizontalLine,Circle2,Null,Circle2,Null,Circle2], [Null,Circle1,VerticalLine,Circle1,Null,Circle1,Null] ,
                   [Circle2,Null,Circle2,HorizontalLine,Circle2,HorizontalLine,Circle2], [Null,Circle1,Null,Circle1,Null,Circle1,Null], [Circle2,Null,Circle2,Null,Circle2,Null,Circle2], [Null,Circle1,Null,Circle1,Null,Circle1,Null]]

generateRow1 :: Int -> [Field]
generateRow1 0 = []
generateRow1 n = [Null, Circle1] ++ (generateRow1 (n-1))

generateRow2 :: Int -> [Field]
generateRow2 0 = []
generateRow2 n = [Circle2, Null] ++ (generateRow2 (n-1))

generateBoard :: Int -> Int -> Board
generateBoard 0 m = Board [generateRow1 m]
generateBoard n m = 
  if n `mod` 2 == 0
  then case generateBoard (n-1) m of
	  Board (matrix) -> (Board ((generateRow1 (m)):matrix))
  else case generateBoard (n-1) m of
	  Board (matrix) -> (Board ((generateRow2 (m+1)):matrix))
	  
placeBridgeRow :: Field -> Int -> [Field] -> [Field]
placeBridgeRow field 0 (head:tail) = (field:tail)
placeBridgeRow field x (head:tail) = (head: (placeBridgeRow field (x-1) tail)) 
	  
placeBridge :: Board -> Int -> Int -> Field -> Board
placeBridge (Board (row:restRows)) 0 y field = (Board ((placeBridgeRow field y (row) : restRows)))
placeBridge (Board (row:restRows)) x y field = case placeBridge (Board restRows) (x-1) y field of 
                                                  Board (matrix) -> (Board (row:matrix))
												  
												  
getField :: Int -> Int -> Board -> Field
getField 0 y (Board (row:restRows)) = if y < 0 || y > 8 then 
                                        Null 
                                      else getFieldRow y row
getField x y (Board (row:restRows)) = if x < 0 || x > 8 || y < 0 || y > 8 then 
                                        Null
                                      else getField (x-1) y (Board restRows)

getFieldRow :: Int -> [Field] -> Field
getFieldRow 0 [] = Null
getFieldRow 0 (head:tail) = head
getFieldRow x (head:tail) = getFieldRow (x-1) tail
 	  
winner :: Board -> Bool
winner board = False

getLinie :: String -> Field
getLinie "orizontala" = HorizontalLine
getLinie "verticala" = VerticalLine

checkWinner1 :: Board -> Bool
checkWinner1 board
  | checkPath board 0 1 == True = True
  | checkPath board 0 3 == True = True  
  | checkPath board 0 5 == True = True 
  | otherwise = False
  
checkWinner2 :: Board -> Bool
checkWinner2 board
  | checkPath board 1 0 == True = True
  | checkPath board 3 0 == True = True  
  | checkPath board 5 0 == True = True  
  | otherwise = False
  
checkPath :: Board -> Int -> Int -> Bool
checkPath board x y
  | x>5 || y>5 = True
  | getField (x+1) y board == VerticalLine = checkPath (placeBridge board (x+1) y Null) (x+2) y  
  | getField (x-1) y board == VerticalLine = checkPath (placeBridge board (x-1) y Null) (x-2) y
  | getField x (y+1) board == HorizontalLine = checkPath (placeBridge board x (y+1) Null) x (y+2)
  | getField x (y-1) board == HorizontalLine = checkPath (placeBridge board x (y-1) Null) x (y-2)
  | otherwise = False
  
checkPosition :: Board -> Field -> Field -> Int -> Int -> Bool
checkPosition board Circle1 HorizontalLine x y = let left = getField x (y-1) board
                                                     right = getField x (y+1) board
                                                     current = getField x y board in
                                                   if current == Null && left == Circle1 && right == Circle1 then
                                                     True
                                                   else False
checkPosition board Circle1 VerticalLine x y = let up = getField (x-1) y board
                                                   down = getField (x+1) y board
                                                   current = getField x y board in
                                                 if current == Null && up == Circle1 && down == Circle1 then
                                                   True
                                                 else False
checkPosition board Circle2 HorizontalLine x y = let left = getField x (y-1) board
                                                     right = getField x (y+1) board
                                                     current = getField x y board in
                                                   if current == Null && left == Circle2 && right == Circle2 then
                                                     True
                                                   else False
checkPosition board Circle2 VerticalLine x y = let up = getField (x-1) y board
                                                   down = getField (x+1) y board
                                                   current = getField x y board in
                                                 if current == Null && up == Circle2 && down == Circle2 then
                                                   True
                                                 else False