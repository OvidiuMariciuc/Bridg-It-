import BridgIt
import GHC.IO.Encoding
import System.IO
import Data.Char

main :: IO ()
main = do
	   putStrLn " Welcome to"
	   putStrLn " BRIDG-IT BOARD GAME : PLAYER VS PLAYER MODE\n\n"
	   startgame (generateBoard 6 3) Circle1

startgame :: Board -> Field -> IO ()
startgame board player
  | checkWinner1 board = putStrLn " Felicitari Player 1(O) -> Ai castigat!"
  | checkWinner2 board = putStrLn " Felicitari Player 2(@) -> Ai castigat!"  
  | otherwise = do
	   print board
	   if player == Circle1 then
	     putStrLn "Player 1(O) -> Este randul tau!"
	   else putStrLn "Player 2(@) -> Este randul tau!"	 	  
	   putStrLn "Alege linia pentru urmatoarea mutare:"
	   linie <- getLine
	   putStrLn "Alege coloana pentru urmatoarea mutare:"
	   coloana <- getLine
	   putStrLn "Alege tipul de linie(orizontala/verticala) pentru urmatoarea mutare:"
	   tiplinie <- getLine
	   let x = read linie :: Int
	       y = read coloana :: Int
	       l = getLinie tiplinie
	       board' = placeBridge board x y l
	   if checkPosition board player l x y == True then
	     if player == Circle1 then
		   startgame board' Circle2
		 else startgame board' Circle1		 
	   else putStrLn "\nMutare gresita! Incearca din nou.\n">>(startgame board player) 		   