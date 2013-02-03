import System.Random
import System.Time
import Control.Exception
import Data.Char
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

mastermind = do
		let letras = ['A','B','C','D','E','F']
		let poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]	
		let shuf = shuffle poblacion
		--print (unsafeDupablePerformIO shuf)
		let guess = take 1 (unsafeDupablePerformIO shuf)
		--print guess
		hr'
		putStr (take 35 (cycle " "))
		putStr ("MASTERMIND")
		putStr (take 35 (cycle " "))
		hr'
		br'
		tab'
		putStr "Nombre de jugador: "
		nombre <- getLine
		tab'
		putStrLn ("Bienvenid@ " ++ nombre ++ "!")
		br'
		tab'		
		putStr "Mi adivinanza es: "
		print guess
		br'
		putStrLn "-[ Retroalimentacion ]-"
		putStr "Ingrese cuantas coinciden en letra y posicion: "
		letrapos <- getLine
		putStr "Ingrese cuantas coinciden en letra pero con diferente posicion: "
		difpos <- getLine
		putStr(comprueba(read letrapos, read difpos))
                
hr' = putStr (take 80 (cycle "*"))
br' = putStrLn " "
tab' = putStr (take 5 (cycle " "))

comprueba :: (Int,Int) -> String
comprueba (a,b)
    | (a,b) <= (0,0) = "Bien!"
    | (a,b) <= (1,0) = "Bien!"
	| (a,b) <= (2,0) = "Bien!"
	| (a,b) <= (3,0) = "Bien!"
	| (a,b) <= (4,0) = "Bien!"
	| (a,b) <= (0,1) = "Bien!"
	| (a,b) <= (1,1) = "Bien!"
	| (a,b) <= (2,1) = "Bien!"
	| (a,b) <= (3,1) = "Bien!"
	| (a,b) <= (0,2) = "Bien!"
	| (a,b) <= (1,2) = "Bien!"
	| (a,b) <= (2,2) = "Bien!"
	| (a,b) <= (0,3) = "Bien!"
	| (a,b) <= (1,3) = "Bien!"
	| (a,b) <= (0,4) = "Bien!"
    | otherwise      = "Combinacion invalida. (u_u)"
        
shuffle :: [a] -> IO[a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs 
                        