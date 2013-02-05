import System.Random
import System.Time
import System.IO
import Control.Exception
import Data.Char
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe

mastermind = do				
		guardarLetras
		let letras = unsafeDupablePerformIO (readFile "letras.txt")		
		let poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]	
		let shuf = shuffle poblacion						
		let guess = take 1 (unsafeDupablePerformIO shuf)		
		hr'
		putStr (take 35 (cycle " "))
		putStr ("MASTERMIND")
		putStr (take 35 (cycle " "))
		hr'		
		putStr "\n\tNombre de jugador: "
		nombre <- getLine		
		putStrLn ("\tBienvenid@ " ++ nombre ++ "!\n\n")		
		putStr "\n\tMi adivinanza es: "
		putStrLn (head guess)
		putStrLn "\n\n\t-[ Retroalimentacion ]-"
		linea
		putStr "\tCuantas letras estan en la posicion correcta? "
		letrapos <- getLine
		putStr "\tCuantas letras estan en la posicion incorrecta? "
		difpos <- getLine
		putStr(comprueba(read letrapos, read difpos))
                
hr' = putStr (take 80 (cycle "*"))
br' = putStrLn " "
tab' = putStr (take 5 (cycle " "))
linea = putStrLn "\t-------------------------------------------"

comprueba :: (Int,Int) -> String
comprueba (a,b)
    | fst (a,b) == 0 && snd (a,b) == 0 = "Caso 1!"
    | fst (a,b) == 1 && snd (a,b) == 0 = "Caso 2!"
	| fst (a,b) == 2 && snd (a,b) == 0 = "Caso 3!"
	| fst (a,b) == 3 && snd (a,b) == 0 = "Caso 4!"
	| fst (a,b) == 4 && snd (a,b) == 0 = "Caso 5!"
	| fst (a,b) == 0 && snd (a,b) == 1 = "Caso 6!"
	| fst (a,b) == 1 && snd (a,b) == 1 = "Caso 7!"
	| fst (a,b) == 2 && snd (a,b) == 1 = "Caso 8!"
	| fst (a,b) == 3 && snd (a,b) == 1 = "Caso 9!"
	| fst (a,b) == 0 && snd (a,b) == 2 = "Caso 10!"
	| fst (a,b) == 1 && snd (a,b) == 2 = "Caso 11!"
	| fst (a,b) == 2 && snd (a,b) == 2 = "Caso 12!"
	| fst (a,b) == 0 && snd (a,b) == 3 = "Caso 13!"
	| fst (a,b) == 1 && snd (a,b) == 3 = "Caso 14!"
	| fst (a,b) == 0 && snd (a,b) == 4 = "Caso 15!"
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
	
guardarLetras = do    
		outh <- openFile "letras.txt" WriteMode
		let letters = ["ABCDEF"]
		hPutStr outh (head letters)
		hClose outh					
		
{--	
savePopulation = do    
		outh <- openFile "poblacion.txt" WriteMode
		let letters = ['A','B','C','D','E','F']
		let population = [[w,x,y,z]|w<-letters,x<-letters,y<-letters,z<-letters]
		hPrint outh population
		hClose outh
		
xNegras :: IO String
xNegras = do
	putStr "\n\t# de coincidencias en letra y posicion: "
	x <- getLine
	if (isNumber x) 
		then xn = unsafeDupablePerformIO x
		else xNegras
		
yBlancas :: IO String
yBlancas = do
	putStr "\n\t# de coincidencias solo en letra: "
	y <- getLine
	if (isNumber y) 
		then yn = unsafeDupablePerformIO y
		else yBlancas--}