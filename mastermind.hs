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
		intentos (head guess) 		                      		

intentos :: String -> IO ()
intentos guess = do
		let historial=[]
		putStr "\n\t ---> Mi adivinanza es: "
		putStrLn guess
		putStrLn "\n\n\n\t-[ Retroalimentacion ]-"
		linea
		putStr "\tCuantas letras estan en la posicion correcta? "
		letrapos <- getLine
		putStr "\tCuantas letras estan en la posicion incorrecta? "
		difpos <- getLine
		guardaHistorial guess letrapos difpos
		if(comprueba(read letrapos, read difpos) == "Caso x!")
			then putStrLn "hacer el arregloAletorio y cambiar de pos en caso sea necesario"
			else
				if(comprueba(read letrapos, read difpos) == "---")
					then intentos guess
					else 
						if(comprueba(read letrapos, read difpos) == "Caso 2!")
							then putStrLn "\n\n\t\t --- He descubierto tu codigo, TE GANE!! ---\n\n"
							else
								if(comprueba(read letrapos, read difpos) == "Caso 1!")
									then intentos(head (unsafeDupablePerformIO (procesoCasoUno)))
									else
										if(comprueba(read letrapos, read difpos) == "Caso 3!")
										then intentos(unsafeDupablePerformIO (shuffleGuess guess))
										else putStrLn $ show (unsafeDupablePerformIO (arregloAleatorio(read letrapos)))
				
		
arregloAleatorio letrapos = do
		let
			array = [1,2,3,4]
			shuf = shuffle array
			ob = take letrapos (unsafeDupablePerformIO shuf) 
		return ob													

enviarAhistorialCasoUno = do			
			let letras = unsafeDupablePerformIO (readFile "letras.txt")		
			let poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]	
			let shuf = shuffle poblacion						
			let guess' = take 1 (unsafeDupablePerformIO shuf)
			return guess'
			
procesoCasoUno = do
		handle <- openFile "historial.txt" ReadMode
		contents <- hGetContents handle
		hClose handle
		if null contents
			then return (unsafeDupablePerformIO enviarAhistorialCasoUno)
			else return (unsafeDupablePerformIO enviarAhistorialCasoUno)
			
guardaHistorial :: String -> String -> String -> IO ()
guardaHistorial guess correctas incorrectas = do
			outh <- openFile "historial.txt" AppendMode			
			hPutStr outh (guess	++ " ")
			hPutStr outh (correctas ++ " ")
			hPutStrLn outh incorrectas
			hClose outh
			
shuffleGuess guess = do
		let new = shuffle guess
		return (unsafeDupablePerformIO new)
		
hr' = putStr (take 80 (cycle "*"))
br' = putStrLn " "
tab' = putStr (take 5 (cycle " "))
linea = putStrLn "\t-------------------------------------------"

comprueba :: (Int,Int) -> String
comprueba (a,b)
    | fst (a,b) == 0 && snd (a,b) == 0 = "Caso 1!"
    | fst (a,b) == 1 && snd (a,b) == 0 = "Caso x!"
	| fst (a,b) == 2 && snd (a,b) == 0 = "Caso x!"
	| fst (a,b) == 3 && snd (a,b) == 0 = "Caso x!"
	| fst (a,b) == 4 && snd (a,b) == 0 = "Caso 2!"
	| fst (a,b) == 0 && snd (a,b) == 1 = "Caso x!"
	| fst (a,b) == 1 && snd (a,b) == 1 = "Caso x!"
	| fst (a,b) == 2 && snd (a,b) == 1 = "Caso x!"
	| fst (a,b) == 3 && snd (a,b) == 1 = "Caso x!"
	| fst (a,b) == 0 && snd (a,b) == 2 = "Caso x!"
	| fst (a,b) == 1 && snd (a,b) == 2 = "Caso x!"
	| fst (a,b) == 2 && snd (a,b) == 2 = "Caso x!"
	| fst (a,b) == 0 && snd (a,b) == 3 = "Caso x!"
	| fst (a,b) == 1 && snd (a,b) == 3 = "Caso x!"
	| fst (a,b) == 0 && snd (a,b) == 4 = "Caso 3!"
    | otherwise      = "---"

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