import System.Random
import System.Time
import System.IO
import Control.Exception
import Data.Char
import Data.Array.IO
import Control.Monad
import System.IO.Unsafe
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Data.List


mastermind = do				
		guardarLetras
		let letras = unsafeDupablePerformIO (readFile "letras.txt")		
		let poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]	
		num <- getStdGen
		let shuf = randPerm num poblacion		
		let guess = take 1 (shuf)	
		hr'
		putStr (take 35 (cycle " "))
		putStr ("MASTERMIND")
		putStr (take 35 (cycle " "))
		hr'		
		putStr "\n\tNombre de jugador: "
		nombre <- getLine		
		putStrLn ("\tBienvenid@ " ++ nombre ++ "!\n\n")		
		intentos 1 (head guess) 		                      		

intentos :: Int -> String -> IO ()
intentos numero guess = do
		let historial=[]
		putStr "\n\t ---> Mi adivinanza es: "
		putStrLn guess
		putStrLn "\n\n\n\t-[ Retroalimentacion ]-"
		linea
		putStr "\tCuantas letras estan en la posicion correcta? "
		letrapos <- getLine
		putStr "\tCuantas letras estan en la posicion incorrecta? "
		difpos <- getLine		
		if(numero == 15)
			then putStrLn "\n\n\t\tLo siento, no pude conseguir el codigo... (u_u)\n\t\t\t --->TU GANAS!\n\n"
			else
				if(comprueba(read letrapos, read difpos) == "Caso x!")
					then do
						guardaHistorial guess letrapos difpos
						let array = arregloAleatorio (read letrapos)
						if (length (array) == 1)
							then do
								let guess' = shuffleGuess guess 
								let guess1 = guess' !! 0
								intentos (succ numero) (head (guess1 : ["ABC"]))
							else putStr "valimos"
					else
						if(comprueba(read letrapos, read difpos) == "---")
							then do
								guardaHistorial guess letrapos difpos
								intentos (succ numero) guess
							else 
								if(comprueba(read letrapos, read difpos) == "Caso 2!")
									then do
										putStrLn "\n\n\t\t --- He descubierto tu codigo, TE GANE!! ---\n\n"
										guardaHistorial guess letrapos difpos
									else
										if(comprueba(read letrapos, read difpos) == "Caso 1!")
											then do
												guardaHistorial guess letrapos difpos
												intentos (succ numero) ( head( unsafeDupablePerformIO( procesoCasoUno)))
											else
												if(comprueba(read letrapos, read difpos) == "Caso 3!")
												then do
													guardaHistorial guess letrapos difpos
													intentos (succ numero) (unsafeDupablePerformIO (shuffleGuess guess))
												else do
													putStrLn $ show (unsafeDupablePerformIO (arregloAleatorio(read letrapos)))
													guardaHistorial guess letrapos difpos
				
		
arregloAleatorio letrapos = do
		let
			array = [1,2,3,4]
			shuf = shuffle array
			ob = take letrapos (unsafeDupablePerformIO shuf) 
		return ob													

enviarAhistorialCasoUno = do			
			let letras = unsafeDupablePerformIO (readFile "letras.txt")		
			let poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]	
			newStdGen
			n <- getStdGen
			let shuf = randPerm n poblacion
			let guess' = take 1 shuf
			return guess'
			
procesoCasoUno = do
		handle <- openFile "historial.txt" ReadMode
		contents <- hGetContents handle
		hClose handle
		return (unsafeDupablePerformIO enviarAhistorialCasoUno)			
			
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
		
randPerm :: StdGen -> [a] -> [a]
randPerm _ []   = []
randPerm gen xs = let (n,newGen) = randomR (0,length xs -1) gen
                      front = xs !! n
                  in  front : randPerm newGen (take n xs ++ drop (n+1) xs)
	
guardarLetras = do    
		outh <- openFile "letras.txt" WriteMode
		let letters = ["ABCDEF"]
		hPutStr outh (head letters)
		hClose outh
		
listaDeHistorial = do
	handle <- openFile "historial.txt" ReadMode
	contents <- hGetContents handle
	hClose handle
	return (unsafeDupablePerformIO (formarListaGeneral (lines contents) []))

tamanioHistorial = length (unsafeDupablePerformIO(listaDeHistorial))
	
conseguirGuess string vacia = do
						let lista = vacia ++ take 1 string
						if length lista < 4 
							then conseguirGuess (drop 1 string) lista																
							else return lista

conseguirLetrapos string vacia = do
						let lista = vacia ++ take 1 string
						if length lista < 6 
							then conseguirLetrapos (drop 1 string) lista																
							else return (last lista)
							
conseguirDifpos string vacia = do
						let lista = vacia ++ take 1 string
						if length lista < 8 
							then conseguirDifpos (drop 1 string) lista																
							else return (last lista)
							
formarListaGeneral listaStrings nuevaLista = do			
			if null listaStrings
				then return nuevaLista
				else do
					let
						string = head listaStrings
						guess = unsafeDupablePerformIO (conseguirGuess string [])
						letrapos = unsafeDupablePerformIO (conseguirLetrapos string [])
						difpos = unsafeDupablePerformIO (conseguirDifpos string [])						
						tupla = (digitToInt letrapos, digitToInt difpos)
						elemento = (guess,tupla)
						nuevaNuevaLista = elemento : nuevaLista
					print nuevaNuevaLista
					formarListaGeneral (tail listaStrings) nuevaNuevaLista
					
mejorCaso (a,b)
		| fst (a,b) == 0 && snd (a,b) == 0 = "0"
		| fst (a,b) == 0 && snd (a,b) == 1 = "1"
		| fst (a,b) == 1 && snd (a,b) == 0 = "1"
		| fst (a,b) == 0 && snd (a,b) == 2 = "2"
		| fst (a,b) == 1 && snd (a,b) == 1 = "2"
		| fst (a,b) == 2 && snd (a,b) == 0 = "2"
		| fst (a,b) == 0 && snd (a,b) == 3 = "3"
		| fst (a,b) == 1 && snd (a,b) == 2 = "3"
		| fst (a,b) == 2 && snd (a,b) == 1 = "3"
		| fst (a,b) == 3 && snd (a,b) == 0 = "3"
		| fst (a,b) == 0 && snd (a,b) == 4 = "4"
		| fst (a,b) == 1 && snd (a,b) == 3 = "4"
		| fst (a,b) == 2 && snd (a,b) == 2 = "4"
		| fst (a,b) == 3 && snd (a,b) == 1 = "4"
		| otherwise						   = "Some dfault"

mejorGuess guess (letrapos, difpos) = do
		let caso = mejorCaso (letrapos, difpos)
		if caso == "4"
			then return guess
			else mejorGuess guess (letrapos, difpos)