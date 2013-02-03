import Data.Char

mastermind = do
                let
                        letras = ['A','B','C','D','E','F']
                        poblacion = [[w,x,y,z]|w<-letras,x<-letras,y<-letras,z<-letras]
                print poblacion
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
    
    
  
                        