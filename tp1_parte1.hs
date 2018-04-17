type Memoria = [Int]
type  AcumuladorA = Int
type  AcumuladorB = Int
type Pc = Int
type  EtiquetaError = String
data Microprocesador = Microprocesador Memoria AcumuladorA AcumuladorB Pc EtiquetaError deriving (Show) 
--punto 3.1
xt8088 = Microprocesador [] 0 0 0 " "
fp20 = Microprocesador [] 7 24 0 " "
at8086 = Microprocesador [1..20] 0 0 0 " "
-----
nop :: Microprocesador -> Microprocesador
nop (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) = Microprocesador memoria acumuladorA acumuladorB (pc + 1) etiquetaError 

add :: Microprocesador -> Microprocesador
add (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) = Microprocesador memoria (acumuladorA + acumuladorB)  0 (pc + 1) etiquetaError

divide :: Microprocesador -> Microprocesador
divide (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) | acumuladorA < acumuladorB = Microprocesador memoria acumuladorA acumuladorB (pc + 1) "no se puede usar div"
                                                                          | otherwise = Microprocesador memoria (div acumuladorA acumuladorB) 0 (pc + 1) etiquetaError
swap :: Microprocesador -> Microprocesador
swap (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) = Microprocesador memoria acumuladorB acumuladorA (pc + 1) etiquetaError

lodv :: Microprocesador -> Int -> Microprocesador
lodv (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val = Microprocesador memoria val acumuladorB (pc + 1) etiquetaError

-- punto 3.2 
--nop((nop.nop) xt8088)

--punto 3.3
programaSumar :: Microprocesador -> Int -> Int -> Microprocesador
programaSumar (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val1 val2 = add (lodv (swap (lodv (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val1)) val2)
--parte1 :: Microprocesador -> Int -> Microprocesador
--parte1 (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val1 = (swap.lodv) (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val1
--parte2 :: Microprocesador -> Int -> Microprocesador
--parte2 (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val2 = (add.lodv) (Microprocesador memoria acumuladorA acumuladorB pc etiquetaError) val2


--para test unitarios
programCounter :: Microprocesador -> Pc
programCounter (Microprocesador _ _ _ pc _)= pc 

acumuladorA :: Microprocesador -> AcumuladorA 
acumuladorA (Microprocesador _ acumulA _ _ _)= acumulA

acumuladorB :: Microprocesador -> AcumuladorB
acumuladorB (Microprocesador _ _ acumlB _ _)= acumlB 

memoria :: Microprocesador -> Memoria
memoria (Microprocesador memoria _ _ _ _) = memoria

mensajeError :: Microprocesador  -> EtiquetaError
mensajeError (Microprocesador _ _ _ _ etiquetaError) = etiquetaError
