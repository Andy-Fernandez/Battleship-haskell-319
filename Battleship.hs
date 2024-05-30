import Data.Char (ord)
import Data.List (permutations)

-- Definiciones de tipos
type Coordenada = (Int, Int)
type Barco = [Coordenada]
type Campo = [[Bool]]
type Jugador = String

tamanoCampo = 10
tamanoMinBarco = 2
tamanoMaxBarco = 5

-- Seleccionar el n-ésimo elemento de una lista
-- En el juego esto se usa para seleccionar una fila y luego una columna
seleccionar :: Int -> [a] -> a
seleccionar n xs = head (drop (n-1) (take n xs))

-- Cambiar el n-ésimo elemento de una lista
-- En el juego esto se usa para cambiar una fila y luego una columna
reemplazar :: Int -> [a] -> a -> [a]
reemplazar n xs x = take (n-1) xs ++ [x] ++ drop n xs

-- Inicializar el campo de 10x10
-- Un campo es una lista de listas de booleanos que representan si una celda ha sido disparada o no
inicializarCampo :: Campo
inicializarCampo = take tamanoCampo (repeat (take tamanoCampo (repeat False)))

-- Extraer la coordenada de una cadena
-- También convierte inmediatamente la coordenada del rango [0,10[ a [1,10]
-- Se devuelve una coordenada inválida cuando la cadena no tiene el estilo correcto.
-- Por ejemplo, convertirCadenaACoordenadas "(1,2)" = (1,2)
convertirCadenaACoordenadas :: String -> Coordenada
convertirCadenaACoordenadas ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertirCadenaACoordenadas _ = (-1, -1)

-- Dividir una cadena que contiene coordenadas separadas por punto y coma en una lista de coordenadas (sin validar).
-- Aún debes llamar a convertirCadenaACoordenadas en cada elemento de la lista devuelta.
-- Por ejemplo, dividirCoordenadasEnCadena "(1,2);(3,4);(5,6)" = ["(1,2)", "(3,4)", "(5,6)"]
dividirCoordenadasEnCadena :: String -> [String]
dividirCoordenadasEnCadena [] = [[]]
dividirCoordenadasEnCadena (x:xs) = if x == ';' then
                                       [] : dividirCoordenadasEnCadena xs
                                   else
                                       (x : head (dividirCoordenadasEnCadena xs)) : tail (dividirCoordenadasEnCadena xs)

-- Verificar si una coordenada se encuentra dentro del campo
-- Las coordenadas válidas están en el rango [1,10]
validarCoordenada :: Coordenada -> Bool
validarCoordenada coord = and [ fst coord >= 1,
                                snd coord >= 1,
                                fst coord <= tamanoCampo,
                                snd coord <= tamanoCampo
                              ]

-- Asegurarse de que el barco tiene coordenadas válidas
validarCoordenadasBarco :: [Barco] -> Barco -> Int -> Bool
validarCoordenadasBarco barcosColocados barco longitudBarco
    | length barco /= longitudBarco = False -- Verificar si el barco tiene suficientes coordenadas
    | or [coord1 == coord2 | barco2 <- barcosColocados, coord1 <- barco, coord2 <- barco2] = False -- Las coordenadas no deben superponerse con otro barco
    | not (and [validarCoordenada coord | coord <- barco]) = False -- Verificar si las coordenadas están dentro del campo
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- barco, coord2 <- barco]) -- Verificar si las coordenadas son vecinas (vertical)
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- barco, coord2 <- barco]) * 3 == (longitudBarco-1) * (longitudBarco^2 + longitudBarco)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- barco, coord2 <- barco]) -- Verificar si las coordenadas son vecinas (horizontal)
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- barco, coord2 <- barco]) * 3 == (longitudBarco-1) * (longitudBarco^2 + longitudBarco)
    | otherwise = False -- Las coordenadas no están en la misma línea

-- Convertir el campo en una cadena imprimible
convertirCampoACadena :: Campo -> [Barco] -> Coordenada -> String
convertirCampoACadena campo barcos coordenada
    | fst coordenada <= tamanoCampo
      && snd coordenada <= tamanoCampo = if seleccionar (fst coordenada) (seleccionar (snd coordenada) campo) == True then
                                             if or [coordenada == coord | barco <- barcos, coord <- barco] then 'o' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
                                                 else 'x' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
                                         else ' ' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
    | snd coordenada <= tamanoCampo = "\n" ++ convertirCampoACadena campo barcos (1, snd coordenada + 1)
    | otherwise = []

-- Mostrar el campo en el terminal
imprimirCampo :: String -> Campo -> [Barco] -> IO ()
imprimirCampo nombreJugador campo barcos = do
                                             putStrLn (nombreJugador ++ "'s field:")
                                             putStrLn (replicate (tamanoCampo+2) 'H' ++ "\nH" ++ convertirCampoACadena campo barcos (1, 1) ++ replicate (tamanoCampo+1) 'H')
                                             putStrLn ""

-- Marcar una celda en el campo como disparada
marcarDisparo :: Campo -> Int -> Int -> Campo
marcarDisparo campo x y = reemplazar x campo (reemplazar y (seleccionar x campo) True)

-- Eliminar los barcos de la lista cuando son destruidos
eliminarBarcosDestruidos :: [Barco] -> [Barco]
eliminarBarcosDestruidos [] = []
eliminarBarcosDestruidos (x:xs) | null x    = eliminarBarcosDestruidos xs
                                | otherwise = x : eliminarBarcosDestruidos xs

-- Verificar si el barco ha sido destruido y eliminarlo del juego cuando lo esté
--
-- Entrada:
--    campo:      El campo en el que se encuentra el barco
--    barco:      El barco que debemos verificar con la coordenada
--    coordenada: La coordenada a la que se está disparando
--
-- Salida:
--    Tupla del barco que se dio como entrada y un booleano que indica si el disparo fue un acierto o fallo.
--    Cuando el barco se hunde, se devuelve una lista vacía en lugar del barco que se dio como entrada.
--
verificarBarcoDestruido :: Campo -> Barco -> Coordenada -> (Barco, Bool)
verificarBarcoDestruido campo barco coordenada = if or [coordenada == coord | coord <- barco] == False then
                                                     (barco, False)    -- Fallo
                                                 else
                                                     if and [seleccionar (fst coord) (seleccionar (snd coord) campo) == True | coord <- barco, coord /= coordenada] == False then
                                                         (barco, True) -- Acierto, pero no hundido
                                                     else
                                                         ([], True)   -- Acierto y hundido



-- Disparar a una coordenada dada
--
-- Entrada:
--    campoEnemigo: El campo 10x10 del oponente
--    barcosEnemigos: Una lista de todos los barcos del oponente
--    coordenada: La posición a la que estamos disparando
--
-- Salida:
--    Tupla con el campoEnemigo actualizado, los barcosEnemigos y un booleano que indica un acierto o fallo
--
disparar :: (Campo, [Barco]) -> Coordenada -> (Campo, [Barco], Bool)
disparar (campoEnemigo, barcosEnemigos) coordenada = (marcarDisparo campoEnemigo (snd coordenada) (fst coordenada),
                                                      eliminarBarcosDestruidos [fst (verificarBarcoDestruido campoEnemigo barco coordenada) | barco <- barcosEnemigos],
                                                      or [snd (verificarBarcoDestruido campoEnemigo barco coordenada) | barco <- barcosEnemigos])

-- Disparar al oponente una vez por cada barco que te queda
--
-- Entrada:
--    campoEnemigo: Campo actual del oponente
--    barcosEnemigos: La lista de todos los barcos del oponente
--    nuestrosBarcos: Lista de barcos que nos quedan y que aún pueden disparar
--
-- Salida:
--    Tupla que contiene el campo y los barcos actualizados del oponente
--
dispararConCadaBarco :: (Campo, [Barco]) -> [Barco] -> IO (Campo, [Barco])
dispararConCadaBarco (campoEnemigo, barcosEnemigos) [] = return (campoEnemigo, barcosEnemigos)
dispararConCadaBarco (campoEnemigo, barcosEnemigos) nuestrosBarcos = do
    putStrLn ("Ingrese las coordenadas para disparar (" ++ show (length nuestrosBarcos) ++ " disparos restantes)")
    string <- getLine
    let coord = convertirCadenaACoordenadas string
    if validarCoordenada coord then
        do
            let (nuevoCampoEnemigo, nuevosBarcosEnemigos, acierto) = disparar (campoEnemigo, barcosEnemigos) coord

            if acierto then
                putStrLn ("Disparando a la coordenada (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Acierto")
            else
                putStrLn ("Disparando a la coordenada (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Fallo")

            if length nuevosBarcosEnemigos < length barcosEnemigos then
                do
                    putStrLn "¡Hundiste mi acorazado!"
                    dispararConCadaBarco (nuevoCampoEnemigo, nuevosBarcosEnemigos) (tail nuestrosBarcos)
            else
                dispararConCadaBarco (nuevoCampoEnemigo, nuevosBarcosEnemigos) (tail nuestrosBarcos)
    else do
        putStrLn "Coordenadas inválidas, por favor intente de nuevo."
        dispararConCadaBarco (campoEnemigo, barcosEnemigos) nuestrosBarcos

-- Jugar el juego, un turno a la vez
--
-- Entrada:
--    nombres: Lista de nombres de los jugadores
--    campos: Lista de campos pertenecientes a los jugadores
--    barcos: Lista de barcos pertenecientes al jugador
--
-- El primer elemento en las listas, es del jugador cuyo turno es actualmente
--
jugar :: [String] -> [Campo] -> [[Barco]] -> IO ()
jugar nombres campos barcos = do
                                putStrLn ("\nTurno de " ++ head nombres)
                                imprimirCampo (last nombres) (last campos) (last barcos)
                                (nuevoCampo, nuevaListaBarcos) <- dispararConCadaBarco (last campos, last barcos) (head barcos)
                                if length nuevaListaBarcos == 0 then
                                    do
                                      putStrLn ("\n" ++ head nombres ++ " ganó!\n")
                                      imprimirCampo (last nombres) nuevoCampo nuevaListaBarcos
                                      imprimirCampo (head nombres) (head campos) (head barcos)
                                else
                                    jugar [last nombres, head nombres] [nuevoCampo, head campos] [nuevaListaBarcos, head barcos]

-- Ingresar un barco con una longitud dada
ingresarBarco :: [Barco] -> Int -> IO Barco
ingresarBarco barcosColocados len = do
                                      putStrLn ("Ingrese las coordenadas del barco de longitud " ++ show len ++ ":")
                                      string <- getLine
                                      let stringCoords = dividirCoordenadasEnCadena string
                                      let coords = map convertirCadenaACoordenadas stringCoords
                                      if validarCoordenadasBarco barcosColocados coords len then
                                          return coords
                                      else
                                          ingresarBarco barcosColocados len

-- Ingresar todos los barcos para un jugador
ingresarBarcos :: Int -> [Barco] -> IO [Barco]
ingresarBarcos tamanoBarco barcosColocados = if tamanoBarco <= tamanoMaxBarco then
                                                 do
                                                   barco <- ingresarBarco barcosColocados tamanoBarco
                                                   listaBarcos <- ingresarBarcos (tamanoBarco + 1) (barco : barcosColocados)
                                                   return (barco : listaBarcos)
                                             else
                                                 return []

-- Ingresar los nombres de los jugadores
ingresarNombres :: IO [String]
ingresarNombres = do
                    putStrLn "¿Cuál es el nombre del primer jugador?"
                    nombre1 <- getLine
                    putStrLn "¿Cuál es el nombre del segundo jugador?"
                    nombre2 <- getLine
                    return [nombre1, nombre2]

-- Punto de entrada del programa
main :: IO ()
main = do
         nombres <- ingresarNombres

         putStrLn (head nombres ++ ", ingrese sus barcos")
         barcosJugador1 <- ingresarBarcos tamanoMinBarco []

         putStrLn (last nombres ++ ", ingrese sus barcos")
         barcosJugador2 <- ingresarBarcos tamanoMinBarco []

         jugar nombres [inicializarCampo, inicializarCampo] [barcosJugador1, barcosJugador2]


