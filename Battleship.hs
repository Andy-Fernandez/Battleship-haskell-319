import Data.Char (ord,chr)
import Data.List (permutations)


type Coordenadas = (Int, Int)
type Barco = [Coordenadas]
type Campo = [[Bool]]
type Jugador = String


tamañoCampo = 10
tamañoMinimoBarco = 2
tamañoMaximoBarco = 5


-- Seleccionar el n-ésimo elemento en una lista
seleccionar :: Int -> [a] -> a
seleccionar n xs = head (drop (n-1) (take n xs))

-- Cambiar el n-ésimo elemento en una lista
reemplazar :: Int -> [a] -> a -> [a]
reemplazar n xs x = take (n-1) xs ++ [x] ++ drop n xs

-- Inicializar el campo de 10x10
inicializarCampo :: Campo
inicializarCampo = take tamañoCampo (repeat (take tamañoCampo (repeat False)))

-- Extraer la coordenadas de la cadena
-- También convertir inmediatamente la coordenadas del rango [0,9] a [1,10] para las columnas y del rango [A,J] a [1,10]
-- Se devuelve una coordenadas inválida cuando la cadena no tiene el estilo correcto.
converitrCadenaACoordenadas :: String -> Coordenadas
converitrCadenaACoordenadas ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord 'A') + 1)
converitrCadenaACoordenadas _ = (-1, -1)

-- Dividir una cadena que contiene Coordenadas separadas por punto y coma en una lista de (sin verificar) Coordenadas.
-- Aún debes llamar a convertirCadenaACoordenadas en cada elemento de la lista devuelta.
dividirCoordenadas :: String -> [String]
dividirCoordenadas [] = [[]]
dividirCoordenadas (x:xs) = if x == ';' then
                                      [] : dividirCoordenadas xs
                                  else
                                      (x : head (dividirCoordenadas xs)) : tail (dividirCoordenadas xs)

-- Verificar si una Coordenadas está dentro del campo
validarCoordenadas :: Coordenadas -> Bool
validarCoordenadas coordenadas = and [ fst coordenadas >= 1,
                                 snd coordenadas >= 1,
                                 fst coordenadas <= tamañoCampo,
                                 snd coordenadas <= tamañoCampo
                               ]

-- Asegurarse de que el barco tenga Coordenadas válidas
validarCoordenadasBarco :: [Barco] -> Barco -> Int -> Bool
validarCoordenadasBarco barcosColocados barco longitudBarco
    | length barco /= longitudBarco = False -- Verificar si al barco se le dieron suficientes Coordenadas
    | or [coord1 == coord2 | barco2 <- barcosColocados, coord1 <- barco, coord2 <- barco2] = False -- Las Coordenadas no pueden superponerse con otro barco
    | not (and [validarCoordenadas coordenadas | coordenadas <- barco]) = False -- Verificar si las Coordenadas están en el campo
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- barco, coord2 <- barco])  -- Verificar si las Coordenadas son vecinas (vertical)
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- barco, coord2 <- barco]) * 3 == (longitudBarco-1) * (longitudBarco^2 + longitudBarco)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- barco, coord2 <- barco]) -- Verificar si las Coordenadas son vecinas (horizontal)
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- barco, coord2 <- barco]) * 3 == (longitudBarco-1) * (longitudBarco^2 + longitudBarco)
    | otherwise = False -- Las Coordenadas no están en la misma línea

-- Convertir el campo en una cadena imprimible
convertirCampoACadena :: Campo -> [Barco] -> Coordenadas -> String
convertirCampoACadena campo barcos coordenada
        | fst coordenada <= tamañoCampo
          && snd coordenada <= tamañoCampo = if seleccionar (fst coordenada) (seleccionar (snd coordenada) campo) == True then
                                               if or [coordenada == coordenadas | barco <- barcos, coordenadas <- barco] then 'o' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
                                                   else 'x' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
                                           else '.' : convertirCampoACadena campo barcos (fst coordenada + 1, snd coordenada)
                                        
        | snd coordenada <= tamañoCampo =  "\n"++ (cad ((snd coordenada)+65)) ++ convertirCampoACadena campo barcos (1, snd coordenada + 1)
        | otherwise = []

-- Obtener cadena de un char (excluyendo char 75 puesto que no queremos que se imprima la K)
cad :: Int -> String
cad fila = if fila == 75 then " " else [chr fila]

-- Imprimir el campo en la terminal
imprimirCampo :: String -> Campo -> [Barco] -> IO ()
imprimirCampo nombreJugador campo barcos = do
                                      putStrLn ("Este es el campo de "++ nombreJugador ++ ":")
                                      putStrLn (" "++ ['0'..'9'] ++ "\nA" ++ convertirCampoACadena campo barcos (1, 1))
                                      putStrLn ""

-- Marcar una celda en el campo como disparada
marcarCelda :: Campo -> Int -> Int -> Campo
marcarCelda campo x y = reemplazar x campo (reemplazar y (seleccionar x campo) True)

-- Eliminar los barcos de la lista cuando están destruidos
eliminarBarcosDestruidos :: [Barco] -> [Barco]
eliminarBarcosDestruidos [] = []
eliminarBarcosDestruidos (x:xs) | null x    = eliminarBarcosDestruidos xs
                            | otherwise = x : eliminarBarcosDestruidos xs

-- Verificar si el barco ha sido destruido y eliminarlo del juego cuando lo está
--
-- Entrada:
--    campo:      El campo en el que se encuentra el barco
--    barco:      El barco que debemos verificar la coordenadas
--    coordenadas: La coordenadas a la que se le está disparando
--
-- Salida:
--    Tupla del barco que se dio como entrada y un booleano que indica si el disparo fue un acierto o un fallo.
--    Cuando el barco se hunde, se devolverá una lista vacía en lugar del barco que se dio como entrada.
--
verificarBarcoDestruido :: Campo -> Barco -> Coordenadas -> (Barco, Bool)
verificarBarcoDestruido campo barco coordenada = if or [coordenada == coordenadas | coordenadas <- barco] == False then do
                                               (barco, False)    -- Fallo
                                           else do
                                               if and [seleccionar (fst coordenadas) (seleccionar (snd coordenadas) campo) == True | coordenadas <- barco, coordenadas /= coordenada] == False then
                                                   (barco, True) -- Acierto, pero no hundido
                                               else
                                                   ([], True)   -- Acierto y hundido


-- Disparar a una Coordenada dada
--
-- Entrada:
--    campoEnemigo: El campo de 10x10 del oponente
--    barcosEnemigos: Una lista de todos los barcos del oponente
--    Coordenada: La posición a la que estamos disparando
--
-- Salida:
--    Tupla con el campo enemigo actualizado, barcos enemigos y un booleano para indicar un acierto o fallo
--
disparar :: (Campo, [Barco]) -> Coordenadas -> (Campo, [Barco], Bool)
disparar (campoEnemigo, barcosEnemigos) coordenada = (marcarCelda campoEnemigo (snd coordenada) (fst coordenada),
                                            eliminarBarcosDestruidos [fst (verificarBarcoDestruido campoEnemigo barco coordenada) | barco <- barcosEnemigos],
                                            or [snd (verificarBarcoDestruido campoEnemigo barco coordenada) | barco <- barcosEnemigos])


-- Disparar al oponente una vez por cada barco que te queda
--
-- Entrada:
--    campoEnemigo: Campo actual del oponente
--    barcosEnemigos: La lista de todos los barcos del oponente
--    barcosRestantes: Lista de barcos que nos quedan y que todavía pueden disparar
--
-- Salida:
--    Tupla que contiene el campo y los barcos actualizados del oponente
--
dispararConCadaBarco :: (Campo, [Barco]) -> [Barco] -> IO (Campo, [Barco])
dispararConCadaBarco (campoEnemigo, barcosEnemigos) [] = return (campoEnemigo, barcosEnemigos)
dispararConCadaBarco (campoEnemigo, barcosEnemigos) barcosRestantes = do
                                                        putStrLn ("Ingresa las coordenadas para disparar (" ++ show (length barcosRestantes) ++ " tiros restantes)")
                                                        cadena <- getLine
                                                        let coordenadas = converitrCadenaACoordenadas cadena
                                                        if validarCoordenadas coordenadas then
                                                            do
                                                              let (nuevoCampoEnemigo, nuevosBarcosEnemigos, impacto) = disparar (campoEnemigo, barcosEnemigos) coordenadas

                                                              if impacto then
                                                                  putStrLn ("Disparando a las coordenadas, Impacto!")
                                                              else
                                                                  putStrLn ("Disparando a las coordenadas, Fallo")

                                                              if length nuevosBarcosEnemigos < length barcosEnemigos then
                                                                  do
                                                                    putStrLn "¡Has sumergido un barco de guerra!"
                                                                    dispararConCadaBarco (nuevoCampoEnemigo, nuevosBarcosEnemigos) (tail barcosRestantes)
                                                              else
                                                                  dispararConCadaBarco (nuevoCampoEnemigo, nuevosBarcosEnemigos) (tail barcosRestantes)
                                                        else
                                                            dispararConCadaBarco (campoEnemigo, barcosEnemigos) barcosRestantes

-- Jugar un turno
--
-- Entrada:
--    nombreJugador: Nombre del jugador que está atacando
--    suCampo: El campo del jugador atacante
--    susBarcos: Barcos del jugador atacante
--    campoEnemigo: El campo del jugador defensor
--    barcosEnemigos: Los barcos del jugador defensor
--
-- Salida:
--    Tupla que contiene el campo del jugador defensor y sus barcos después de los disparos
--
jugarTurno :: [String] -> [Campo] -> [[Barco]] -> IO ()
jugarTurno nombres campos barcos = do
                            putStrLn ("\n>>>Es el turno de " ++ head nombres)
                            imprimirCampo (last nombres) (last campos) (last barcos)
                            (nuevoCampo, nuevaListaBarcos) <- dispararConCadaBarco (last campos, last barcos) (head barcos)
                            if length nuevaListaBarcos == 0 then
                                do
                                  putStrLn ("\n>>>" ++ head nombres ++ " ganaste!\n")
                                  imprimirCampo (last nombres) nuevoCampo nuevaListaBarcos
                                  imprimirCampo (head nombres) (head campos) (head barcos)
                            else
                                jugarTurno [last nombres, head nombres] [nuevoCampo, head campos] [nuevaListaBarcos, head barcos]

-- Entrada de un barco con una longitud determinada
ingresarBarco :: [Barco] -> Int -> IO Barco
ingresarBarco barcosColocados longitud = do
                              putStrLn ("Introduce las coordenadas del barco de tamaño " ++ show longitud)
                              cadena <- getLine
                              let coordenadasCadena = dividirCoordenadas cadena
                              let coordenadas = map converitrCadenaACoordenadas coordenadasCadena
                              if validarCoordenadasBarco barcosColocados coordenadas longitud then
                                  return coordenadas
                              else
                                  ingresarBarco barcosColocados longitud

-- Entrada de todos los barcos para cada jugador
ingresarBarcos :: Int -> [Barco] -> IO [Barco]
ingresarBarcos tamañoCampo barcosColocados = if tamañoCampo <= tamañoMaximoBarco then
                                      do
                                        barco <- ingresarBarco barcosColocados tamañoCampo
                                        listaBarcos <- ingresarBarcos (tamañoCampo + 1) (barco : barcosColocados)
                                        return (barco : listaBarcos)
                                  else
                                      return []

-- Entrada de los nombres de los jugadores
ingresarNombre :: IO [String]
ingresarNombre = do
               putStrLn "Nombre del primer jugador:"
               nombre1 <- getLine
               putStrLn "Nombre del segundo jugador: "
               nombre2 <- getLine
               return [nombre1, nombre2]

-- El punto de entrada del programa.
main :: IO ()
main = do
         nombres <- ingresarNombre

         putStrLn (">>> " ++ head nombres ++ ", es tu turno de colocar los barcos")
         barcosJugador1 <- ingresarBarcos tamañoMinimoBarco []

         putStrLn (">>>" ++ last nombres ++ ", es tu turno de colocar los barcos")
         barcosJugador2 <- ingresarBarcos tamañoMinimoBarco []

         jugarTurno nombres [inicializarCampo, inicializarCampo] [barcosJugador1, barcosJugador2]

-- Ejemplo de uso con un campo y barcos definidos
main2 :: IO ()
main2 = do
    let nombreJugador = "Jugador1"
    let campo = inicializarCampo
    let barcos = [[(0,0),(0,1)],[(6,7),(7,7),(8,7)],[(3,3),(3,4),(3,5),(3,6)],[(5,9),(6,9),(7,9),(8,9),(9,9)]]
    --let barcos = [[(1,1),(1,2)], [(3,4),(3,5),(3,6)], [(6,6),(6,7),(6,8),(6,9)]]
    imprimirCampo nombreJugador campo barcos