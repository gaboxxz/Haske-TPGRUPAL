data Archivo = Archivo { nombre :: String, contenido :: String } deriving (Read, Show, Eq)

archivoPrueba1 :: Archivo
archivoPrueba1 = Archivo "Ejemplo" "Trabajo practico de hasdbasbjkdsbksadjbsadsadsadsadsadsadsadsadsadsdasadsdasad Paradigmas\n\t \f \r \v \nNombre del TP HIT" 

archivoPrueba2 :: Archivo
archivoPrueba2 = Archivo "Ejemplo.hs" "" --Archivo vacio

archivoPrueba3 :: Archivo
archivoPrueba3 = Archivo "Ejemplo3.hs" "TP: Paradigmas\nEste texto esta en la segunda linea \nParadigmas"

archivoPrueba4 :: Archivo
archivoPrueba4 = Archivo "Ejemplo4.hs" "TP: Paradigmas\nEste texto esta en la segunda linea \nParadigmas"

isSpace caracter = (==) caracter ' ' || (==) caracter '\n' || (==) caracter '\t' || (==) caracter '\r' || (==) caracter '\f' || (==) caracter '\v'

--OPERACIONES BASICAS SOBRE ARCHIVOS

dameNombre :: Archivo -> String
dameContenido :: Archivo -> String 

dameContenido (Archivo _ contenido) = contenido
dameNombre (Archivo nombre _) = nombre

--Punto 1)

dameTamañoDelArchivo :: Archivo -> Int
dameTamañoDelArchivo archivo =  ((*8).length.dameContenido) archivo

dameTamañoDelArchivoPrueba1 = dameTamañoDelArchivo archivoPrueba1
dameTamañoDelArchivoPrueba2 = dameTamañoDelArchivo archivoPrueba2

--Punto2)

archivoVacio :: Archivo -> Bool
archivoVacio =  (==0).dameTamañoDelArchivo

archivoVacioPrueba1 = archivoVacio archivoPrueba1
archivoVacioPrueba2 = archivoVacio archivoPrueba2

--Punto3) 

dameCantidadDeLineas :: Archivo -> Int
dameCantidadDeLineas (Archivo _ contenido) = (length.(filter (=='\n'))) contenido

dameCantidadDeLineasPrueba1 = dameCantidadDeLineas archivoPrueba1
dameCantidadDeLineasPrueba2 = dameCantidadDeLineas archivoPrueba2

--Punto4)

algunaLineaBlanca :: Archivo -> Bool
algunaLineaBlanca =  ((any (all isSpace)).lines.dameContenido)

algunaLineaBlancaPrueba1 = algunaLineaBlanca archivoPrueba1
algunaLineaBlancaPrueba2 = algunaLineaBlanca archivoPrueba2

--Punto5)

esDeExtencionHS :: Archivo -> Bool
esDeExtencionHS = (=="sh.").darVueltaYTomarUltimosTres.dameNombre

darVueltaYTomarUltimosTres = (take 3).reverse

esDeExtencionHSPrueba1 = esDeExtencionHS archivoPrueba1
esDeExtencionHSPrueba2 = esDeExtencionHS archivoPrueba2

--MODIFICACIONES SOBRE ARCHIVOS

--Punto6) 

renombrarArchivo :: String -> Archivo -> Archivo
renombrarArchivo nombreNuevo (Archivo _ contenido ) = (Archivo nombreNuevo contenido)

renombrarArchivoPrueba1 = renombrarArchivo "NOMBRE NUEVO" archivoPrueba1

--Punto7)

agregarNuevaLinea :: Int -> String -> Archivo -> Archivo
lineasAntesDe :: Int -> String -> String
lineasDespuesDe :: Int -> String -> String

agregarNuevaLinea numeroDeLinea contenidoAAgregar (Archivo nombre contenido) = (Archivo nombre ((lineasAntesDe numeroDeLinea contenido) ++ contenidoAAgregar ++ "\n" ++ (lineasDespuesDe numeroDeLinea contenido)))

lineasAntesDe numero = unlines.(take numero).lines
lineasDespuesDe numero =  unlines.(drop numero).lines 
 
agregarNuevaLineaPrueba1 = agregarNuevaLinea 2 "SOY UNA NUEVA LINEA" archivoPrueba1
agregarNuevaLineaPrueba2 = agregarNuevaLinea 1 "SOY UNA NUEVA LINEA" archivoPrueba2

--convertirAUnSoloString listaDeStrings = foldl1 (\ palabra1 palabra2 -> palabra1 ++ " " ++ palabra2) listaDeStrings 

--Punto8)

quitarUnaLinea :: Int -> Archivo -> Archivo
quitarUnaLinea numeroDeLinea (Archivo nombre contenido) = (Archivo nombre (((lineasAntesDe (numeroDeLinea-1) contenido)) ++ (lineasDespuesDe numeroDeLinea contenido))) 

quitarUnaLineaPrueba1 = quitarUnaLinea 1 archivoPrueba1
quitarUnaLineaPrueba2 = quitarUnaLinea 5 archivoPrueba2

--Punto9) 

reemplazarUnaLinea :: Int -> String -> Archivo -> Archivo
reemplazarUnaLinea  numeroDeLinea  contenidoAAgregar (Archivo nombre contenido) = (Archivo nombre (((lineasAntesDe (numeroDeLinea-1) contenido)) ++ contenidoAAgregar ++ "\n" ++ (lineasDespuesDe numeroDeLinea contenido))) 

reemplazarUnaLineaPrueba1 = reemplazarUnaLinea 2 "NUEVA LINEA" archivoPrueba1
reemplazarUnaLineaPrueba2 = reemplazarUnaLinea 3 "NUEVA LINEA" archivoPrueba2

--Punto10)

buscarYReemplazar :: String -> String -> Archivo -> Archivo
buscarYReemplazar palabraAReemplazar palabraNueva (Archivo nombre contenido) = (Archivo nombre (unlines (map unwords (map (map (reemplazarPalabra palabraNueva palabraAReemplazar)) (map words (lines contenido))))))

reemplazarPalabra palabraNueva palabraAReemplazar palabraActual 
 | palabraAReemplazar == palabraActual = palabraNueva
 | otherwise = palabraActual
 
buscarYReemplazarPrueba1 = buscarYReemplazar "Paradigmas" "NUEVAPALABRA" archivoPrueba1
buscarYReemplazarPrueba2 = buscarYReemplazar "Paradigmas" "NUEVAPALABRA" archivoPrueba2
buscarYReemplazarPrueba3 = buscarYReemplazar "Paradigmas" "NUEVAPALABRA" archivoPrueba3
 
--Punto11)

wrappearLineas :: Archivo -> Archivo		 
wrappearLineas (Archivo nombre contenido) = Archivo nombre (unlines (map (funcionPrueba) (lines contenido)))	
funcionPrueba linea 
 |length linea <= 80 = linea
 |otherwise = (take 80 linea) ++ "\n" ++ (funcionPrueba (drop 80 linea))
 
wrappearLineasPrueba1 = wrappearLineas archivoPrueba1
wrappearLineasPrueba2 = wrappearLineas archivoPrueba2
wrappearLineasPrueba3 = wrappearLineas archivoPrueba3

--Punto12)

modificacionInutil :: (Archivo -> Archivo) -> Archivo -> Bool

modificacionInutil funcionModificacion (Archivo nombre contenido) = dameNombre (funcionModificacion (Archivo nombre contenido)) == nombre && words (dameContenido (funcionModificacion (Archivo nombre contenido))) == words contenido

modificacionInutilPrueba1 = modificacionInutil (renombrarArchivo "NUEVO NOMBRE") archivoPrueba1
modificacionInutilPrueba2 = modificacionInutil (buscarYReemplazar "PEPE" "PALABRANUEVA") archivoPrueba1

-- REVISIONES DE UN ARCHIVO

--Punto13)

-- REVISIONES DE UN ARCHIVO

revision :: [(Archivo -> Archivo)] -> Archivo -> Archivo
revision listaDeFunciones archivo = (foldl1 (.) listaDeFunciones) archivo

revisionPrueba1 = revision [(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA")] 
 
revisionPrueba2 = revision [(quitarUnaLinea 2)]


--REVISION AL DIRECTORIO
revisionAlDirectorio :: [(Archivo, Archivo -> Archivo )] -> [Archivo]
revisionAlDirectorio revisionesAArchivos =  map (foldl1 (.) (map snd revisionesAArchivos)) (map fst revisionesAArchivos) -- recibe [(archivo1, revision1),(archivo2, revision2)]
 
--Punto14)

--[(Archivo, [Funciones])] = lista
--(listaDeFunciones, archivo)
archivoMasGrandeDirectorio :: [([Archivo -> Archivo], Archivo)] -> Archivo
auxiliarAplicarFuncionesAUnArchivo :: ([Archivo -> Archivo], Archivo) -> Archivo
compararYElegirAlMayor :: Int -> [Archivo] -> Archivo
aplicarFunciones :: [([Archivo -> Archivo], Archivo)] -> [Archivo]

archivoMasGrandeDirectorio lista = compararYElegirAlMayor(maximum (map dameTamañoDelArchivo (aplicarFunciones lista))) (aplicarFunciones lista)

auxiliarAplicarFuncionesAUnArchivo (listaDeFunciones, archivo)	
 | length listaDeFunciones /= 0 = auxiliarAplicarFuncionesAUnArchivo ((tail (fst (listaDeFunciones, archivo))), (head (fst (listaDeFunciones, archivo)) (snd (listaDeFunciones, archivo))))
 | otherwise = snd (listaDeFunciones, archivo)
 -- recibe una lista de tuplas (listaDeFunciones, Archivo)

compararYElegirAlMayor numero lista
 | dameTamañoDelArchivo (head lista) == numero = (head lista)
 | otherwise = compararYElegirAlMayor numero (tail lista)

aplicarFunciones lista =  map auxiliarAplicarFuncionesAUnArchivo lista

archivoMasGrandeDirectorioPrueba1 = archivoMasGrandeDirectorio [([(buscarYReemplazar "Paradigmas" "HOLA")], archivoPrueba1 ) , ([(agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2")] , archivoPrueba2)] 
archivoMasGrandeDirectorioPrueba2 = archivoMasGrandeDirectorio [([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 3")], archivoPrueba3) , ([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 4")] , archivoPrueba4)] 

 --Punto15)
 
mayorCambio :: [([Archivo -> Archivo], Archivo)] -> String
compararYElegirMayorCambio :: Int -> [([Archivo -> Archivo], Archivo)] -> String
sacarMayorCambio :: [([Archivo -> Archivo], Archivo)] -> Int
listaDeValoresAbsolutos :: [([Archivo -> Archivo], Archivo)] -> [Int]
 
mayorCambio lista =  compararYElegirMayorCambio (sacarMayorCambio lista) lista

compararYElegirMayorCambio numero lista
 | numero == (head (listaDeValoresAbsolutos lista)) = dameNombre (snd (head lista))
 | otherwise = compararYElegirMayorCambio numero (tail lista)


sacarMayorCambio lista = maximum (listaDeValoresAbsolutos lista)

listaDeValoresAbsolutos lista = map abs (zipWith (-) (map dameTamañoDelArchivo (aplicarFunciones lista)) (map dameTamañoDelArchivo (map snd lista)))

mayorCambioPrueba1 = mayorCambio [([(buscarYReemplazar "Paradigmas" "HOLA")], archivoPrueba1 ) , ([(agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2")] , archivoPrueba2)] 
mayorCambioPrueba2 = mayorCambio [([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 3")], archivoPrueba3) , ([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 4")] , archivoPrueba4)]

--Punto16)

serieRevisionesDirectorio :: [([(Archivo -> Archivo)] , Archivo)] -> [Archivo]
serieRevisionesDirectorio lista = zipWith (\ x y -> x y) (map (foldl1 (.)) (map fst lista)) (map snd lista)

serieRevisionesDirectorioPrueba1 = serieRevisionesDirectorio [([(buscarYReemplazar "Paradigmas" "HOLA")], archivoPrueba1 ) , ([(agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2")] , archivoPrueba2)]
serieRevisionesDirectorioPrueba2 = serieRevisionesDirectorio [([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 3")], archivoPrueba3) , ([(buscarYReemplazar "Paradigmas" "HOLA") , (agregarNuevaLinea 2 "SOY UNA NUEVA LINEA DEL ARCHIVO 4")] , archivoPrueba4)]


nuevaSerieRevisionesDirectorio :: [Archivo] -> [[Archivo -> Archivo]] -> [Archivo]
nuevaSerieRevisionesDirectorio listaDeArchivos listaDeModificaciones = zipWith (\ a m -> m a) listaDeArchivos (map (foldl1 (.)) listaDeModificaciones)

nuevaSerieRevisionesDirectorioPrueba1 = nuevaSerieRevisionesDirectorio [archivoPrueba1, archivoPrueba2] [[buscarYReemplazar "Paradigmas" "HOLA", agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2"], [agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2", buscarYReemplazar "Paradigmas" "HOLA"] ]
nuevaSerieRevisionesDirectorioPrueba2 = nuevaSerieRevisionesDirectorio [archivoPrueba3, archivoPrueba4] [[buscarYReemplazar "Paradigmas" "HOLA", agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2"], [agregarNuevaLinea 5 "SOY UNA NUEVA LINEA DEL ARCHIVO 2", buscarYReemplazar "Paradigmas" "HOLA"] ]


--serieRevisionesDirectorio recibe por ejemplo una lista asi [(listaDeRevisiones1 , archivo1),(listaDeRevisiones2, archivo2),...,(listaDeRevisionesN, archivoN)]

--zipWithFuncionesValores :: ((a -> e) -> b -> c) -> [(a->e)] -> [b] -> c

--funcionParaAplicarleAZipWith funcion valor = funcion valor





