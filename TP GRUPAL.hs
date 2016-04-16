data Archivo = Archivo { nombre :: String, contenido :: String } deriving (Read, Show, Eq)

archivoPrueba :: Archivo
archivoPrueba = Archivo "Ejemplo" "Trabajo practico de Paradigmas\n Nombre del TP HIT \n \t \f \n otralinea" 

isSpace caracter = (==) caracter " " || (==) caracter "\n" || (==) caracter "\t" || (==) caracter "\r" || (==) caracter "\f" || (==) caracter "\v"

--Punto 1)
dameTamañoDelArchivo :: Archivo -> Int
dameTamañoDelArchivo (Archivo _ contenido) =  (*8)(length (contenido))

--Punto2)
archivoEsVacio = (==0).dameTamañoDelArchivo

--Punto3) 
--Para saber la cantidad de lineas de un archio usamos lines que cuando ve 
-- un \n salta de linea, pone en una lista
cantidadDeLineasArch (Archivo _ contenido) = length (lines (contenido)) 

--punto4)
algunaLineaBlanco (Archivo _ contenido) = any (==0)(map length(map words(lines  contenido)))



