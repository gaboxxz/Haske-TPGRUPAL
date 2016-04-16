data Archivo = Archivo { nombre :: String, contenido :: String } deriving (Read, Show, Eq)

archivoPrueba :: Archivo
archivoPrueba = Archivo "Ejemplo" "Trabajo practico de Paradigmas\n Nombre del TP HIT" 

isSpace caracter = (==) caracter ' ' || (==) caracter '\n' || (==) caracter '\t' || (==) caracter '\r' || (==) caracter '\f' || (==) caracter '\v'

--Punto 1)
dameTamañoDelArchivo :: Archivo -> Int
dameTamañoDelArchivo (Archivo _ contenido) =  (*8)(length (contenido))

--Punto2)
archivoEsVacio = (==0).dameTamañoDelArchivo

--Punto3) 
--Para saber la cantidad de lineas de un archivo contamos la cantidad de saltos \nombre




