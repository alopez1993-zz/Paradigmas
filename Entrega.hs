module Entrega where
type Fecha = (Int, Int, Int)
data EstudianteData = EstudianteData String String String Fecha Float Bool
data EstudianteSyntax = EstudianteSyntax {
  nombre :: String,
  dni :: String,
  legajo :: String,
  fecha :: Fecha,
  promedio :: Float,
  regular :: Bool
}

etiqueta :: Integer
etiqueta = 80


--promedioCurso :: CursoData -> Int
--promedioCurso (CursoData ) = 

--mejorNota :: CursoSyntax -> String

