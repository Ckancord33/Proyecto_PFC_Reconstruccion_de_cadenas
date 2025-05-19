import Oraculo.*
import Benchmark.*
import ReconstCadenas.*
import scala.util.Random
import org.scalameter.*

/*Crear una secuencia aleatoria de tamano i, solo cambiar
  el rango para hacer una secuencia mas larga */
val pruebaSecuencia: Seq[Char] =  for{
  i <- 1 to 6
}yield alfabeto(3)
val pruebaOraculo: Oraculo = crearOraculo(1)(pruebaSecuencia)

//Pruebas
compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoV2) (pruebaSecuencia.length, pruebaOraculo)
