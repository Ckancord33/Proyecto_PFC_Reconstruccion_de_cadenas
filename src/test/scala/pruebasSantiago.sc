import Benchmark.compararAlgoritmos
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.reconstruirCadenaMejorado
import ReconstCadenasPar.reconstruirCadenaMejoradoPar
import scala.util.Random
import scala.util.Random.*

/*Crear una secuencia aleatoria de tamano i, solo cambiar
  el rango para hacer una secuencia mas larga */
val pruebaSecuencia: Seq[Char] =  for{
  i <- 1 to 100
}yield alfabeto(Random.nextInt(4))

val pruebaOraculo: Oraculo = crearOraculo(0)(pruebaSecuencia)

reconstruirCadenaMejorado(pruebaSecuencia.length, pruebaOraculo)
reconstruirCadenaMejoradoPar(2)(pruebaSecuencia.length, pruebaOraculo)
//Pruebas
compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(100000)) (pruebaSecuencia.length, pruebaOraculo)

