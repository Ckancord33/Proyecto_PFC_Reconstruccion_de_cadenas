import Benchmark.compararAlgoritmos
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.reconstruirCadenaMejorado
import ReconstCadenasPar.reconstruirCadenaMejoradoPar

/*Crear una secuencia aleatoria de tamano i, solo cambiar
  el rango para hacer una secuencia mas larga */
val pruebaSecuencia: Seq[Char] =  for{
  i <- 1 to 6
}yield alfabeto(3)
val pruebaOraculo: Oraculo = crearOraculo(1)(pruebaSecuencia)

reconstruirCadenaMejorado(pruebaSecuencia.length, pruebaOraculo)
reconstruirCadenaMejoradoPar(2)(pruebaSecuencia.length, pruebaOraculo)
//Pruebas
compararAlgoritmos(reconstruirCadenaMejoradoPar(3), reconstruirCadenaMejoradoPar(4)) (pruebaSecuencia.length, pruebaOraculo)

