import Oraculo.*
import ReconstCadenas.*
import scala.util.Random

/*Crear una secuencia aleatoria de tamano i, solo cambiar
  el rango para hacer una secuencia mas larga */
val pruebaSecuencia: Seq[Char] = for{
  i <- 1 to 12
}yield alfabeto(Random.nextInt(alfabeto.length))
val pruebaOraculo: Oraculo = crearOraculo(0)(pruebaSecuencia)

//Pruebas
reconstruirCadenaIngenuo(pruebaSecuencia.length, pruebaOraculo)



