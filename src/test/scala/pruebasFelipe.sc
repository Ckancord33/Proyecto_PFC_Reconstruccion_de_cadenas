import Oraculo._
import ReconstCadenas._
import scala.util.Random

val random = new Random()

def secAlAzar(long:Int, s:Seq[Char]): Seq[Char] = {
  //Crea una secuencia de long caracteres del alfabeto,
  // escogidos de forma aleatoria, terminando en s
  if (s.length==long) s
  else {
    val indiceAzar=random.nextInt(4)
    secAlAzar(long,alfabeto(indiceAzar)+:s)
  }
}
val costoOraculo = 1

// Pruebas Solución Turbo
val sec1=Seq('a', 'c', 'c', 'a')
val sec2 = Seq('a', 'c', 'g', 'c', 'a') //No se puede reconstruir por que no es potencia de 2
val sec3=secAlAzar(8,Seq())
val sec4=secAlAzar(16,Seq())

val or_1=crearOraculo(costoOraculo)(sec1)
val or_2=crearOraculo(costoOraculo)(sec2)
val or_3=crearOraculo(costoOraculo)(sec3)
val or_4=crearOraculo(costoOraculo)(sec4)

reconstruirCadenaMejorado(sec1.length, or_1)
reconstruirCadenaMejorado(sec2.length, or_2)
reconstruirCadenaMejorado(sec3.length, or_3)
reconstruirCadenaMejorado(sec4.length, or_4)