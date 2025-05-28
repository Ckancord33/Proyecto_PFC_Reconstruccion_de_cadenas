import Benchmark.compararAlgoritmos
import Oraculo.{Oraculo, alfabeto, crearOraculo}
import ReconstCadenas.{reconstruirCadenaIngenuo, reconstruirCadenaMejorado}
import ReconstCadenasPar.reconstruirCadenaMejoradoPar
import scala.util.Random

// ————————————————————————————————
// Generador de secuencias aleatorias
// ————————————————————————————————
val random = new Random()
def secAlAzar(long: Int, s: Seq[Char]): Seq[Char] =
  if (s.length == long) s
  else secAlAzar(long, alfabeto(random.nextInt(alfabeto.length)) +: s)

// ————————————————————————————————
// Parámetros iniciales predeterminado
// ————————————————————————————————
val costoOraculo = 1

// ————————————————————————————————
// Ejecuciones individuales versión secuencial
// ————————————————————————————————

// PRUEBA N°1: n = 88
val pruebaSecuencia1: Seq[Char] = for { i <- 1 to 88  } yield alfabeto(3)
val pruebaOraculo1: Oraculo       = crearOraculo(costoOraculo)(pruebaSecuencia1)
reconstruirCadenaMejorado(pruebaSecuencia1.length, pruebaOraculo1)

// PRUEBA N°2: n = 69
val pruebaSecuencia2: Seq[Char] = for { i <- 1 to 69  } yield alfabeto(3)
val pruebaOraculo2: Oraculo       = crearOraculo(costoOraculo)(pruebaSecuencia2)
reconstruirCadenaMejorado(pruebaSecuencia2.length, pruebaOraculo2)

// PRUEBA N°3: n = 668
val pruebaSecuencia3: Seq[Char] = for { i <- 1 to 668 } yield alfabeto(3)
val pruebaOraculo3: Oraculo       = crearOraculo(costoOraculo)(pruebaSecuencia3)
reconstruirCadenaMejorado(pruebaSecuencia3.length, pruebaOraculo3)

// PRUEBA N°4: n = 700
val pruebaSecuencia4: Seq[Char] = for { i <- 1 to 700 } yield alfabeto(3)
val pruebaOraculo4: Oraculo       = crearOraculo(costoOraculo)(pruebaSecuencia4)
reconstruirCadenaMejorado(pruebaSecuencia4.length, pruebaOraculo4)

// PRUEBA N°5: n = 1024
val pruebaSecuencia5: Seq[Char] = for { i <- 1 to 1024 } yield alfabeto(3)
val pruebaOraculo5: Oraculo       = crearOraculo(costoOraculo)(pruebaSecuencia5)
reconstruirCadenaMejorado(pruebaSecuencia5.length, pruebaOraculo5)


// ————————————————————————————————
// Ejecuciones individuales versión paralela
// ————————————————————————————————
val umbralPequeno1 = 9
reconstruirCadenaMejoradoPar(umbralPequeno1)(pruebaSecuencia1.length, pruebaOraculo1)

val umbralPequeno2 = 34
reconstruirCadenaMejoradoPar(umbralPequeno2)(pruebaSecuencia2.length, pruebaOraculo2)

val umbralPequeno3 = 668
reconstruirCadenaMejoradoPar(umbralPequeno3)(pruebaSecuencia3.length, pruebaOraculo3)

val umbralPequeno4 = 783
reconstruirCadenaMejoradoPar(umbralPequeno4)(pruebaSecuencia4.length, pruebaOraculo4)

val umbralPequeno5 = 512
reconstruirCadenaMejoradoPar(umbralPequeno5)(pruebaSecuencia5.length, pruebaOraculo5)


// ————————————————————————————————
// Comparativa de umbrales en la versión paralela
// ————————————————————————————————
val umbralMedio1  = 3
val umbralGrande1 = 4
compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umbralMedio1),
  reconstruirCadenaMejoradoPar(umbralGrande1)
)(pruebaSecuencia1.length, pruebaOraculo1)

val umbralMedio2  = 7
val umbralGrande2 = 9
compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umbralMedio2),
  reconstruirCadenaMejoradoPar(umbralGrande2)
)(pruebaSecuencia2.length, pruebaOraculo2)

val umbralMedio3  = 512
val umbralGrande3 = 1024
compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umbralMedio3),
  reconstruirCadenaMejoradoPar(umbralGrande3)
)(pruebaSecuencia3.length, pruebaOraculo3)

val umbralMedio4  = 128
val umbralGrande4 = 512
compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umbralMedio4),
  reconstruirCadenaMejoradoPar(umbralGrande4)
)(pruebaSecuencia4.length, pruebaOraculo4)

val umbralMedio5  = 256
val umbralGrande5 = 1024
compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umbralMedio5),
  reconstruirCadenaMejoradoPar(umbralGrande5)
)(pruebaSecuencia5.length, pruebaOraculo5)


// ————————————————————————————————
// Comparativa de alternativas en la versión secuencial
// ————————————————————————————————

// PRUEBA N°1
val n4  = 4
val s4  = secAlAzar(n4, Seq())
val o4  = crearOraculo(costoOraculo)(s4)
val res4 = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaMejorado)(n4, o4)

// PRUEBA N°2
val n8  = 8
val s8  = secAlAzar(n8, Seq())
val o8  = crearOraculo(costoOraculo)(s8)
val res8 = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaMejorado)(n8, o8)

// PRUEBA N°3
val n12  = 12
val s12  = secAlAzar(n12, Seq())
val o12  = crearOraculo(costoOraculo)(s12)
val res12 = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaMejorado)(n12, o12)

// PRUEBA N°4
val n16  = 16
val s16  = secAlAzar(n16, Seq())
val o16  = crearOraculo(costoOraculo)(s16)
val res16 = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaMejorado)(n16, o16)

// PRUEBA N°5
val n1024  = 1024
val s1024  = secAlAzar(n1024, Seq())
val o1024  = crearOraculo(costoOraculo)(s1024)
val res1024 = compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaMejorado)(n1024, o1024)
