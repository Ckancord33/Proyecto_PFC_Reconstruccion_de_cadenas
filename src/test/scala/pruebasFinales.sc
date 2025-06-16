import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import org.scalameter.measure
import Benchmark._
import scala.util.Random

def secuenciaRandom(longitud: Int) = {
  val pruebaSecuencia: Seq[Char] = for {
    i <- 1 to longitud
  } yield alfabeto(Random.nextInt(4))
  val o: Oraculo = crearOraculo(1)(pruebaSecuencia)
  val n = pruebaSecuencia.length
  (pruebaSecuencia, n, o)
}

/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION INGENUA
 */

// Caso 1: Cadena vacía
val sec1 = Seq[Char]()
val or1 = crearOraculo(1)(sec1)
reconstruirCadenaIngenuo(sec1.length, or1) == sec1
reconstruirCadenaIngenuoPar(0)(sec1.length, or1) == sec1

// Caso 2: Cadena de longitud 1
val sec2 = Seq('a')
val or2 = crearOraculo(1)(sec2)
reconstruirCadenaIngenuo(sec2.length, or2) == sec2
reconstruirCadenaIngenuoPar(0)(sec2.length, or2) == sec2

// Caso 3: Cadena de longitud 2 con caracteres distintos
val sec3 = Seq('c', 'g')
val or3 = crearOraculo(1)(sec3)
reconstruirCadenaIngenuo(sec3.length, or3) == sec3
reconstruirCadenaIngenuoPar(1)(sec3.length, or3) == sec3

// Caso 4: Cadena con caracteres repetidos
val sec4 = Seq('g', 'g', 'g')
val or4 = crearOraculo(1)(sec4)
reconstruirCadenaIngenuo(sec4.length, or4) == sec4
reconstruirCadenaIngenuoPar(1)(sec4.length, or4) == sec4

// Caso 5: Cadena palíndroma
val sec5 = Seq('a', 'c', 'a')
val or5 = crearOraculo(1)(sec5)
reconstruirCadenaIngenuo(sec5.length, or5) == sec5
reconstruirCadenaIngenuoPar(2)(sec4.length, or4) == sec4

// Caso 6: Cadena de tamaño medio (5 caracteres)
val sec6 = Seq('t', 'g', 'a', 'c', 't')
val or6 = crearOraculo(1)(sec6)
reconstruirCadenaIngenuo(sec6.length, or6) == sec6
reconstruirCadenaIngenuoPar(2)(sec6.length, or6) == sec6

// Caso 7: Cadena más larga (8 caracteres), con mezcla y repeticiones
val sec7 = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
val or7 = crearOraculo(1)(sec7)
reconstruirCadenaIngenuo(sec7.length, or7) == sec7
reconstruirCadenaIngenuoPar(3)(sec7.length, or7) == sec7

//Rendimiento de la funcion ingenua secuencial
val (secuenciaIngenuo, nIngenuo, oIngenuo) = secuenciaRandom(10)
measure { reconstruirCadenaIngenuo(nIngenuo, oIngenuo) }

//Rendimiento de la funcion ingenua paralela
val (secuenciaIngenuo2, nIngenuo2, oIngenuo2) = secuenciaRandom(12)
measure { reconstruirCadenaIngenuoPar(4)(nIngenuo2, oIngenuo2) }

/**
 * IMPACTO DE TECNICAS DE PARALELIZACION PARA LA VERSION INGENUA
 */

// Tamaño 2
val (_, nIngenuop2, oIngenuop2) = secuenciaRandom(2)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar(1))(nIngenuop2, oIngenuop2)

// Tamaño 4
val (_, nIngenuop4, oIngenuop4) = secuenciaRandom(4)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar(2))(nIngenuop4, oIngenuop4)

// Tamaño 8
val (_, nIngenuop8, oIngenuop8) = secuenciaRandom(8)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar(3))(nIngenuop8, oIngenuop8)


/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION SOLUCION MEJORADO
 */

// Caso 1: cadena vacía (2^0 = 0)
val secM1 = Seq[Char]()
val orM1 = crearOraculo(1)(secM1)
reconstruirCadenaMejorado(secM1.length, orM1) == secM1
reconstruirCadenaMejoradoPar(0)(secM1.length, orM1) == secM1


// Caso 2: longitud 1 (2^0 = 1)
val secM2 = Seq('a')
val orM2 = crearOraculo(1)(secM2)
reconstruirCadenaMejorado(secM2.length, orM2) == secM2
reconstruirCadenaMejoradoPar(0)(secM2.length, orM2) == secM2


// Caso 3: longitud 2 (2^1)
val secM3 = Seq('c', 'g')
val orM3 = crearOraculo(1)(secM3)
reconstruirCadenaMejorado(secM3.length, orM3) == secM3
reconstruirCadenaMejoradoPar(1)(secM3.length, orM3) == secM3


// Caso 4: longitud 25 (2^4 < 25 < 2^5)
val secM4 = Seq('c', 't', 'a', 'g', 't', 'g', 't', 't', 'c', 't', 'a', 't', 'a', 'c', 'c', 'a', 'g', 'c', 't', 'g', 'a', 't', 'c', 'g', 't')
val orM4 = crearOraculo(1)(secM4)
reconstruirCadenaMejorado(secM4.length, orM4) == secM4
reconstruirCadenaMejoradoPar(1)(secM4.length, orM4) == secM4



// Caso 5: longitud 8 (2^3)
val secM5 = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
val orM5 = crearOraculo(1)(secM5)
reconstruirCadenaMejorado(secM5.length, orM5) == secM5
reconstruirCadenaMejoradoPar(2)(secM5.length, orM5) == secM5


// Caso 6: longitud 16 (2^4)
val secM6 = Seq('a', 'c', 'g', 't', 'c', 'a', 'g', 't', 'g', 't', 'c', 'a', 't', 'g', 'a', 'c')
val orM6 = crearOraculo(1)(secM6)
reconstruirCadenaMejorado(secM6.length, orM6) == secM6
reconstruirCadenaMejoradoPar(3)(secM6.length, orM6) == secM6


// Caso 7: longitud 32 (2^5)
val secM7 = Seq('c', 't', 'a', 'g', 't', 'g', 'c', 't', 'a', 'g', 't', 'g', 't', 't', 'c', 't', 'a', 't', 'a', 'c', 'c', 't', 't', 'c', 't', 'a', 't', 'a', 'c', 'c', 'g', 'a')
val orM7 = crearOraculo(1)(secM7)
reconstruirCadenaMejorado(secM7.length, orM7) == secM7
reconstruirCadenaMejoradoPar(4)(secM7.length, orM7) == secM7


// Caso 8: longitud 128 (2^7)
val secM8 = Seq('a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t','a','c','g','t')
val orM8 = crearOraculo(1)(secM8)
reconstruirCadenaMejorado(secM8.length, orM8) == secM8
reconstruirCadenaMejoradoPar(6)(secM8.length, orM8) == secM8

//Rendimiento de la funcion mejorada
val (secMRend, nMRend, oMRend) = secuenciaRandom(1024)
measure { reconstruirCadenaMejorado(nMRend, oMRend) }
measure { reconstruirCadenaMejoradoPar(9)(nMRend, oMRend) }

/**
 * IMPACTO DE TECNICAS DE PARALELIZACION PARA LA VERSION SOLUCION MEJORADA
 */

// Tamaño 1 (2^0). Umbral > Tamaño (Ej: Umbral = 10)
val (_, nMejoradoP_gt1, oMejoradoP_gt1) = secuenciaRandom(1)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(10))(nMejoradoP_gt1, oMejoradoP_gt1)

// Tamaño 2 (2^1). Umbral = Tamaño (Ej: Umbral = 2)
val (_, nMejoradoP_eq1, oMejoradoP_eq1) = secuenciaRandom(2)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(2))(nMejoradoP_eq1, oMejoradoP_eq1)

// Tamaño 4 (2^2). Umbral > Tamaño (Ej: Umbral = 100)
val (_, nMejoradoP_gt2, oMejoradoP_gt2) = secuenciaRandom(4)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(100))(nMejoradoP_gt2, oMejoradoP_gt2)

// Tamaño 8 (2^3). Umbral = Tamaño (Ej: Umbral = 8)
val (_, nMejoradoP_eq2, oMejoradoP_eq2) = secuenciaRandom(8)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(8))(nMejoradoP_eq2, oMejoradoP_eq2)

// Tamaño 16 (2^4). Umbral < Tamaño (Ej: Umbral = 8)
val (_, nMejoradoP_lt1, oMejoradoP_lt1) = secuenciaRandom(16)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(8))(nMejoradoP_lt1, oMejoradoP_lt1)

// Tamaño 32 (2^5). Umbral = Tamaño (Ej: Umbral = 32)
val (_, nMejoradoP_eq3, oMejoradoP_eq3) = secuenciaRandom(32)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(32))(nMejoradoP_eq3, oMejoradoP_eq3)

// Tamaño 64 (2^6). Umbral < Tamaño (Ej: Umbral = 32)
val (_, nMejoradoP_lt2, oMejoradoP_lt2) = secuenciaRandom(64)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(32))(nMejoradoP_lt2, oMejoradoP_lt2)

// Tamaño 128 (2^7). Umbral > Tamaño (Ej: Umbral = 2000)
val (_, nMejoradoP_gt3, oMejoradoP_gt3) = secuenciaRandom(128)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(2000))(nMejoradoP_gt3, oMejoradoP_gt3)

// Tamaño 256 (2^8). Umbral < Tamaño (Ej: Umbral = 128)
val (_, nMejoradoP_lt3, oMejoradoP_lt3) = secuenciaRandom(256)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(128))(nMejoradoP_lt3, oMejoradoP_lt3)

// Tamaño 512 (2^9). Umbral > Tamaño (Ej: Umbral = 4096)
val (_, nMejoradoP_gt4, oMejoradoP_gt4) = secuenciaRandom(512)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar(4096))(nMejoradoP_gt4, oMejoradoP_gt4)



/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION SOLUCION TURBO
 */

/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION TURBO MEJORADA
 */


/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION TURBO ACELERADA
 */


