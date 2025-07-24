import ArbolSufijos._
import Oraculo.*
import ReconstCadenas.*
import ReconstCadenasPar.*
import org.scalameter.measure
import Benchmark.*

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
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION MEJORADO
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
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION SOLUCION TURBO
 */

// Caso 1: cadena vacía
val secTurboDef1 = Seq[Char]()
val orTurboDef1 = crearOraculo(1)(secTurboDef1)
reconstruirCadenaTurbo(secTurboDef1.length, orTurboDef1) == secTurboDef1
reconstruirCadenaTurboPar(1)(secTurboDef1.length, orTurboDef1) == secTurboDef1 // Umbral = 2^0 = 1

// Caso 2: longitud 1 (2^0 = 1)
val secTurboDef2 = Seq('t')
val orTurboDef2 = crearOraculo(1)(secTurboDef2)
reconstruirCadenaTurbo(secTurboDef2.length, orTurboDef2) == secTurboDef2
reconstruirCadenaTurboPar(2)(secTurboDef2.length, orTurboDef2) == secTurboDef2 // Umbral = 2^1 = 2

// Caso 3: longitud 2 (2^1)
val secTurboDef3 = Seq('a', 't')
val orTurboDef3 = crearOraculo(1)(secTurboDef3)
reconstruirCadenaTurbo(secTurboDef3.length, orTurboDef3) == secTurboDef3
reconstruirCadenaTurboPar(2)(secTurboDef3.length, orTurboDef3) == secTurboDef3 // Umbral = 2^1 = 2

// Caso 4: longitud 4 (2^2)
val secTurboDef4 = Seq('c', 'a', 'c', 'g')
val orTurboDef4 = crearOraculo(1)(secTurboDef4)
reconstruirCadenaTurbo(secTurboDef4.length, orTurboDef4) == secTurboDef4
reconstruirCadenaTurboPar(2)(secTurboDef4.length, orTurboDef4) == secTurboDef4 // Umbral = 2^1 = 2

// Caso 5: longitud 8 (2^3)
val secTurboDef5 = Seq('g', 't', 'a', 'c', 'g', 't', 'a', 'c')
val orTurboDef5 = crearOraculo(1)(secTurboDef5)
reconstruirCadenaTurbo(secTurboDef5.length, orTurboDef5) == secTurboDef5
reconstruirCadenaTurboPar(8)(secTurboDef5.length, orTurboDef5) == secTurboDef5 // Umbral = 2^3 = 8

// Caso 6: longitud 16 (2^4)
val secTurboDef6 = Seq('t', 'c', 'a', 'g', 't', 'c', 'a', 'g', 'a', 'g', 'c', 't', 'g', 'a', 't', 'c')
val orTurboDef6 = crearOraculo(1)(secTurboDef6)
reconstruirCadenaTurbo(secTurboDef6.length, orTurboDef6) == secTurboDef6
reconstruirCadenaTurboPar(4)(secTurboDef6.length, orTurboDef6) == secTurboDef6 // Umbral = 2^2 = 4

// Caso 7: longitud 32 (2^5)
val secTurboDef7 = Seq('a', 'a', 'c', 'c', 'g', 'g', 't', 't', 'a', 'a', 'c', 'c', 'g', 'g', 't', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
val orTurboDef7 = crearOraculo(1)(secTurboDef7)
reconstruirCadenaTurbo(secTurboDef7.length, orTurboDef7) == secTurboDef7
reconstruirCadenaTurboPar(64)(secTurboDef7.length, orTurboDef7) == secTurboDef7 // Umbral = 2^6 = 64

// Caso 8: longitud 128 (2^7)
val secTurboDef8 = Seq('g','t','a','g','g','a','g','a','g','a','g','g','t','c','a','g','c','g','c','g','g','a','t','t','t','a','a','a','t','a','a','c','g','t','g','c','a','g','g','c','t','t','t','t','c','g','a','t','t','a','t','t','t','c','g','g','c','c','t','c','c','a','c','t','g','t','c','t','c','t','t','c','g','g','t','c','a','t','t','t','a','t','g','a','c','c','g','g','g','c','t','a','c','t','c','g','t','a','c','g','a','t','c','g','c','g','a','t','t','c','g','g','a','c','g','c','t','t','g','g','c','c','g','a','c','t','a','a')
val orTurboDef8 = crearOraculo(1)(secTurboDef8)
reconstruirCadenaTurbo(secTurboDef8.length, orTurboDef8) == secTurboDef8
reconstruirCadenaTurboPar(128)(secTurboDef8.length, orTurboDef8) == secTurboDef8 // Umbral = 2^7 = 128


//Rendimiento secuencial de la funcion solucion turbo

// Caso 10: Longitud 2^9 = 512
val (secTRend10, nTRend10, oTRend10) = secuenciaRandom(512)
measure { reconstruirCadenaTurbo(nTRend10, oTRend10) }

/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION TURBOMEJORADA
 */

// Caso 1: Cadena de longitud 1 (2^0)
val secTM1 = Seq('c')
val orTM1 = crearOraculo(1)(secTM1)
reconstruirCadenaTurboMejorada(secTM1.length, orTM1) == secTM1
reconstruirCadenaTurboMejoradaPar(0)(secTM1.length, orTM1) == secTM1

// Caso 2: Cadena de longitud 2 (2^1) con caracteres distintos
val secTM2 = Seq('a', 't')
val orTM2 = crearOraculo(1)(secTM2)
reconstruirCadenaTurboMejorada(secTM2.length, orTM2) == secTM2
reconstruirCadenaTurboMejoradaPar(1)(secTM2.length, orTM2) == secTM2

// Caso 3: Cadena de 2 caracteres iguales (2^1)
val secTM3 = Seq('g', 'g')
val orTM3 = crearOraculo(1)(secTM3)
reconstruirCadenaTurboMejorada(secTM3.length, orTM3) == secTM3
reconstruirCadenaTurboMejoradaPar(1)(secTM3.length, orTM3) == secTM3

// Caso 4: Cadena palíndroma de tamaño 4 (2^2)
val secTM4 = Seq('a', 'c', 'c', 'a')
val orTM4 = crearOraculo(1)(secTM4)
reconstruirCadenaTurboMejorada(secTM4.length, orTM4) == secTM4
reconstruirCadenaTurboMejoradaPar(2)(secTM4.length, orTM4) == secTM4

// Caso 5: Cadena de longitud 8 (2^3) con solo 2 letras del alfabeto
val secTM5 = Seq('a', 'a', 'c', 'a', 'c', 'a', 'a', 'c')
val orTM5 = crearOraculo(1)(secTM5)
reconstruirCadenaTurboMejorada(secTM5.length, orTM5) == secTM5
reconstruirCadenaTurboMejoradaPar(4)(secTM5.length, orTM5) == secTM5

// Caso 6: Cadena variada de longitud 16 (2^4)
val secTM6 = Seq('g','a','c','t','g','a','c','t','c','g','a','t','c','g','a','t')
val orTM6 = crearOraculo(1)(secTM6)
reconstruirCadenaTurboMejorada(secTM6.length, orTM6) == secTM6
reconstruirCadenaTurboMejoradaPar(8)(secTM6.length, orTM6) == secTM6

// Caso 7: Cadena aleatoria de longitud 64 (2^6)
val (secTM7, nTM7, orTM7) = secuenciaRandom(64)
reconstruirCadenaTurboMejorada(nTM7, orTM7) == secTM7
reconstruirCadenaTurboMejoradaPar(32)(nTM7, orTM7) == secTM7

//Rendimiento de la funcion turboMejorada

for {
  i <- 12 to 12
  (sec, n, or) = secuenciaRandom(Math.pow(2, i).toInt)
} yield measure(reconstruirCadenaTurboMejorada(n, or))

/**
 * CASOS DE PRUEBA PARA LA FUNCIÓN PERTENECE
 */

// Construcción del trie con todas los sufijos de las palabras
val palabras = Seq(
  Seq('a', 'c', 't'),
  Seq('a', 'c'),
  Seq('t', 'a', 'g')
)
val trie = arbolDeSufijos(palabras)

// Caso 1: Palabra insertada directamente y también sufijo de otra
val p1 = Seq('a', 'c')
pertenece(p1, trie) // true

// Caso 2: Palabra original completa
val p2 = Seq('a', 'c', 't')
pertenece(p2, trie) // true

// Caso 3: Sufijo de una palabra, no insertado directamente pero generado por arbolDeSufijos
val p3 = Seq('c', 't')
pertenece(p3, trie) // true

// Caso 4: Secuencia que no aparece como sufijo de ninguna palabra
val p4 = Seq('g', 'a')
pertenece(p4, trie) // false

// Caso 5: Último carácter de una palabra (es un sufijo válido)
val p5 = Seq('g')
pertenece(p5, trie) // true

// Caso 6: Palabra que no aparece como sufijo en ningún lugar
val p6 = Seq('a', 'g', 'a')
pertenece(p6, trie) // false

// Caso 7: Secuencia vacía (raíz no está marcada)
val p7 = Seq()
pertenece(p7, trie) // false

/**
 * CASOS DE PRUEBA PARA LA FUNCIÓN ADICIONAR
 */

// Trie vacío inicial
val trie0 = Nodo(' ', false, Nil)

// Caso 0: Probar que el trie este vacio
pertenece(Seq('a'), trie0) // false

// Caso 1: Añadir una palabra de un solo carácter
val trie1 = adicionar(Seq('a'), trie0)
pertenece(Seq('a'), trie1) // true

// Caso 2: Añadir una palabra nueva de longitud mayor
val trie2 = adicionar(Seq('a', 'c', 't'), trie1)
pertenece(Seq('a', 'c', 't'), trie2) // true
pertenece(Seq('a', 'c'), trie2)      // false (aún no marcada)

// Caso 3: Añadir ese prefijo como palabra válida
val trie3 = adicionar(Seq('a', 'c'), trie2)
pertenece(Seq('a', 'c'), trie3)      // true

// Caso 4: Añadir una palabra completamente distinta
val trie4 = adicionar(Seq('t', 'a', 'g'), trie3)
pertenece(Seq('t', 'a', 'g'), trie4) // true
pertenece(Seq('t'), trie4)          // false

// Caso 5: Verificar que palabras no añadidas no existan
pertenece(Seq('g', 'a'), trie4)      // false
pertenece(Seq('a', 'g'), trie4)      // false

// Caso 6: Reinsertar palabra ya existente
val trie5 = adicionar(Seq('a', 'c'), trie4)
pertenece(Seq('a', 'c'), trie5)      // true

/**
 * CASOS DE PRUEBA PARA LA FUNCIÓN arbolDeSufijos
 */

// Caso 1: Palabra simple
val sufTrie1 = arbolDeSufijos(Seq(Seq('a', 'c', 't')))
pertenece(Seq('a', 'c', 't'), sufTrie1) // true
pertenece(Seq('c', 't'), sufTrie1)      // true
pertenece(Seq('t'), sufTrie1)           // true
pertenece(Seq('a', 'c'), sufTrie1)      // false
pertenece(Seq('c'), sufTrie1)           // false
pertenece(Seq('a'), sufTrie1)           // false

// Caso 2: Palabra palíndroma
val sufTrie2 = arbolDeSufijos(Seq(Seq('g', 'a', 'g')))
pertenece(Seq('g', 'a', 'g'), sufTrie2) // true
pertenece(Seq('a', 'g'), sufTrie2)      // true
pertenece(Seq('g'), sufTrie2)           // true
pertenece(Seq('g', 'a'), sufTrie2)      // false

// Caso 3: Varias palabras
val sufTrie3 = arbolDeSufijos(Seq(Seq('a', 'c', 't'), Seq('g', 'a'), Seq('t', 'a', 'g')))
pertenece(Seq('a', 'c', 't'), sufTrie3) // true
pertenece(Seq('c', 't'), sufTrie3)      // true
pertenece(Seq('a'), sufTrie3)           // true
pertenece(Seq('g', 'a'), sufTrie3)      // true
pertenece(Seq('a', 'g'), sufTrie3)      // true
pertenece(Seq('t', 'a', 'g'), sufTrie3) // true
pertenece(Seq('g'), sufTrie3)           // true
pertenece(Seq('t', 'a'), sufTrie3)      // false

// Caso 4: Palabra única de un solo carácter
val sufTrie4 = arbolDeSufijos(Seq(Seq('z')))
pertenece(Seq('z'), sufTrie4) // true
pertenece(Seq(), sufTrie4)    // false

// Caso 5: Palabra vacía
var sufTrie5 = arbolDeSufijos(Seq(Seq()))
pertenece(Seq(), sufTrie5) // true
pertenece(Seq('a'), sufTrie5) // false

/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCIÓN TURBOACELERADA
 */

// Caso 1: Cadena de longitud 1 (2^0)
val secTA1 = Seq('c')
val orTA1 = crearOraculo(1)(secTA1)
reconstruirCadenaTurboAcelerada(secTA1.length, orTA1) == secTA1
reconstruirCadenaTurboAceleradaPar(0)(secTA1.length, orTA1) == secTA1

// Caso 2: Cadena de longitud 2 (2^1) con caracteres distintos
val secTA2 = Seq('a', 't')
val orTA2 = crearOraculo(1)(secTA2)
reconstruirCadenaTurboAcelerada(secTA2.length, orTA2) == secTA2
reconstruirCadenaTurboAceleradaPar(1)(secTA2.length, orTA2) == secTA2

// Caso 3: Cadena de 2 caracteres iguales (2^1)
val secTA3 = Seq('g', 'g')
val orTA3 = crearOraculo(1)(secTA3)
reconstruirCadenaTurboAcelerada(secTA3.length, orTA3) == secTA3
reconstruirCadenaTurboAceleradaPar(1)(secTA3.length, orTA3) == secTA3

// Caso 4: Cadena palíndroma de tamaño 4 (2^2)
val secTA4 = Seq('a', 'c', 'c', 'a')
val orTA4 = crearOraculo(1)(secTA4)
reconstruirCadenaTurboAcelerada(secTA4.length, orTA4) == secTA4
reconstruirCadenaTurboAceleradaPar(2)(secTA4.length, orTA4) == secTA4

// Caso 5: Cadena de longitud 8 (2^3) con solo 2 letras del alfabeto
val secTA5 = Seq('a', 'a', 'c', 'a', 'c', 'a', 'a', 'c')
val orTA5 = crearOraculo(1)(secTA5)
reconstruirCadenaTurboAcelerada(secTA5.length, orTA5) == secTA5
reconstruirCadenaTurboAceleradaPar(4)(secTA5.length, orTA5) == secTA5

// Caso 6: Cadena variada de longitud 16 (2^4)
val secTA6 = Seq('g','a','c','t','g','a','c','t','c','g','a','t','c','g','a','t')
val orTA6 = crearOraculo(1)(secTA6)
reconstruirCadenaTurboAcelerada(secTA6.length, orTA6) == secTA6
reconstruirCadenaTurboAceleradaPar(8)(secTA6.length, orTA6) == secTA6

// Caso 7: Cadena aleatoria de longitud 64 (2^6)
val (secTA7, nTA7, orTA7) = secuenciaRandom(64)
reconstruirCadenaTurboAcelerada(nTA7, orTA7) == secTA7
reconstruirCadenaTurboAceleradaPar(32)(nTA7, orTA7) == secTA7

//Rendimiento de la funcion turboMejorada

for {
  i <- 12 to 12
  (sec, n, or) = secuenciaRandom(Math.pow(2, i).toInt)
} yield reconstruirCadenaTurboAcelerada(n, or) == sec

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
 * IMPACTO DE TECNICAS DE PARALELIZACION PARA LA VERSION SOLUCION TURBO
 */

// Tamaño 1 (2^0). Umbral > Tamaño (Ej: Umbral = 2^1 = 2)
val (_, nTurboP_gt1, oTurboP_gt1) = secuenciaRandom(1)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(nTurboP_gt1, oTurboP_gt1)

// Tamaño 2 (2^1). Umbral = Tamaño (Ej: Umbral = 2^1 = 2)
val (_, nTurboP_eq1, oTurboP_eq1) = secuenciaRandom(2)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(2))(nTurboP_eq1, oTurboP_eq1)

// Tamaño 4 (2^2). Umbral > Tamaño (Ej: Umbral = 2^4 = 16)
val (_, nTurboP_gt2, oTurboP_gt2) = secuenciaRandom(4)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(16))(nTurboP_gt2, oTurboP_gt2)

// Tamaño 8 (2^3). Umbral = Tamaño (Ej: Umbral = 2^3 = 8)
val (_, nTurboP_eq2, oTurboP_eq2) = secuenciaRandom(8)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(8))(nTurboP_eq2, oTurboP_eq2)

// Tamaño 16 (2^4). Umbral < Tamaño (Ej: Umbral = 2^2 = 4)
val (_, nTurboP_lt1, oTurboP_lt1) = secuenciaRandom(16)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(4))(nTurboP_lt1, oTurboP_lt1)

// Tamaño 32 (2^5). Umbral = Tamaño (Ej: Umbral = 2^5 = 32)
val (_, nTurboP_eq3, oTurboP_eq3) = secuenciaRandom(32)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(32))(nTurboP_eq3, oTurboP_eq3)

// Tamaño 64 (2^6). Umbral < Tamaño (Ej: Umbral = 2^3 = 8)
val (_, nTurboP_lt2, oTurboP_lt2) = secuenciaRandom(64)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(8))(nTurboP_lt2, oTurboP_lt2)

// Tamaño 128 (2^7). Umbral > Tamaño (Ej: Umbral = 2^9 = 512)
val (_, nTurboP_gt3, oTurboP_gt3) = secuenciaRandom(128)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(512))(nTurboP_gt3, oTurboP_gt3)

// Tamaño 256 (2^8). Umbral < Tamaño (Ej: Umbral = 2^7 = 128)
val (_, nTurboP_lt3, oTurboP_lt3) = secuenciaRandom(256)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(128))(nTurboP_lt3, oTurboP_lt3)

// Tamaño 512 (2^9). Umbral > Tamaño (Ej: Umbral = 2^12 = 4096)
val (_, nTurboP_gt4, oTurboP_gt4) = secuenciaRandom(512)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(4096))(nTurboP_gt4, oTurboP_gt4)

// Tamaño 1024 (2^10). Umbral < Tamaño (Ej: Umbral = 2^9 = 512)
val (_, nTurboP_lt4, oTurboP_lt4) = secuenciaRandom(1024)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar(512))(nTurboP_lt4, oTurboP_lt4)


/**
 * IMPACTO DE TÉCNICAS DE PARALELIZACIÓN PARA LA VERSIÓN TURBOMEJORADA
 */

// Tamaño 1 (2^0). Umbral = 0
val (_, nTurboP_1, oTurboP_1) = secuenciaRandom(1)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_1 / 2))(nTurboP_1, oTurboP_1)

// Tamaño 2 (2^1). Umbral = 1
val (_, nTurboP_2, oTurboP_2) = secuenciaRandom(2)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_2 / 2))(nTurboP_2, oTurboP_2)

// Tamaño 4 (2^2). Umbral = 2
val (_, nTurboP_4, oTurboP_4) = secuenciaRandom(4)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_4 / 2))(nTurboP_4, oTurboP_4)

// Tamaño 8 (2^3). Umbral = 4
val (_, nTurboP_8, oTurboP_8) = secuenciaRandom(8)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_8 / 2))(nTurboP_8, oTurboP_8)

// Tamaño 16 (2^4). Umbral = 8
val (_, nTurboP_16, oTurboP_16) = secuenciaRandom(16)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_16 / 2))(nTurboP_16, oTurboP_16)

// Tamaño 32 (2^5). Umbral = 16
val (_, nTurboP_32, oTurboP_32) = secuenciaRandom(32)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_32 / 2))(nTurboP_32, oTurboP_32)

// Tamaño 64 (2^6). Umbral = 32
val (_, nTurboP_64, oTurboP_64) = secuenciaRandom(64)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_64 / 2))(nTurboP_64, oTurboP_64)

// Tamaño 128 (2^7). Umbral = 64
val (_, nTurboP_128, oTurboP_128) = secuenciaRandom(128)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_128 / 2))(nTurboP_128, oTurboP_128)

// Tamaño 256 (2^8). Umbral = 128
val (_, nTurboP_256, oTurboP_256) = secuenciaRandom(256)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_256 / 2))(nTurboP_256, oTurboP_256)

// Tamaño 512 (2^9). Umbral = 256
val (_, nTurboP_512, oTurboP_512) = secuenciaRandom(512)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_512 / 2))(nTurboP_512, oTurboP_512)

// Tamaño 1024 (2^10). Umbral = 512
val (_, nTurboP_1024, oTurboP_1024) = secuenciaRandom(1024)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_1024 / 2))(nTurboP_1024, oTurboP_1024)

// Tamaño 2048 (2^11). Umbral = 1024
val (_, nTurboP_2048, oTurboP_2048) = secuenciaRandom(2048)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar(nTurboP_2048 / 2))(nTurboP_2048, oTurboP_2048)

/**
 * IMPACTO DE TÉCNICAS DE PARALELIZACIÓN PARA LA VERSIÓN TURBOACELERADA
 */

// Tamaño 1 (2^0). Umbral = 0
val (_, nTA_1, oTA_1) = secuenciaRandom(1)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_1 / 2))(nTA_1, oTA_1)

// Tamaño 2 (2^1). Umbral = 1
val (_, nTA_2, oTA_2) = secuenciaRandom(2)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_2 / 2))(nTA_2, oTA_2)

// Tamaño 4 (2^2). Umbral = 2
val (_, nTA_4, oTA_4) = secuenciaRandom(4)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_4 / 2))(nTA_4, oTA_4)

// Tamaño 8 (2^3). Umbral = 4
val (_, nTA_8, oTA_8) = secuenciaRandom(8)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_8 / 2))(nTA_8, oTA_8)

// Tamaño 16 (2^4). Umbral = 8
val (_, nTA_16, oTA_16) = secuenciaRandom(16)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_16 / 2))(nTA_16, oTA_16)

// Tamaño 32 (2^5). Umbral = 16
val (_, nTA_32, oTA_32) = secuenciaRandom(32)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_32 / 2))(nTA_32, oTA_32)

// Tamaño 64 (2^6). Umbral = 32
val (_, nTA_64, oTA_64) = secuenciaRandom(64)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_64 / 2))(nTA_64, oTA_64)

// Tamaño 128 (2^7). Umbral = 64
val (_, nTA_128, oTA_128) = secuenciaRandom(128)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_128 / 2))(nTA_128, oTA_128)

// Tamaño 256 (2^8). Umbral = 128
val (_, nTA_256, oTA_256) = secuenciaRandom(256)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_256 / 2))(nTA_256, oTA_256)

// Tamaño 512 (2^9). Umbral = 256
val (_, nTA_512, oTA_512) = secuenciaRandom(512)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_512 / 2))(nTA_512, oTA_512)

// Tamaño 1024 (2^10). Umbral = 512
val (_, nTA_1024, oTA_1024) = secuenciaRandom(1024)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_1024 / 2))(nTA_1024, oTA_1024)

// Tamaño 2048 (2^11). Umbral = 1024
val (_, nTA_2048, oTA_2048) = secuenciaRandom(2048)
for {
  i <- 1 to 3
} yield compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar(nTA_2048 / 2))(nTA_2048, oTA_2048)

