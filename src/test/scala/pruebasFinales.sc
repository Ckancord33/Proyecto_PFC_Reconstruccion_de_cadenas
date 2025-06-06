import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._

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

/**
 * CASOS DE PRUEBA Y DE RENDIMIENTO DE LA FUNCION TURBO ACELERADA
 */

