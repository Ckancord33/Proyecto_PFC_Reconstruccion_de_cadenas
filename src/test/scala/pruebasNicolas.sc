import Oraculo.*
import Benchmark.*
import ReconstCadenas.*
import ReconstCadenasPar._
import scala.util.Random
import org.scalameter.*
import ArbolSufijos._

/*Crear una secuencia aleatoria de tamano i, solo cambiar
  el rango para hacer una secuencia mas larga */
val pruebaSecuencia: Seq[Char] =  for{
  i <- 1 to 6
}yield alfabeto(3)
val pruebaOraculo: Oraculo = crearOraculo(1)(pruebaSecuencia)

reconstruirCadenaIngenuo(pruebaSecuencia.length, pruebaOraculo)
reconstruirCadenaIngenuoPar(2)(pruebaSecuencia.length, pruebaOraculo)
//Pruebas
compararAlgoritmos(reconstruirCadenaIngenuoPar(3), reconstruirCadenaIngenuoPar(4)) (pruebaSecuencia.length, pruebaOraculo)

// Árbol base de prueba
val arbolDePrueba: Trie = Nodo(' ', false, List(
  Nodo('a', false, List(
    Hoja('b', true),
    Hoja('c', true)
  )),
  Hoja('x', true)
))

// Pruebas iniciales
assert(pertenece(Seq('a', 'b'), arbolDePrueba))     // true
assert(pertenece(Seq('a', 'c'), arbolDePrueba))     // true
assert(pertenece(Seq('x'), arbolDePrueba))          // true
assert(!pertenece(Seq('a'), arbolDePrueba))         // false
assert(!pertenece(Seq('b'), arbolDePrueba))         // false
assert(!pertenece(Seq('a', 'b', 'd'), arbolDePrueba)) // false
assert(!pertenece(Seq('r'), arbolDePrueba))         // false

// Adicionando una nueva palabra
val arbol1 = adicionar(Seq('r', 'b', 'c', 'd'), arbolDePrueba)
assert(pertenece(Seq('r', 'b', 'c', 'd'), arbol1))   // true
assert(pertenece(Seq('a', 'b'), arbol1))            // true
assert(pertenece(Seq('x'), arbol1))                 // true
assert(!pertenece(Seq('r', 'b', 'c'), arbol1))      // false

// Más adiciones
val arbol2 = adicionar(Seq('a', 'b', 'd'), arbol1)
val arbol3 = adicionar(Seq('a', 'z'), arbol2)
val arbol4 = adicionar(Seq('z'), arbol3)
val arbol5 = adicionar(Seq('z', 'z', 'z'), arbol4)

// Nuevas pruebas
assert(pertenece(Seq('a', 'b', 'd'), arbol2))        // true
assert(pertenece(Seq('a', 'z'), arbol3))             // true
assert(pertenece(Seq('z'), arbol4))                  // true
assert(pertenece(Seq('z', 'z', 'z'), arbol5))        // true
assert(pertenece(Seq('a', 'b'), arbol5))             // true
assert(pertenece(Seq('x'), arbol5))                  // true
assert(!pertenece(Seq('a', 'b', 'd', 'e'), arbol5))  // false
assert(!pertenece(Seq('z', 'z'), arbol5))            // false, arbolDePrueba2)

val arbolS = arbolDeSufijos(Seq(Seq('c','a','c','t'), Seq('a','c','a','c')))
pertenece(Seq('c','t'), arbolS)

val t = Nodo(' ', false, List(
  Nodo('a', false, List(
    Nodo('c', true, List(
      Nodo('a', false, List(
        Hoja('c', true)
      )),
      Hoja('t', true)
    ))
  )),
  Nodo('c', true, List(
    Nodo('a', false, List(
      Nodo('c', true, List(
        Hoja('t', true)
      ))
    )),
    Hoja('t', true)
  )),
  Hoja('t', true)
))

t == arbolS //No quedan iguales nomas porque se organizan distinto

adicionar("hola".toSeq, Nodo(' ', false, Nil))

val arbol10 = arbolDeSufijos(Seq("hola".toSeq, "mundo".toSeq))
// Sufijos esperados: "hola", "ola", "la", "a", "mundo", "undo", "ndo", "do", "o"
assert(pertenece("hola".toSeq, arbol10))
assert(pertenece("mundo".toSeq, arbol10))
assert(pertenece("ndo".toSeq, arbol10))
assert(pertenece("o".toSeq, arbol10))
assert(!pertenece("ho".toSeq, arbol10))
assert(!pertenece("mun".toSeq, arbol10))