import common.*

import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*

import scala.collection.parallel.ParSeq
package object ReconstCadenasPar {
  // Ahora versiones paralelas

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas
    def cadenasDeTamanoN(tamano: Int, combParcial: LazyList[Seq[Char]]): LazyList[Seq[Char]] = {
      if (tamano == n) combParcial
      else {
        val nuevaCombParcial = for {
          comb <- combParcial
          letra <- alfabeto
        } yield comb :+ letra
        cadenasDeTamanoN(tamano + 1, nuevaCombParcial)
      }
    }
    val mitad = alfabeto.size / 2
    val (primerMitad, segundaMitad) = alfabeto.splitAt(mitad)

    def explorarConPrefijos(prefijos: Seq[Char]): Option[Seq[Char]] = {
      val combinaciones = for {
        letra <- prefijos.to(LazyList)
      } yield LazyList(Seq(letra))

      combinaciones
        .flatMap(pref => cadenasDeTamanoN(1, pref))
        .find(o)
    }

    val t1 = task {
      explorarConPrefijos(primerMitad)
    }
    val t2 = task {
      explorarConPrefijos(segundaMitad)
    }
    t1.join().getOrElse(t2.join().getOrElse(Seq()))
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    ???
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    ???
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    ???
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    // Usa paralelismo de tareas y/o datos
    ???
  }
}
