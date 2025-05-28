error id: file:///C:/Users/felip/Documents/ScalaProjectsVS/Proyecto_PFC_Reconstruccion_de_cadenas/src/main/scala/ReconstCadenasPar/package.scala:`<none>`.
file:///C:/Users/felip/Documents/ScalaProjectsVS/Proyecto_PFC_Reconstruccion_de_cadenas/src/main/scala/ReconstCadenasPar/package.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -common/umbral.
	 -common/umbral#
	 -common/umbral().
	 -java/util/concurrent/umbral.
	 -java/util/concurrent/umbral#
	 -java/util/concurrent/umbral().
	 -scala/collection/parallel/CollectionConverters.umbral.
	 -scala/collection/parallel/CollectionConverters.umbral#
	 -scala/collection/parallel/CollectionConverters.umbral().
	 -Oraculo.umbral.
	 -Oraculo.umbral#
	 -Oraculo.umbral().
	 -ArbolSufijos.umbral.
	 -ArbolSufijos.umbral#
	 -ArbolSufijos.umbral().
	 -umbral.
	 -umbral#
	 -umbral().
	 -scala/Predef.umbral.
	 -scala/Predef.umbral#
	 -scala/Predef.umbral().
offset: 3385
uri: file:///C:/Users/felip/Documents/ScalaProjectsVS/Proyecto_PFC_Reconstruccion_de_cadenas/src/main/scala/ReconstCadenasPar/package.scala
text:
```scala
import common.*
import java.util.concurrent._
import scala.util.DynamicVariable
import scala.collection.parallel.CollectionConverters._
import Oraculo.*
import ArbolSufijos.*

import scala.collection.parallel.ParSeq
package object ReconstCadenasPar {
  // Ahora versiones paralelas
  
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas
    def cadenasDeTamanoN(contador: Int, tamano: Int, combParcial: LazyList[Seq[Char]]): LazyList[Seq[Char]] = {
      if (contador == tamano) combParcial
      else {
        val nuevaCombParcial = for {
          comb <- combParcial
          letra <- alfabeto
        } yield comb :+ letra
        cadenasDeTamanoN(contador + 1, tamano, nuevaCombParcial)
      }
    }

    val base = cadenasDeTamanoN(0, umbral, LazyList(Seq()))
    val tareas = (for{
      comb <- base
    }yield task{cadenasDeTamanoN(umbral, n, LazyList(comb)).find(o)}).toList

    def buscar(tareas: List[ForkJoinTask[Option[Seq[Char]]]]): Seq[Char] = tareas match{
      case x :: xs => x.join().getOrElse(buscar(xs))
    }
    buscar(tareas)
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y de datos según el tamaño del conjunto
    def construirCandidatos(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) Seq.empty // no encontrado (no debería pasar)
      else {
        // 1) Generar todas las extensiones
        val ext: Seq[Seq[Char]] =
          if (candidatos.size * alfabeto.size <= umbral) {
            for {
              prefijo <- candidatos
              c <- alfabeto
            } yield prefijo :+ c
          } else {
            candidatos.par
              .flatMap(prefijo => alfabeto.map(c => prefijo :+ c))
              .toList
          }

        // 2) Filtrar con el oráculo
        val filtrados: Seq[Seq[Char]] =
          if (ext.size <= umbral) {
            ext.filter(o)
          } else {
            ext.par
              .filter(o)
              .toList
          }

        // 3) Si alguno ya tiene longitud n, devolverlo
        val completados = filtrados.filter(_.length == n)
        if (completados.nonEmpty) completados.head
        else
          // 4) Continuar con la siguiente longitud
          construirCandidatos(k + 1, filtrados)
      }
    }

    // inicio con SC₀ = Seq(Seq.empty)
    construirCandidatos(1, Seq(Seq.empty))
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
  require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

  @annotation.tailrec
  def iterar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
    if (k == n) {
      candidatos.find(_.length == n) match {
        case null => Seq.empty
        case sol if sol != null => sol
      }
    } else {
      val parCandidatos = if (candidatos.size > umb@@ral) candidatos.par else candidatos

      val combinaciones = parCandidatos.flatMap { s1 =>
        val inner = if (candidatos.size > umbral/2) candidatos.par else candidatos
        inner.collect {
          case s2 if (s1 ++ s2).length == k*2 => s1 ++ s2
        }
      }.distinct.seq

      val validas = if (combinaciones.size > umbral) {
        combinaciones.par.filter(o).seq
      } else {
        combinaciones.filter(o)
      }

      val solucion = validas.find(_.length == n)
      if (solucion != null) solucion
      else iterar(k * 2, validas)
    }
  }

  val iniciales = if (alfabeto.size > umbral) {
    alfabeto.par.map(Seq(_)).filter(o).seq
  } else {
    alfabeto.view.map(Seq(_)).filter(o).toSeq
  }

  if (n == 1) {
    val res = iniciales.find(_.length == 1)
    if (res != null) res else Seq.empty
  } else {
    iterar(1, iniciales)
  }
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

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.