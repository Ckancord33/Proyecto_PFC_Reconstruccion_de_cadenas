import common.*
import java.util.concurrent._
import scala.util.DynamicVariable
import scala.collection.parallel.CollectionConverters._
import Oraculo.*
import ArbolSufijos.*
import scala.collection.parallel.ParSeq

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
      // Versión paralela de find para grandes conjuntos
      if (candidatos.size > umbral) {
        candidatos.par.find(_.length == n).getOrElse(Seq.empty)
      } else {
        candidatos.find(_.length == n).getOrElse(Seq.empty)
      }
    } else {
      // Decidir si paralelizar basado en el umbral
      val combinaciones = if (candidatos.size > umbral) {
        val parCandidatos = candidatos.par
        parCandidatos.flatMap { s1 =>
          parCandidatos.collect {
            case s2 if (s1 ++ s2).length == k*2 => s1 ++ s2
          }
        }.distinct.seq
      } else {
        candidatos.flatMap { s1 =>
          candidatos.collect {
            case s2 if (s1 ++ s2).length == k*2 => s1 ++ s2
          }
        }.distinct
      }

      // Filtrado paralelo para grandes conjuntos
      val validas = if (combinaciones.size > umbral) {
        combinaciones.par.filter(o).seq
      } else {
        combinaciones.filter(o)
      }

      // Verificación paralela de existencia
      if (validas.size > umbral) {
        if (validas.par.exists(_.length == n)) validas.par.find(_.length == n).get
        else iterar(k * 2, validas)
      } else {
        if (validas.exists(_.length == n)) validas.find(_.length == n).get
        else iterar(k * 2, validas)
      }
    }
  }

  // Generación inicial paralela para alfabetos grandes
  val iniciales = if (alfabeto.size > umbral) {
    alfabeto.par.map(Seq(_)).filter(o).seq
  } else {
    alfabeto.view.map(Seq(_)).filter(o).toSeq
  }

  if (n == 1) {
    if (iniciales.size > umbral) iniciales.par.find(_.length == 1).getOrElse(Seq.empty)
    else iniciales.find(_.length == 1).getOrElse(Seq.empty)
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
