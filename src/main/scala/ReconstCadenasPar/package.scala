import common.*
import java.util.concurrent._
import scala.util.DynamicVariable
import scala.collection.parallel.CollectionConverters._
import Oraculo.*
import ArbolSufijos.*
import scala.collection.parallel.ParSeq

import scala.collection.parallel.ParSeq
package object ReconstCadenasPar {
  
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

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
      case Nil => Seq()
      case x :: xs => x.join().getOrElse(buscar(xs))
    }

    buscar(tareas)
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    def construirCandidatosPar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) candidatos.headOption.getOrElse(Seq.empty)
      else {
        val filtrados: Seq[Seq[Char]] =
          if (candidatos.size * alfabeto.size <= umbral) {
            for {
              prefijo <- candidatos
              c <- alfabeto
              combinacion = prefijo :+ c
              if o(combinacion)
            } yield combinacion
          } else {
            candidatos.par.flatMap { prefijo =>
              alfabeto.par.flatMap { c =>
                val combinacion = prefijo :+ c
                if (o(combinacion)) Seq(combinacion) else Nil
              }
            }.toList
          }
        construirCandidatosPar(k + 1, filtrados)
      }
    }

    construirCandidatosPar(0, Seq(Seq.empty))

  }

def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
  require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

  def construirCandidatos(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
    if (k == n) candidatos.headOption.getOrElse(Seq())
    else {
      // Decidir si paralelizar basado en el umbral
      val combinaciones = if (k <= umbral) {
        val parCandidatos = candidatos.par
        parCandidatos.flatMap { s1 =>
          parCandidatos.map {
            s2 => s1 ++ s2
          }
        }.seq
      } else {
        candidatos.flatMap { s1 =>
          candidatos.collect {
            s2 => s1 ++ s2
          }
        }
      }

      // Filtrado paralelo para grandes conjuntos
      val validas = if (k * 2 <= umbral) {
        combinaciones.par.filter(o).seq
      } else {
        combinaciones.filter(o)
      }

      // Verificación paralela de existencia
      construirCandidatos(k*2, validas)
    }
  }
  val iniciales: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
  construirCandidatos(1, iniciales)
}

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (sc.size <= umbral) {
        for {
          s1 <- sc
          s2 <- sc
          combinacion = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = combinacion.slice(i, i + k)
            sc.contains(sub)
          }
        } yield combinacion

      } else {
        sc.par.flatMap { s1 =>
          sc.par.flatMap { s2 =>
            val combinacion = s1 ++ s2
            val todasSubcadenasPresentes = (0 to k).forall { i =>
              val sub = combinacion.slice(i, i + k)
              sc.contains(sub)
            }
            if (todasSubcadenasPresentes) Seq(combinacion) else Nil
          }
        }.toList
      }
    }

    def iterarTamanosPar(k: Int, sc: Seq[Seq[Char]]): Seq[Char] =
      if (k == n) sc.headOption.getOrElse(Seq.empty)
      else {
        val candidatos = filtrar(sc, k)
        val validas =
          if (candidatos.size < umbral) candidatos.filter(o)
          else candidatos.par.filter(o).toList

        iterarTamanosPar(k * 2, validas)
      }

    val inicial =
      if (alfabeto.size < umbral) alfabeto.map(c => Seq(c))
      else alfabeto.par.map(c => Seq(c)).toList

    iterarTamanosPar(1, inicial)

  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val arbolSc: Trie = arbolDeSufijos(sc)
      if (k <= umbral) {
        val parSc = sc.par
        (for {
          s1 <- parSc
          s2 <- parSc
          combinacion = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = combinacion.slice(i, i + k)
            pertenece(sub, arbolSc)
          }
          if o(combinacion)
        } yield combinacion).seq
      } else {
        for {
          s1 <- sc
          s2 <- sc
          combinacion = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = combinacion.slice(i, i + k)
            pertenece(sub, arbolSc)
          }
          if o(combinacion)
        } yield combinacion
      }
    }

    def iterarTamanos(k: Int, sc: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) sc.headOption.getOrElse(Seq.empty)
      else {
        val validas = filtrar(sc, k)
        iterarTamanos(k * 2, validas)
      }
    }

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterarTamanos(1, inicial)
  }
}
