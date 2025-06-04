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
            candidatos.flatMap(prefijo =>
              alfabeto.map(c => prefijo :+ c)
            )
          } else {
            // paralelismo de datos para conjuntos grandes
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
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val pares =
        if (k <= umbral) {
          val parCandidatos = sc.par
          parCandidatos.flatMap { s1 =>
            parCandidatos.map { s2 => s1 ++ s2 }
          }.seq
        } else {
          sc.flatMap { s1 =>
            sc.map { s2 => s1 ++ s2 }
          }
        }

      val filtradas =
        if (k <= umbral) {
          pares.par.filter { s =>
            val maxStart = s.length - k
            (0 to maxStart).forall { i =>
              val sub = s.slice(i, i + k)
              sc.contains(sub)
            }
          }.seq
          
        } else {
          pares.filter { s =>
            val maxStart = s.length - k
            (0 to maxStart).forall { i =>
              val sub = s.slice(i, i + k)
              sc.contains(sub)
            }
          }
        }

      filtradas
    }

    def iterarTamanos(k: Int, sc: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n)
        sc.headOption.getOrElse(Seq.empty)
      else {
        val candidatos = filtrar(sc, k)

        val validas =
          if (k * 2 <= umbral)
            candidatos.par.filter(o).seq
          else
            candidatos.filter(o)

        iterarTamanos(k * 2, validas)
      }
    }

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterarTamanos(1, inicial)
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
