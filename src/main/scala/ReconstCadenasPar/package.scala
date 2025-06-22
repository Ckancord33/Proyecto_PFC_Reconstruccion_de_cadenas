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

    def construirCandidatos(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) candidatos.headOption.getOrElse(Seq.empty)
      else {
        val filtrados: Seq[Seq[Char]] =
          if (k <= umbral) {
            candidatos.par.flatMap { prefijo =>
              alfabeto.par.flatMap { c =>
                val combinacion = prefijo :+ c
                if (o(combinacion)) Seq(combinacion) else Nil
              }
            }.toList
          } else {
            for {
              prefijo <- candidatos
              c <- alfabeto
              combinacion = prefijo :+ c
              if o(combinacion)
            } yield combinacion
          }

        construirCandidatos(k + 1, filtrados)
      }
    }

    construirCandidatos(0, Seq(Seq.empty))

  }

def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
  require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

  def iterar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
    if (k == n) candidatos.headOption.getOrElse(Seq())
    else {
      val validas = if (k <= umbral) {
        val parCandidatos = candidatos.par
        (for {
          s1 <- parCandidatos
          s2 <- parCandidatos
          combinacion = s1 ++ s2
          if (o(combinacion))
        } yield combinacion).seq
      }
      else {
        for {
          s1 <- candidatos
          s2 <- candidatos
          combinacion = s1 ++ s2
          if (o(combinacion))
        } yield combinacion
      }
      iterar(k * 2, validas)
    }
  }

  val iniciales: Seq[Seq[Char]] = alfabeto.map(c => Seq(c)).filter(o)
  iterar(1, iniciales)
}


  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (k <= umbral) {
        val parSc = sc.par
        (for {
          s1 <- parSc
          s2 <- parSc
          combinacion = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = combinacion.slice(i, i + k)
            sc.contains(sub)
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
            sc.contains(sub)
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

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c)).filter(o)
    iterarTamanos(1, inicial)
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

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

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c)).filter(o)
    iterarTamanos(1, inicial)
  }
}
