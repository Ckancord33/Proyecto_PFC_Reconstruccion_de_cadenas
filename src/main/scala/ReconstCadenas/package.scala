import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

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

    cadenasDeTamanoN(0, n, LazyList(Seq())).find(o).getOrElse(Seq())
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    def construirCandidatos(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) candidatos.headOption.getOrElse(Seq.empty)
      else {
        val filtrados = for {
          prefijo <- candidatos
          c       <- alfabeto
          combinacion = prefijo :+ c
          if o(combinacion)
        } yield combinacion
        construirCandidatos(k + 1, filtrados)
      }
    }

    construirCandidatos(0, Seq(Seq.empty))
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def iterar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) candidatos.headOption.getOrElse(Seq())
      else {
        val validas = for {
          s1 <- candidatos
          s2 <- candidatos
          combinacion = s1 ++ s2
          if(o(combinacion))
        } yield combinacion
        iterar(k * 2, validas)
      }
    }

    val iniciales: Seq[Seq[Char]] = alfabeto.map(c => Seq(c)).filter(o)
    iterar(1, iniciales)
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        for {
          s1 <- sc
          s2 <- sc
          combinacion = s1 ++ s2
          if (0 to k).forall { i =>
            val sub = combinacion.slice(i, i + k)
            sc.contains(sub)
          }
        } yield combinacion
    }

    def iterarTamanos(k: Int, sc: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) sc.headOption.getOrElse(Seq.empty)
      else {
        val candidatos = filtrar(sc, k)
        val validas = candidatos.filter(o)
        iterarTamanos(k * 2, validas)
      }
    }

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterarTamanos(1, inicial)
  }


  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val arbolSc: Trie = arbolDeSufijos(sc)
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

    def iterarTamanos(k: Int, sc: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n)
        sc.headOption.getOrElse(Seq.empty)
      else {
        val validas = filtrar(sc, k)
        iterarTamanos(k * 2, validas)
      }
    }

    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterarTamanos(1, inicial)
  }

}