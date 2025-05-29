import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {
  
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
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
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    def construirCandidatos(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k > n) Seq.empty  // caso base: no lo encontró (no debería suceder) cadena vacia 
      else {
        // 1) extender cada candidato con cada letra
        val ext = for {
          prefijo <- candidatos
          c       <- alfabeto
        } yield prefijo :+ c

        // 2) filtrar con el oráculo
        val filtrados = ext.filter(o)

        // 3) si alguno ya tiene longitud n, devolverlo
        val completos = filtrados.filter(_.length == n)
        if (completos.nonEmpty) completos.head
        else
          // 4) seguir con k+1 y los nuevos candidatos
          construirCandidatos(k + 1, filtrados)
      }
    }

    // arrancamos con k = 1 y SC₀ = Seq(Seq.empty)
    construirCandidatos(1, Seq(Seq.empty))
  }


  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
      // Verificamos que n sea potencia de 2 (requisito del algoritmo turbo)
    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")
  
    def iterar(k: Int, candidatos: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n) candidatos.headOption.getOrElse(Seq())
      else {
        val combinaciones = candidatos.flatMap { s1 =>
          candidatos.map {
            s2 => s1 ++ s2
          }
        }
        val validas = combinaciones.filter(o)
        iterar(k * 2, validas)
      }
    }
    val iniciales: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterar(1, iniciales)
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido

    require((n & (n - 1)) == 0 && n > 0, "La longitud debe ser potencia de dos")

    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      // 1) todas las concatenaciones s1 ++ s2
      val pares: Seq[Seq[Char]] =
        for {
          s1 <- sc
          s2 <- sc
        } yield s1 ++ s2

      // 2) para cada candidato s, comprobar todas sus subcadenas de longitud k
      pares.filter { s =>
        // indices validos de subcadena
        val maxStart = s.length - k
        // generar cada subcadena
        (0 to maxStart).forall { i =>
          val sub = s.drop(i).take(k)
          sc.contains(sub)
        }
      }
    }

    def iterarTamanos(k: Int, sc: Seq[Seq[Char]]): Seq[Char] = {
      if (k == n)
        sc.headOption.getOrElse(Seq.empty)
      else {
        //  SC_k = Filtrar(SC_{k/2}, k/2)
        val candidatos = filtrar(sc, k / 2)
        //  preguntar oraculo solo sobre estos candidatos
        val validas = candidatos.filter(o)

        iterarTamanos(k * 2, validas)
      }
    }

    // arranca con SC₁ = alfabeto como secuencias de longitud 1
    val inicial: Seq[Seq[Char]] = alfabeto.map(c => Seq(c))
    iterarTamanos(1, inicial)
  }


  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oráculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    ???
  }

}