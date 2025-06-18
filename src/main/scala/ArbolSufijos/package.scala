package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
    s match{
      case Nil => t match{
        case Nodo(_, marcada, _) => marcada
        case Hoja(_, marcada) => marcada
      }
      case x +: xs =>
        def buscarEnHijos(hijos: List[Trie]): Boolean = hijos match {
          case Nil => false
          case h :: hs =>
            if (raiz(h) == x) pertenece(xs, h)
            else buscarEnHijos(hs)
        }
        t match {
          case Nodo(_, _, hijos) => buscarEnHijos(hijos)
          case _ => false
        }
    }
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Adiciona una secuencia de uno o más caracteres a un trie
    s match {
      case Nil => t match {
        case Nodo(car, marcada, hijos) => Nodo(car, true, hijos)
        case Hoja(car, marcada) => Hoja(car, true)
      }
      case x +: xs =>
        t match {
          case Nodo(car, marcada, hijos) => if (!cabezas(t).contains(x)) Nodo(car, marcada, adicionar(xs, Hoja(x, false)) :: hijos)
            else{
              def reemplazarHijo(hijos: List[Trie]): List[Trie] = hijos match{
                case Nil => Nil
                case h :: hs =>
                  if (raiz(h) == x) adicionar(xs, h) :: hs
                  else h :: reemplazarHijo(hs)
              }
              Nodo(car, marcada, reemplazarHijo(hijos))
            }
          case Hoja(car, marcada) => Nodo(car, marcada, List(adicionar(xs, Hoja(x, false))))
        }
    }
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // Dada una secuencia no vacía de secuencias, devuelve el árbol de sufijos asociado a esas secuencias
    def agregarSufijos(s: Seq[Char], t: Trie): Trie = s match {
      case Nil => adicionar(s, t)
      case x +: xs => agregarSufijos(xs, adicionar(s, t))
    }
    def crearArbolDeSufijos(ss: Seq[Seq[Char]], t: Trie): Trie = ss match {
      case Nil => t
      case x +: xs => crearArbolDeSufijos(xs, agregarSufijos(x, t))
    }
    crearArbolDeSufijos(ss, Nodo(' ', false, Nil))
  }
}