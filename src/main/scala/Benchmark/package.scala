import ReconstCadenas._
import Oraculo._
import scala.collection.parallel.CollectionConverters._
import org.scalameter._
package object Benchmark {
  /*
-----------------------------------------------------------------------------------------
------------------ PARA GUIARNOS DEL BENCHMARK DEL TALLER ANTERIOR ----------------------
-----------------------------------------------------------------------------------------

*/
  type AlgoritmoIngenuo = (Int, Oraculo) => Seq[Char]


  def compararAlgoritmos(a1:AlgoritmoIngenuo, a2:AlgoritmoIngenuo)
                        (tamano: Int, oraculo: Oraculo):(Double,Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a1(tamano,oraculo))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (a2(tamano,oraculo))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
  
  /*


  def compararMultMatriz(n:Int) = {
    // Para probar con paralelismo de datos
    val m1= matrizAlAzar(n,2)
    val m2= matrizAlAzar(n,2)
    val m1parD= transformToParD(m1)
    val m2parD= transformToParD(m2)
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {multMatriz(m1, m2)}

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {multMatrizParD(m1parD,m2parD)}
    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }

  def compararProdPunto(n:Int) = {
    // Para probar con paralelismo de datos
    val v1= vectorAlAzar(n,2)
    val v2= vectorAlAzar(n,2)
    val v1parD= v1.par
    val v2parD= v2.par
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {prodPunto(v1,v2)}

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure {prodPuntoParD(v1parD,v2parD)}
    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
 */
}
