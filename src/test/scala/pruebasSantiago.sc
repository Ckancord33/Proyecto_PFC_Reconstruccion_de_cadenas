import Benchmark.compararAlgoritmos
import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._
import scala.util.Random
import org.scalameter.*

// ————————————————————————————————
// Generador de secuencias aleatorias
// ————————————————————————————————
val random = new Random()
def secAlAzar(long: Int, s: Seq[Char]): Seq[Char] =
  if (s.length == long) s
  else secAlAzar(long, alfabeto(random.nextInt(alfabeto.length)) +: s)

// ————————————————————————————————
// Parámetros iniciales predeterminado
// ————————————————————————————————
val costoOraculo = 1





/*
-----------------------------------------------------------------------------------------
           ------------------ SOLUCION MEJORADA  ----------------------
-----------------------------------------------------------------------------------------
*/
// ————————————————————————————————
// Ejecuciones individuales versión secuencial
// ————————————————————————————————

// PRUEBA N°1: 
val ps1: Seq[Char] = for {i <- 1 to 64} yield alfabeto(Random.nextInt(4))
val po1: Oraculo       = crearOraculo(costoOraculo)(ps1)
reconstruirCadenaMejorado(ps1.length, po1)

// PRUEBA N°2: 
val ps2: Seq[Char] = for {i <- 1 to 128} yield alfabeto(Random.nextInt(4))
val po2: Oraculo       = crearOraculo(costoOraculo)(ps2)
reconstruirCadenaMejorado(ps2.length, po2)

// PRUEBA N°3: 
val ps3: Seq[Char] = for {i <- 1 to 256} yield alfabeto(Random.nextInt(4))
val po3: Oraculo       = crearOraculo(costoOraculo)(ps3)
reconstruirCadenaMejorado(ps3.length, po3)

// PRUEBA N°4: 
val ps4: Seq[Char] = for {i <- 1 to 512} yield alfabeto(Random.nextInt(4))
val po4: Oraculo       = crearOraculo(costoOraculo)(ps4)
reconstruirCadenaMejorado(ps4.length, po4)

// PRUEBA N°5: 
val ps5: Seq[Char] = for {i <- 1 to 1024} yield alfabeto(Random.nextInt(4))
val po5: Oraculo       = crearOraculo(costoOraculo)(ps5)
reconstruirCadenaMejorado(ps5.length, po5)


// ————————————————————————————————
// Ejecuciones individuales versión paralela
// ————————————————————————————————

// PRUEBA N°1
val up1 = 16
reconstruirCadenaMejoradoPar(up1)(ps1.length, po1)

// PRUEBA N°2
val up2 = 32
reconstruirCadenaMejoradoPar(up2)(ps2.length, po2)

// PRUEBA N°3
val up3 = 64
reconstruirCadenaMejoradoPar(up3)(ps3.length, po3)

// PRUEBA N°4
val up4 = 128
reconstruirCadenaMejoradoPar(up4)(ps4.length, po4)

// PRUEBA N°5
val up5 = 256
reconstruirCadenaMejoradoPar(up5)(ps5.length, po5)
/*
-----------------------------------------------------------------------------------------
    ------------------ COMPARATIVAS DE LA SOLUCION MEJORADA  ----------------------
-----------------------------------------------------------------------------------------
*/

// ————————————————————————————————————————————————————————————————
// Comparativa de umbrales en la versión paralela
// ————————————————————————————————————————————————————————————————

/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaMejoradoPar
 ****************************************************************/

// PRUEBA N°1
val um1  = 16
val ug1  = 32
val resPP1 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um1),
  reconstruirCadenaMejoradoPar(ug1)
)(ps1.length, po1)

// PRUEBA N°2
val um2  = 64
val ug2  = 128
val resPP2 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um2),
  reconstruirCadenaMejoradoPar(ug2)
)(ps2.length, po2)

// PRUEBA N°3
val um3  = 256
val ug3  = 512
val resPP3 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um3),
  reconstruirCadenaMejoradoPar(ug3)
)(ps3.length, po3)

// PRUEBA N°4
val um4  = 512
val ug4  = 1024
val resPP4 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um4),
  reconstruirCadenaMejoradoPar(ug4)
)(ps4.length, po4)

// PRUEBA N°5
val um5  = 1024
val ug5  = 1024
val resPP5 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um5),
  reconstruirCadenaMejoradoPar(ug5)
)(ps5.length, po5)

/*
// TARDA DEMASIADO EN CUESTION DE HORAS
/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaIngenuoPar
 ****************************************************************/

// PRUEBA N°1
val umi1  = 2
val ugi1  = 4
val resPI1 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umi1),
  reconstruirCadenaIngenuoPar(ugi1)
)(ps1.length, po1)

// PRUEBA N°2
val umi2  = 4
val ugi2  = 8
val resPI2 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umi2),
  reconstruirCadenaIngenuoPar(ugi2)
)(ps2.length, po2)

// PRUEBA N°3
val umi3  = 4
val ugi3  = 2
val resPI3 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umi3),
  reconstruirCadenaIngenuoPar(ugi3)
)(ps3.length, po3)

// PRUEBA N°4
val umi4  = 8
val ugi4  = 1
val resPI4 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umi4),
  reconstruirCadenaIngenuoPar(ugi4)
)(ps4.length, po4)

// PRUEBA N°5
val umi5  = 13
val ugi5  = 4
val resPI5 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umi5),
  reconstruirCadenaIngenuoPar(ugi5)
)(ps5.length, po5)

*/
/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaTurboPar
 ****************************************************************/

// PRUEBA N°1
val umt1  = 32
val ugt1  = 64
val resPT1 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umt1),
  reconstruirCadenaTurboPar(ugt1)
)(ps1.length, po1)

// PRUEBA N°2
val umt2  = 64
val ugt2  = 128
val resPT2 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umt2),
  reconstruirCadenaTurboPar(ugt2)
)(ps2.length, po2)

// PRUEBA N°3
val umt3  = 128
val ugt3  = 128
val resPT3 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umt3),
  reconstruirCadenaTurboPar(ugt3)
)(ps3.length, po3)

// PRUEBA N°4
val umt4  = 64
val ugt4  = 512
val resPT4 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umt4),
  reconstruirCadenaTurboPar(ugt4)
)(ps4.length, po4)

// PRUEBA N°5
val umt5  = 32
val ugt5  = 512
val resPT5 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(umt5),
  reconstruirCadenaTurboPar(ugt5)
)(ps5.length, po5)

/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaTurboMejoradaPar
 ****************************************************************/

// PRUEBA N°1
val utm1  = 8
val utgTM1 = 16
val resMTM1 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um1),
  reconstruirCadenaTurboMejoradaPar(utm1)
)(ps1.length, po1)

// PRUEBA N°2
val utm2  = 32
val utgTM2 = 32
val resMTM2 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um2),
  reconstruirCadenaTurboMejoradaPar(utm2)
)(ps2.length, po2)

// PRUEBA N°3
val utm3  = 64
val utgTM3 = 256
val resMTM3 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um3),
  reconstruirCadenaTurboMejoradaPar(utm3)
)(ps3.length, po3)

// PRUEBA N°4
val utm4  = 64
val utgTM4 = 1024
val resMTM4 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um4),
  reconstruirCadenaTurboMejoradaPar(utm4)
)(ps4.length, po4)

// PRUEBA N°5
val utm5  = 128
val utgTM5 = 1024
val resMTM5 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um5),
  reconstruirCadenaTurboMejoradaPar(utm5)
)(ps5.length, po5)


/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaTurboAceleradaPar
 ****************************************************************/

// PRUEBA N°1
val ua1  = 8
val ugA1 = 64
val resMTA1 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um1),
  reconstruirCadenaTurboAceleradaPar(ua1)
)(ps1.length, po1)

// PRUEBA N°2
val ua2  = 128
val ugA2 = 32
val resMTA2 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um2),
  reconstruirCadenaTurboAceleradaPar(ua2)
)(ps2.length, po2)

// PRUEBA N°3
val ua3  = 128
val ugA3 = 256
val resMTA3 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um3),
  reconstruirCadenaTurboAceleradaPar(ua3)
)(ps3.length, po3)

// PRUEBA N°4
val ua4  = 64
val ugA4 = 1024
val resMTA4 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um4),
  reconstruirCadenaTurboAceleradaPar(ua4)
)(ps4.length, po4)

// PRUEBA N°5
val ua5  = 128
val ugA5 = 512
val resMTA5 = compararAlgoritmos(
  reconstruirCadenaMejoradoPar(um5),
  reconstruirCadenaTurboAceleradaPar(ua5)
)(ps5.length, po5)


// ————————————————————————————————————————————————————————————————
// Comparativa de alternativas en la versión secuencial
// ————————————————————————————————————————————————————————————————

/****************************************************************
 reconstruirCadenaMejorado vs reconstruirCadenaMejorado
 ****************************************************************/

// PRUEBA N°1

val resMM1 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaMejorado
)(ps1.length, po1)

// PRUEBA N°2

val resMM2 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaMejorado
)(ps2.length, po2)

// PRUEBA N°3

val resMM3 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaMejorado
)(ps3.length, po3)

// PRUEBA N°4

val resMM4 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaMejorado
)(ps4.length, po4)

// PRUEBA N°5

val resMM5 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaMejorado
)(ps5.length, po5)

/*
// TARDA DEMASIADO EN CUESTION DE HORAS CON  CANTIDADES MUY GRANDES
/****************************************************************
 reconstruirCadenaMejoradoPar vs reconstruirCadenaIngenuoPar
 ****************************************************************/

// PRUEBA N°1
val nmMI1  = 8
val smMI1  = secAlAzar(nmMI1, Seq())
val omMI1  = crearOraculo(costoOraculo)(smMI1)
val resMI1 = compararAlgoritmos(
  reconstruirCadenaIngenuo,
  reconstruirCadenaMejorado
)(nmMI1, omMI1)


// PRUEBA N°2

val nmMI2  = 8
val smMI2  = secAlAzar(nmMI2, Seq())
val omMI2  = crearOraculo(costoOraculo)(smMI2)
val resMI2 = compararAlgoritmos(
  reconstruirCadenaIngenuo,
  reconstruirCadenaMejorado
)(nmMI2, omMI2)

// PRUEBA N°3

val nmMI3  = 4
val smMI3  = secAlAzar(nmMI3, Seq())
val omMI3  = crearOraculo(costoOraculo)(smMI3)
val resMI3 = compararAlgoritmos(
  reconstruirCadenaIngenuo,
  reconstruirCadenaMejorado
)(nmMI3, omMI3)

// PRUEBA N°4

val nmMI4  = 2
val smMI4  = secAlAzar(nmMI4, Seq())
val omMI4  = crearOraculo(costoOraculo)(smMI4)
val resMI4 = compararAlgoritmos(
  reconstruirCadenaIngenuo,
  reconstruirCadenaMejorado
)(nmMI4, omMI4)

// PRUEBA N°5
val nmMI5  = 13
val smMI5  = secAlAzar(nmMI5, Seq())
val omMI5  = crearOraculo(costoOraculo)(smMI5)
val resMI5 = compararAlgoritmos(
  reconstruirCadenaIngenuo,
  reconstruirCadenaMejorado
)(nmMI5, omMI5)


 */

/****************************************************************
 reconstruirCadenaMejorado vs reconstruirCadenaTurbo
 ****************************************************************/

// PRUEBA N°1

val resMT1 = compararAlgoritmos(
  reconstruirCadenaTurbo,
  reconstruirCadenaMejorado
)(ps1.length, po1)

// PRUEBA N°2

val resMT2 = compararAlgoritmos(
  reconstruirCadenaTurbo,
  reconstruirCadenaMejorado
)(ps2.length, po2)


// PRUEBA N°3

val resMT3 = compararAlgoritmos(
  reconstruirCadenaTurbo,
  reconstruirCadenaMejorado
)(ps3.length, po3)

// PRUEBA N°4

val resMT4 = compararAlgoritmos(
  reconstruirCadenaTurbo,
  reconstruirCadenaMejorado
)(ps4.length, po4)

// PRUEBA N°5

val resMT5 = compararAlgoritmos(
  reconstruirCadenaTurbo,
  reconstruirCadenaMejorado
)(ps5.length, po5)


/****************************************************************
 reconstruirCadenaMejorado vs reconstruirCadenaTurboMejorada
 ****************************************************************/

// PRUEBA N°1

val rTM1 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboMejorada
)(ps1.length, po1)

// PRUEBA N°2

val rTM2 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboMejorada
)(ps2.length, po2)

// PRUEBA N°3

val rTM3 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboMejorada
)(ps3.length, po3)

// PRUEBA N°4

val rTM4 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboMejorada
)(ps4.length, po4)

// PRUEBA N°5

val rTM5 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboMejorada
)(ps5.length, po5)


/****************************************************************
 reconstruirCadenaMejorado vs reconstruirCadenaTurboAcelerada
 ****************************************************************/

// PRUEBA N°1

val rTdA1 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboAcelerada
)(ps1.length, po1)

// PRUEBA N°2

val rTdA2 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboAcelerada
)(ps2.length, po2)

// PRUEBA N°3

val rTdA3 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboAcelerada
)(ps3.length, po3)

// PRUEBA N°4

val rTdA4 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboAcelerada
)(ps4.length, po4)

// PRUEBA N°5

val rTdA5 = compararAlgoritmos(
  reconstruirCadenaMejorado,
  reconstruirCadenaTurboAcelerada
)(ps5.length, po5)


