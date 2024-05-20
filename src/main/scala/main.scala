import common._
import Itinerarios._
//import ItinerariosPar._
import Datos._

object Main {
  def main(args: Array[String]): Unit = {
    val itsTiempoCurso = itinerariosTiempo(vuelosCurso,aeropuertosCurso)
    val itst1 = itsTiempoCurso("MID", "SVCS")
    val itst2 = itsTiempoCurso("CLO", "SVCS")
    val itst3 = itsTiempoCurso("CLO","SVO")
    val itst4 = itsTiempoCurso("CLO", "MEX")
    val itst5 = itsTiempoCurso("CTG", "PTY")
    println(itst3)
    println(itst4)
  }
}