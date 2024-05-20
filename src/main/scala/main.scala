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

    val itsEscalasCurso = itinerariosEscalas(vuelosCurso,aeropuertosCurso)
    val itsc1 = itsEscalasCurso("MID", "SVCS")
    val itsc2 = itsEscalasCurso("CLO", "SVCS")
    val itsc3 = itsEscalasCurso("CLO","SVO")
    val itsc4 = itsEscalasCurso("CLO", "MEX")
    val itsc5 = itsEscalasCurso("CTG","PTY")

    
  }
}