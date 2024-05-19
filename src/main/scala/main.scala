import common._
import Itinerarios._
import ItinerariosPar._
import Datos._

object Main {
  def main(args: Array[String]): Unit = {
    val v = vuelosCurso
    val a = aeropuertosCurso
    val itsCurso = itinerarios(v,a)
    //println(itsCurso)    
  }
}