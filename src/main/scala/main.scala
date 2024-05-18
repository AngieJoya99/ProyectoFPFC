
/*object Main {
  def main(args: Array[String]): Unit = {
    /*import Itinerarios._
    import ItinerariosPar._*/
    import Datos._
    import common._

    case class Aeropuerto (Cod: String , X: Int , Y: Int , GMT: Int )
    case class Vuelo (Aln: String , Num: Int , Org: String , HS: Int , MS: Int , Dst: String , HL: Int , ML: Int , Esc: Int )
    type Itinerario = List[Vuelo]

    def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
      def buscarItinerarios(cod1: String, cod2: String): List[Itinerario] = {
        def generarItinerario(cod1: String, cod2: String, visitados: Set[String], vuelosRestantes: List[Vuelo]): List[Itinerario] = vuelosRestantes.flatMap {
          case vuelo if vuelo.Org == cod1 && vuelo.Dst == cod2 && !visitados.contains(vuelo.Dst) =>
            List(List(vuelo))
          case vuelo if vuelo.Org == cod1 && !visitados.contains(vuelo.Dst) =>
            val nuevosVisitados = visitados + vuelo.Org
            generarItinerario(vuelo.Dst, cod2, nuevosVisitados, vuelos.filterNot(_ == vuelo)).map(vuelo :: _)
          case _ => None
        }

        generarItinerario(cod1, cod2, Set.empty, vuelos)
      }

      buscarItinerarios
    }

    val itsCurso = itinerarios(vuelosCurso,aeropuertosCurso)
    
    
  }
}*/