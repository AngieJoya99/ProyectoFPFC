/*Angie Joya - 2322609
Johan David Pitto - 1932739
Miguel Ángel Salcedo - 2242786
José Daniel Trujillo - 2225611*/

package object Itinerarios{

  case class Aeropuerto (Cod: String , X: Int , Y: Int , GMT: Int )
  /*(Cod:Código del Aeropuerto, X:Coordenada en X, Y:Coordenada en Y, 
  GMT: Entero que representa la hora)*/

  case class Vuelo (Aln: String , Num: Int , Org: String , HS: Int , MS: Int , Dst: String , HL: Int , ML: Int , Esc: Int )
  /*(Aln: Nombre Aerolínea, Num: Número de Vuelo, Org: Código de Aeropuerto de Origen,
  HS: Hora (hh) local de salida, MS: Minutos (mm) de hora local de salida,
  Dst: Código de Aeropuerto de destino, HL: Hora (hh) local de llegada, 
  ML: Minutos (mm) de hora local de llegada, Esc: Número de escalas (mismo avión))*/
  
  type Itinerario = List[Vuelo]

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula todos
    * los itinearios entre 2 aerpuertos que recibe como parámetro
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto cod1 y cod2, y retorna
    * todos los itinerarios entre los dos aeropuertos
    */
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def generarItinerario(cod1: String, cod2: String, visitados: Set[String], vuelosRestantes: List[Vuelo]): List[Itinerario] = vuelosRestantes.flatMap {
      case vuelo if vuelo.Org == cod1 && vuelo.Dst == cod2 && !visitados.contains(vuelo.Dst) =>
        List(List(vuelo))
      case vuelo if vuelo.Org == cod1 && !visitados.contains(vuelo.Dst) =>
        val nuevosVisitados = visitados + vuelo.Org
        generarItinerario(vuelo.Dst, cod2, nuevosVisitados, vuelos.filterNot(_ == vuelo)).map(vuelo :: _)
      case _ => List.empty
    }

    (cod1: String, cod2: String) => generarItinerario(cod1, cod2, Set.empty, vuelos)
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula los 3 (si los hay) 
    * itinerarios que minimizan el tiempo de viaje
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def buscarItinerarios(cod1: String, cod2: String): List[Itinerario] = {
      val posibles = itinerarios(vuelos,aeropuertos)(cod1,cod2)
    }
    buscarItinerarios
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula los 3 (si los hay) 
    * itinerarios que minimizan el número de cambios de avión
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula los 3 (si los hay) 
    * itinerarios que minimizan el tiempo de en el aire
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula
    * un itinearios que optimiza la hora de salida para llegar a tiempo a la cita
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2 y una hora de
    * cita en c2 h:m, y retorna un itinerario
    */
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Itinerario = {

  }
    
    
}
