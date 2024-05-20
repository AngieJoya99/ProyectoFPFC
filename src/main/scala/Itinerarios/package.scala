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
    val listaIt = itinerarios(vuelos,aeropuertos)
    def buscarItinerarios(cod1: String, cod2: String): List[Itinerario] = {
      def horaGMT (v:Vuelo, arpt:List[Aeropuerto]): (Int,Int) ={
        val GMTSalida = (for(a <- arpt if v.Org == a.Cod) yield a).head.GMT
        val GMTLlegada = (for(a <- arpt if v.Dst == a.Cod) yield a).head.GMT
        val hSalida  = ((v.HS - (GMTSalida/100))*60) + v.MS
        val hLlegada = ((v.HL - (GMTLlegada/100))*60) + v.ML
        if (hLlegada <= hSalida) (hSalida,hLlegada+(60*24)) else (hSalida,hLlegada)
      }

      def tiempoVuelo(it:Itinerario, arpt:List[Aeropuerto]): Int = {
        (for (v <- it) yield((horaGMT(v,arpt)._2 - horaGMT(v,arpt)._1))).sum        
      }

      def tiempoEspera(it:Itinerario, arpt:List[Aeropuerto]): Int = {
        val size = it.size
        if(size <= 1)(0) else{
          (for (i <- 1 until size) yield((horaGMT(it(i),arpt)._2 - horaGMT(it(i-1),arpt)._1))).sum
        }
      }

      val listaEntre = listaIt(cod1, cod2)
      if (listaEntre.length<=3)(listaEntre)
      else{
        listaEntre.sortBy(it => (tiempoVuelo(it,aeropuertos)+tiempoEspera(it,aeropuertos))).take(3)
      }
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
    val itinerariosEntre =  itinerarios(vuelos,aeropuertos)

    def encontrarMenorEsc (cod1 : String, cod2 : String): List[Itinerario] = {
      val itinerariosTotales = itinerariosEntre(cod1,cod2)//Halla todos los itinerarios entre el aerocpuerto cod1 y cod2
      
      if (itinerariosTotales.size <= 3) itinerariosTotales   

      else {
        val itinerariosMenorEscOrd:List[Itinerario] = itinerariosTotales.sortBy(
        iti => (iti.map(_.Esc).sum + (iti.size - 1)))/*Ordena la lista de itinerarios de acuerdo al total de escalas
        Que realiza cada itinerario*/

        val itinerariosMenorEsc:List[Itinerario] = itinerariosMenorEscOrd.take(3)
        itinerariosMenorEsc
      }        
    }   
    encontrarMenorEsc
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
    val itinerariosEntre = itinerarios(vuelos,aeropuertos)

    //Fucion que Ordena los itinerarios con menor distancia de vuelo
    def itinerariosMenorDistancia(cod1 : String , cod2 : String): List[Itinerario] = {
      def encontrarAeropuerto(codA:String, arpt:List[Aeropuerto]): Aeropuerto = {
        (for(a <- arpt if codA == a.Cod) yield a).head
      }

      //Funcion Auxiliar que calcula la distancia usando la formula Euclidiana de la distancia entre 2 puntos
      def calcularDist(x2:Int , x1:Int, y2:Int ,y1:Int):Double={
        math.sqrt(math.pow((x2 - x1),2) + math.pow((y2 - y1),2))
      }

      val itinerariosTotales = itinerariosEntre(cod1,cod2)//Halla los itinerarios entre el aeropuerto cod1 y cod2

      if (itinerariosTotales.size <= 3) itinerariosTotales
      else{
        //Retorna la lista de itinerarios ordenada por la distancia de vuelo
        val itineriosMenorDisOrd:List[Itinerario] = itinerariosTotales.sortBy(
          iti => iti.map(vuelo => 
            val origen = encontrarAeropuerto(vuelo.Org, aeropuertos)
            val destino = encontrarAeropuerto(vuelo.Dst, aeropuertos)
            calcularDist(destino.X, origen.X ,destino.Y, origen.Y)).sum
        )
        val itinerariosMenorDis:List[Itinerario] = itineriosMenorDisOrd.take(3)
        itinerariosMenorDis
      }
    }
    itinerariosMenorDistancia
  }        
  

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula
    * un itineario que optimiza la hora de salida para llegar a tiempo a la cita
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto cod1 y cod2 y una hora de
    * cita HC:MC, y retorna un itinerario
    */    
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String,Int,Int) => Itinerario = {
    val listaIt = itinerarios(vuelos,aeropuertos)
    def calcularItinerario(cod1:String, cod2:String, HC:Int ,MC:Int):Itinerario={
      val listaEntre = listaIt(cod1, cod2)
      val listaCita = listaEntre.filter(itinerario => (
        ((itinerario.last.HL*60)+itinerario.last.ML)<=((HC*60)+MC))
      )
      
      if (listaCita.isEmpty)(List.empty[Vuelo])
      else ((listaCita.sortBy(itinerario => (itinerario.head.HS+itinerario.head.MS))).last)
    }
    calcularItinerario
  }
}