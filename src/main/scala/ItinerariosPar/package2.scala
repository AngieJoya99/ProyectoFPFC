/*Angie Joya - 2322609
Johan David Pitto - 1932739
Miguel Ángel Salcedo - 2242786
José Daniel Trujillo - 2225611*/
import common._
import Itinerarios._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._

object ItinerariosPar{
   
  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela todos
    * los itinearios entre 2 aerpuertos que recibe como parámetro
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto cod1 y cod2, y retorna
    * todos los itinerarios entre los dos aeropuertos
    */
  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def generarItinerario(cod1: String, cod2: String, visitados: Set[String], vuelosRestantes: List[Vuelo]): List[Itinerario] = {
      vuelosRestantes.flatMap {
        case vuelo if vuelo.Org == cod1 && vuelo.Dst == cod2 && !visitados.contains(vuelo.Dst) =>
          List(List(vuelo))
        case vuelo if vuelo.Org == cod1 && !visitados.contains(vuelo.Dst) =>
          val nuevosVisitados = visitados + vuelo.Org
          val result = task {
            generarItinerario(vuelo.Dst, cod2, nuevosVisitados, vuelos.filterNot(_ == vuelo)).map(vuelo :: _)
          }
          result.join()
        case _ =>
          List.empty
      }
    }

    (cod1: String, cod2: String) => generarItinerario(cod1, cod2, Set.empty, vuelos)
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela los 3 (si los hay) 
    * itinerarios que minimizan el tiempo de viaje
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */  
  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val listaIt = itinerariosPar(vuelos,aeropuertos)
    def buscarItinerarios(cod1: String, cod2: String): List[Itinerario] = {
      def tiempoItinerario (it:Itinerario, arpt:List[Aeropuerto]): Int = {
        val vInicio = it.head
        val vFin = it.last
        val GMTSalidaLista = (for(a <- arpt if vInicio.Org == a.Cod) yield task(a))
        val GMTLlegadaLista = (for(a <- arpt if vFin.Dst == a.Cod) yield task(a))
        val GMTSalida = GMTSalidaLista.map(y => y.join()).head.GMT
        val GMTLlegada = GMTLlegadaLista.map(y => y.join()).head.GMT

        val hSalida  = (vInicio.HS - (GMTSalida/100)*60) + vInicio.MS
        val hLlegada = (vFin.HS - (GMTLlegada/100)*60) + vFin.MS
        
        if (hLlegada <= hSalida) (hLlegada+(60*24)-hSalida)
        else (hLlegada-hSalida)
      }
      
      val listaEntre = listaIt(cod1, cod2)
      if (listaEntre.length<=3)(itinerariosTiempo(vuelos,aeropuertos)(cod1, cod2))
      else{
        listaEntre.sortBy(it => (tiempoItinerario(it,aeropuertos))).take(3)
      }
    }
    buscarItinerarios
  }
  
  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela los 3 (si los hay) 
    * itinerarios que minimizan el número de cambios de avión
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val itinerariosEntre =  itinerariosPar(vuelos,aeropuertos)

    def encontrarMenorEsc (cod1 : String, cod2 : String): List[Itinerario] = {
      val itinerariosTotales = itinerariosEntre(cod1,cod2)
      
      if (itinerariosTotales.size <= 3) (itinerariosEscalas(vuelos,aeropuertos)(cod1, cod2)) 

      else {
        val itinerariosMenorEscOrd:List[Itinerario] = itinerariosTotales.sortBy(
        iti => (iti.map(_.Esc).sum + (iti.size - 1)))
        val itinerariosMenorEsc:List[Itinerario] = itinerariosMenorEscOrd.take(3)
        itinerariosMenorEsc
      }        
    }   
    encontrarMenorEsc
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela los 3 (si los hay) 
    * itinerarios que minimizan el tiempo de en el aire
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2, y retorna
    * una lista de itinerarios
    */

    def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
      val listaIt = itinerariosPar(vuelos,aeropuertos)

      def buscarItinerarios(cod1: String, cod2: String): List[Itinerario] = {

        def horaGMT (v:Vuelo, arpt:List[Aeropuerto]): (Int,Int) ={
          val GMTSalida = (for(a <- arpt if v.Org == a.Cod) yield a).head.GMT
          val GMTLlegada = (for(a <- arpt if v.Dst == a.Cod) yield a).head.GMT
          val hSalida  = ((v.HS - (GMTSalida/100))*60) + v.MS
          val hLlegada = ((v.HL - (GMTLlegada/100))*60) + v.ML

          if (hLlegada <= hSalida) (hSalida,hLlegada+(60*24)) else (hSalida,hLlegada)
        }

        def ordenarPorAire(itinerarios:List[Itinerario],arpt:List[Aeropuerto]):List[Itinerario] = {
          itinerarios.sortBy(iti => (for (v <- iti) yield((horaGMT(v,arpt)._2 - horaGMT(v,arpt)._1))).sum)
        }

        val listaEntre = listaIt(cod1, cod2)

        if (listaEntre.length<=3)(listaEntre)

        else{
          val sizePart = (listaEntre.size + 3)/4

          val (part1,part2,part3,part4) = parallel(
            task(ordenarPorAire(listaEntre.slice(0,sizePart),aeropuertos).take(3)).join,
            task(ordenarPorAire(listaEntre.slice(sizePart,2*sizePart),aeropuertos).take(3)).join,
            task(ordenarPorAire(listaEntre.slice(2*sizePart,3*sizePart),aeropuertos).take(3)).join,
            task(ordenarPorAire(listaEntre.slice(3*sizePart,listaEntre.size),aeropuertos).take(3)).join
          )
          
          val listaMenorAire = ordenarPorAire((part1++part2++part3++part4),aeropuertos)
          listaMenorAire          
        }
      }
      buscarItinerarios
  }

  /** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela
    * un itineario que optimiza la hora de salida para llegar a tiempo a la cita
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto c1 y c2 y una hora de
    * cita en c2 h:m, y retorna un itinerario
    */
  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String,Int,Int) => Itinerario = {
    val listaIt = itinerariosPar(vuelos,aeropuertos)
    def calcularItinerario(cod1:String, cod2:String, HC:Int ,MC:Int):Itinerario={
      val listaEntre = listaIt(cod1, cod2)
      val listaCita = listaEntre.filter(itinerario => (((itinerario.last.HL*60)+itinerario.last.ML)<=((HC*60)+MC)))
      if (listaCita.isEmpty)(List.empty[Vuelo])
      else ((listaCita.sortBy(itinerario => (itinerario.head.HS+itinerario.head.MS))).last)
    }
    calcularItinerario
  }
}
