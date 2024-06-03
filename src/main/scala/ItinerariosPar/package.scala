/*Angie Joya - 2322609
Miguel Ángel Salcedo - 2242786
José Daniel Trujillo - 2225611*/

import common._
import Itinerarios._
import Datos._
import scala.collection.parallel.immutable._
import scala.collection.parallel.CollectionConverters._

package object ItinerariosPar{

/** Dada una lista de todos los vuelos disponibles y una lista 
    * de todos los aeropuertos, crea una función que calcula de manera paralela todos
    * los itinearios entre 2 aerpuertos que recibe como parámetro
    * @param vuelos Lista de todos los vuelos disponibles
    * @param aeropuertos Lista de todos los aeropuertos
    * @return Función que recibe dos códigos de aeropuerto cod1 y cod2, y retorna
    * todos los itinerarios entre los dos aeropuertos
    */

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen: Map[String, List[Vuelo]] = vuelos.groupBy(_.Org)

    def generarItinerario(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
      val resultadosParalelos = vuelosPorOrigen.getOrElse(cod1, List.empty).flatMap {
        case vuelo if vuelo.Org == cod1 && vuelo.Dst == cod2 && !visitados.contains(vuelo.Dst) =>
          List(task { List(List(vuelo))})
        case vuelo if vuelo.Org == cod1 && !visitados.contains(vuelo.Dst) =>
          val nuevosVisitados = visitados + vuelo.Org
          List(task {
            generarItinerario(vuelo.Dst, cod2, nuevosVisitados).map(vuelo :: _)
          })
        case _ => List.empty
      }

      resultadosParalelos.flatMap(_.join())
    }

    (cod1: String, cod2: String) => generarItinerario(cod1, cod2, Set.empty)
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

      def ordenarPorTiempo(itinerarios:List[Itinerario],arpts:List[Aeropuerto]):List[Itinerario] = {
        itinerarios.sortBy(iti => tiempoItinerario(iti,arpts))
      }
      
      val listaEntre = listaIt(cod1, cod2)
      if (listaEntre.length<=3)(itinerariosTiempo(vuelos,aeropuertos)(cod1, cod2))
      else{
        val sizePart = (listaEntre.size + 3)/4

        val (part1,part2,part3,part4) = parallel(
            ordenarPorTiempo(listaEntre.slice(0,sizePart),aeropuertos).take(3),
            ordenarPorTiempo(listaEntre.slice(sizePart,2*sizePart),aeropuertos).take(3),
            ordenarPorTiempo(listaEntre.slice(2*sizePart,3*sizePart),aeropuertos).take(3),
            ordenarPorTiempo(listaEntre.slice(3*sizePart,listaEntre.size),aeropuertos).take(3)
        )

        val listaMenorTiempo = (part1++part2++part3++part4).sortBy(iti => tiempoItinerario(iti,aeropuertos)).take(3)
        listaMenorTiempo
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
      val itinerariosTotales = itinerariosEntre(cod1,cod2)//Halla todos los itinerarios entre el aerocpuerto cod1 y cod2
      
      if (itinerariosTotales.size <= 3) itinerariosTotales 

      else {
        val tamañoParticion = (itinerariosTotales.size + 3)/4  //sumamos 3 para que se redonde hacia arriba

        val particion1 = itinerariosTotales.slice(0,tamañoParticion)
        val particion2 = itinerariosTotales.slice(tamañoParticion,2*tamañoParticion)
        val particion3 = itinerariosTotales.slice(2*tamañoParticion,3*tamañoParticion)
        val particion4 = itinerariosTotales.slice(3*tamañoParticion,itinerariosTotales.size)

        def ordenarPorEscala(itinearios:List[Itinerario]): List[Itinerario] = {
          itinearios.sortBy(iti => ( iti.map(_.Esc)).sum + (iti.size -1))
        }

        val (part1Ord:List[Itinerario], part2Ord:List[Itinerario], part3Ord:List[Itinerario], part4Ord:List[Itinerario]) = parallel(
          ordenarPorEscala(particion1).take(3),
          ordenarPorEscala(particion2).take(3),
          ordenarPorEscala(particion3).take(3),
          ordenarPorEscala(particion4).take(3)
        )
        val listaMenorEsc = ordenarPorEscala(part1Ord ++ part2Ord ++ part3Ord ++ part4Ord).take(3)
        listaMenorEsc
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
        val GMTSalidaLista = (for(a <- arpt if v.Org == a.Cod) yield task(a))
        val GMTLlegadaLista = (for(a <- arpt if v.Dst == a.Cod) yield task(a))
        val GMTSalida = GMTSalidaLista.map(y => y.join()).head.GMT
        val GMTLlegada = GMTLlegadaLista.map(y => y.join()).head.GMT
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
          ordenarPorAire(listaEntre.slice(0,sizePart),aeropuertos).take(3),
          ordenarPorAire(listaEntre.slice(sizePart,2*sizePart),aeropuertos).take(3),
          ordenarPorAire(listaEntre.slice(2*sizePart,3*sizePart),aeropuertos).take(3),
          ordenarPorAire(listaEntre.slice(3*sizePart,listaEntre.size),aeropuertos).take(3)
        )
        
        val listaMenorAire = ordenarPorAire((part1++part2++part3++part4),aeropuertos)
        listaMenorAire.take(3)
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
      val listaEntre = listaIt(cod1, cod2).par
      val listaCita = listaEntre.filter(itinerario => (
        ((itinerario.last.HL*60)+itinerario.last.ML)<=((HC*60)+MC))
      )
      
      def llegaACita(lista: List[Itinerario]): Itinerario ={
        val ordenada = lista.sortBy(itinerario => (itinerario.head.HS+itinerario.head.MS))
        if (ordenada.size > 0) (ordenada.last)
        else (List.empty[Vuelo])
      }

      val listaCitaSeq: List[Itinerario] = listaCita.toSeq.toList
      if (listaCitaSeq.isEmpty)(List.empty[Vuelo])
      else {
        val sizeLista = listaCitaSeq.size
        sizeLista match {
          case 0 => List.empty[Vuelo]
          case 1 => listaCitaSeq.head
          case 2 => {
            val (parte1,parte2) = parallel(
              llegaACita(listaCitaSeq.slice(0, 1)),
              llegaACita(listaCitaSeq.slice(1, 2))
            )
            val secuenciales = List(parte1,parte2)
            llegaACita(secuenciales)
          }
          case 3 => {
            val paralelas = List(
              task(llegaACita(listaCitaSeq.slice(0, 1))),
              task(llegaACita(listaCitaSeq.slice(1, 2))),
              task(llegaACita(listaCitaSeq.slice(2, 3)))
            )
            val secuenciales = paralelas.map(x => x.join())
            llegaACita(secuenciales)
          }
          case _ => {
            val tamaño = math.ceil(sizeLista.toDouble / 2).toInt
            val (parte1,parte2,parte3,parte4) = parallel(
              llegaACita(listaCitaSeq.slice(0, tamaño)),
              llegaACita(listaCitaSeq.slice(tamaño, tamaño*2)),
              llegaACita(listaCitaSeq.slice(tamaño*2, tamaño*3)),
              llegaACita(listaCitaSeq.slice(tamaño*3, listaCita.size))
            )
            val secuenciales = List(parte1,parte2,parte3,parte4)
            llegaACita(secuenciales)
          }
        }        
      }
    }
    calcularItinerario
  }
}
