/*Angie Joya - 2322609
Miguel Ángel Salcedo - 2242786
José Daniel Trujillo - 2225611*/

import Itinerarios._
import ItinerariosPar._
import scala.collection.parallel.CollectionConverters._
import org.scalameter._

package object Benchmark {

  def compararItineratios(funcion: Int, vuelos: List[Vuelo], aeropuertos: List[Aeropuerto], cod1: String, cod2: String): (Double,Double, Double) ={
    funcion match{
      case 1 => {
        val timeA1 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerarios(vuelos,aeropuertos)(cod1,cod2))

        val timeA2 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosPar(vuelos,aeropuertos)(cod1,cod2))

        val speedUp= timeA1.value/timeA2.value
        (timeA1.value, timeA2.value, speedUp)
      }

      case 2 => {
        val timeA1 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosTiempo(vuelos,aeropuertos)(cod1,cod2))

        val timeA2 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosTiempoPar(vuelos,aeropuertos)(cod1,cod2))

        val speedUp= timeA1.value/timeA2.value
        (timeA1.value, timeA2.value, speedUp)
      }
      case 3 => {
        val timeA1 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosEscalas(vuelos,aeropuertos)(cod1,cod2))

        val timeA2 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosEscalasPar(vuelos,aeropuertos)(cod1,cod2))

        val speedUp= timeA1.value/timeA2.value
        (timeA1.value, timeA2.value, speedUp)
      }
      case 4 => {
        val timeA1 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosAire(vuelos,aeropuertos)(cod1,cod2))

        val timeA2 = config(
          KeyValue(Key.exec.minWarmupRuns -> 10),
          KeyValue(Key.exec.maxWarmupRuns -> 20),
          KeyValue(Key.verbose -> false)
        ) withWarmer(new Warmer.Default) measure (itinerariosAirePar(vuelos,aeropuertos)(cod1,cod2))

        val speedUp= timeA1.value/timeA2.value
        (timeA1.value, timeA2.value, speedUp)
      }
    }    
  }


  def compararItineratiosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto], cod1: String, cod2: String, HC:Int ,MC:Int): (Double,Double, Double) ={
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 10),
      KeyValue(Key.exec.maxWarmupRuns -> 20),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (itinerariosSalida(vuelos,aeropuertos)(cod1,cod2,HC,MC))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 10),
      KeyValue(Key.exec.maxWarmupRuns -> 20),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (itinerariosSalidaPar(vuelos,aeropuertos)(cod1,cod2,HC,MC))

    val speedUp= timeA1.value/timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
  
}
