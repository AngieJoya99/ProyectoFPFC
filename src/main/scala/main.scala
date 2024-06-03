import common._
import Itinerarios._
import ItinerariosPar._
import Datos._
import Benchmark._

object Main {
  def main(args: Array[String]): Unit = {
    
    val itSalidaCurso = itinerariosSalidaPar(vuelosCurso,aeropuertosCurso)
    val itsal1 = itSalidaCurso("CTG","PTY",11, 40)
    val itsal2 = itSalidaCurso("CTG","PTY",11, 55)
    val itsal3 = itSalidaCurso("CTG","PTY",10,30)
    println("Par\nitsal1 = "+itsal1+"\n"+"itsal2 = "+itsal2+"\n"+"itsal3 = "+itsal3)

    println("Longitud 200: "+compararItineratiosSalida((vuelosC1++vuelosC2), aeropuertos, "ATL", "LAX", 10, 50))
    println("Longitud 200: "+compararItineratiosSalida((vuelosC2++vuelosC3), aeropuertos, "BOS", "MIA", 8, 45))
    println("Longitud 200: "+compararItineratiosSalida((vuelosC3++vuelosC4), aeropuertos, "ATL", "DFW", 7, 20))
    println("Longitud 200: "+compararItineratiosSalida((vuelosC4++vuelosC5), aeropuertos, "BOS", "LAX", 6, 0))
    println("Longitud 200: "+compararItineratiosSalida((vuelosC1++vuelosC5), aeropuertos, "ATL", "LAX", 12, 20))

    /*
    //Pruebas Secuenciales
    val itsCurso = itinerariosPar(vuelosCurso,aeropuertosCurso)
    val its1 = itsCurso("MID", "SVCS")
    val its2 = itsCurso("CLO", "SVCS")
    val its3 = itsCurso("CLO","SVO")
    val its4 = itsCurso("CLO", "MEX")
    val its5 = itsCurso("CTG","PTY")
    println("its1 = "+its1+"\n"+"its2 = "+its2+"\n"+"its3 = "+its3+"\n"+"its4 = "+its4+"\n"+"its5 = "+its5+"\n")

    val itsTiempoCurso = itinerariosTiempoPar(vuelosCurso,aeropuertosCurso)
    val itst1 = itsTiempoCurso("MID", "SVCS")
    val itst2 = itsTiempoCurso("CLO", "SVCS")
    val itst3 = itsTiempoCurso("CLO","SVO")
    val itst4 = itsTiempoCurso("CLO", "MEX")
    val itst5 = itsTiempoCurso("CTG", "PTY")
    println("\nitst1 = "+itst1+"\n"+"itst2 = "+itst2+"\n"+"itst3 = "+itst3+"\n"+"itst4 = "+itst4+"\n"+"itst5 = "+itst5+"\n")

    val itsEscalasCurso = itinerariosEscalasPar(vuelosCurso,aeropuertosCurso)
    val itsc1 = itsEscalasCurso("MID", "SVCS")
    val itsc2 = itsEscalasCurso("CLO", "SVCS")
    val itsc3 = itsEscalasCurso("CLO","SVO")
    val itsc4 = itsEscalasCurso("CLO", "MEX")
    val itsc5 = itsEscalasCurso("CTG","PTY")
    println("\nitsc1 = "+itsc1+"\n"+"itsc2 = "+itsc2+"\n"+"itsc3 = "+itsc3+"\n"+"itsc4 = "+itsc4+"\n"+"itsc5 = "+itsc5+"\n")

    val itsAireCurso = itinerariosAirePar(vuelosCurso,aeropuertosCurso)
    val itsa1 = itsAireCurso("MID", "SVCS")
    val itsa2 = itsAireCurso("CLO", "SVCS")
    val itsa3 = itsAireCurso("CLO","SVO")
    val itsa4 = itsAireCurso("CLO", "MEX")
    val itsa5 = itsAireCurso("CTG","PTY")
    println("\nitsa1 = "+itsa1+"\n"+"itsa2 = "+itsa2+"\n"+"itsa3 = "+itsa3+"\n"+"itsa4 = "+itsa4+"\n"+"itsa5 = "+itsa5+"\n")

    val itSalidaCurso = itinerariosSalida(vuelosCurso,aeropuertosCurso)
    val itsal1 = itSalidaCurso("CTG","PTY",11, 40)
    val itsal2 = itSalidaCurso("CTG","PTY",11, 55)
    val itsal3 = itSalidaCurso("CTG","PTY",10,30)
    println("\nitsal1 = "+itsal1+"\n"+"itsal2 = "+itsal2+"\n"+"itsal3 = "+itsal3)
    
    
    println(compararItineratios(2,vuelosCurso, aeropuertosCurso, "MID", "SVCS"))
    println(compararItineratios(2,vuelosCurso, aeropuertosCurso, "CLO", "SVCS"))
    println(compararItineratios(2,vuelosCurso, aeropuertosCurso, "CLO","SVO"))
    println(compararItineratios(2,vuelosCurso, aeropuertosCurso, "CLO", "MEX"))
    println(compararItineratios(2,vuelosCurso, aeropuertosCurso, "CTG","PTY"))
    
    
    println("Itinerarios Tiempo\nLongitud 15: "+compararItineratios(2,vuelosA1, aeropuertos, "HOU", "MSY"))
    println("Longitud 15: "+compararItineratios(1,vuelosA2, aeropuertos, "SFO", "ORD"))
    println("Longitud 15: "+compararItineratios(1,vuelosA3, aeropuertos, "DFW", "ORD"))
    println("Longitud 15: "+compararItineratios(1,vuelosA4, aeropuertos, "ORD", "LAX"))
    println("Longitud 15: "+compararItineratios(1,vuelosA5, aeropuertos, "DFW", "HOU"))
    
    println("Longitud 40: "+compararItineratios(1,vuelosB1, aeropuertos, "BNA", "ORD"))
    println("Longitud 40: "+compararItineratios(1,vuelosB2, aeropuertos, "DFW", "ORD"))
    println("Longitud 40: "+compararItineratios(1,vuelosB3, aeropuertos, "ORD", "DFW"))
    println("Longitud 40: "+compararItineratios(1,vuelosB4, aeropuertos, "LAX", "SEA"))
    println("Longitud 40: "+compararItineratios(1,vuelosB5, aeropuertos, "DEN", "SEA"))
    
    
    println("Longitud 100: "+compararItineratios(1,vuelosC1, aeropuertos, "ATL", "LAX"))
    println("Longitud 100: "+compararItineratios(1,vuelosC2, aeropuertos, "BOS", "ATL"))
    println("Longitud 100: "+compararItineratios(1,vuelosC3, aeropuertos, "ATL", "MIA"))
    println("Longitud 100: "+compararItineratios(1,vuelosC4, aeropuertos, "BOS", "DFW"))
    println("Longitud 100: "+compararItineratios(1,vuelosC5, aeropuertos, "ATL", "LAX"))
    

    println("Longitud 200: "+compararItineratios(1,(vuelosC1++vuelosC2), aeropuertos, "ATL", "ATL"))
    println("Longitud 200: "+compararItineratios(1,(vuelosC2++vuelosC3), aeropuertos, "BOS", "MIA"))
    println("Longitud 200: "+compararItineratios(1,(vuelosC3++vuelosC4), aeropuertos, "ATL", "DFW"))
    println("Longitud 200: "+compararItineratios(1,(vuelosC4++vuelosC5), aeropuertos, "BOS", "LAX"))
    println("Longitud 200: "+compararItineratios(1,(vuelosC1++vuelosC5), aeropuertos, "ATL", "LAX"))
    
    
    //println("Longitud 200: "+compararItineratios(4,(vuelosC1++vuelosC2), aeropuertos,"ATL","LAX"))
    /*println("Longitud 200: "+compararItineratios(1,vuelosD1, aeropuertos, "PHX", "DTW"))
    println("Longitud 200: "+compararItineratios(1,vuelosD2, aeropuertos, "STL", "MIA"))
    println("Longitud 200: "+compararItineratios(1,vuelosD3, aeropuertos, "ORD", "DEN"))*/
    */


  }

}