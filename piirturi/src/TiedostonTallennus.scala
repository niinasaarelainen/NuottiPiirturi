
  import java.io.PrintWriter
  import scala.io.StdIn._
  

class TiedostonTallennus(biisi: Kappale) {
  
   val nimi = readLine("\nMillä nimellä talletetaan? Pelkkä ENTER ei tallenna mitään.")
   val kohdetiedosto = new PrintWriter("output/" + nimi+".txt")
   println("muista painaa F5, jotta tiedosto päivittyy Package Explorerissa.")
   
   try {
      for {
          viivasto <- biisi.kappale 
          rivi <- viivasto
      } kohdetiedosto.println(rivi)
   
            
   } finally {
     kohdetiedosto.close()
  }
   
}