  import scala.io.Source
  import java.io.PrintWriter
  import scala.collection.mutable.Buffer
  import scala.io.StdIn._
  

class TiedostonTallennus(biisi: Kappale) {
  
   val nimi = readLine("millä nimellä talletetaan? ")
   val kohdetiedosto = new PrintWriter("output/" + nimi+".txt")
   println("muista painaa F5, jotta tiedosto päivittyy Package Explorerissa.")
   
/*   
   try {
      for { 
        viivasto<- biisi.kappale      
        rivi<-viivasto.viivasto            // ei ole enää viivasto-olioita !!!
      }  kohdetiedosto.println(rivi)
   } finally {
     kohdetiedosto.close()
  }
  */     
}