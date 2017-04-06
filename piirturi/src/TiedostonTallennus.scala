import java.io.PrintWriter
import scala.io.StdIn._
  

class TiedostonTallennus(biisi: Kappale, kohdetiedosto: PrintWriter) {
  
   
   try {
      for {
          viivasto <- biisi.kappale 
          rivi <- viivasto
      } kohdetiedosto.println(rivi)
   
            
   } finally {
     kohdetiedosto.close()
  }
   
}