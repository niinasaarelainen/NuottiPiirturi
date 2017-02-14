package traitNuotti

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map


trait Nuotti  {
  
  
  def piirraApuviiva(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int) = {
     if(nuotinNimi == "c1"){
       viivasto(nuotit(nuotinNimi))(x-1) = '-'   
       viivasto(nuotit(nuotinNimi))(x+2) = '-'
    }
  }
  
  def piirraVarsi(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int, lisaPituus: Int= 0) = {
    if(nuotit(nuotinNimi) < nuotit("h1")){       // varsi alas
       viivasto(nuotit(nuotinNimi)+1)(x-1) = '|'
       viivasto(nuotit(nuotinNimi)+2)(x-1) = '|'
       viivasto(nuotit(nuotinNimi)+3)(x-1) = '|'
       for(i<-1 to  lisaPituus)
           viivasto(nuotit(nuotinNimi)+3+i)(x-1) = '|'
    } else{
       viivasto(nuotit(nuotinNimi)-1)(x+1) = '|'  // varsi ylös
       viivasto(nuotit(nuotinNimi)-2)(x+1) = '|'
       viivasto(nuotit(nuotinNimi)-3)(x+1) = '|'
       for(i<-1 to  lisaPituus)
           viivasto(nuotit(nuotinNimi)-3-i)(x+1) = '|'
    }
  }
  
}

 class KokoNuotti(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int) extends Nuotti{
    viivasto(nuotit(nuotinNimi))(x) = '('   
    viivasto(nuotit(nuotinNimi))(x+1) = ')'   
    
    piirraApuviiva(nuotit, nuotinNimi, viivasto, x)
 }
 
  class PuoliNuotti(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int) extends KokoNuotti(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int){
     piirraVarsi( nuotit, nuotinNimi, viivasto, x)
  }
  
  class NeljasosaNuotti(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int) extends Nuotti{
  
       viivasto(nuotit(nuotinNimi))(x) = '@'  
       if(nuotinNimi == "c1"){
          viivasto(nuotit(nuotinNimi))(x-1) = '-'   
          viivasto(nuotit(nuotinNimi))(x+1) = '-'     // tämä tulee eri paikkaan kuin puolinuotissa, siksi ei voi kutsua traitin metodia
       }
       piirraVarsi(nuotit, nuotinNimi, viivasto, x)    
  }
  
   class KahdeksasosaNuotti (nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int) extends NeljasosaNuotti(nuotit: Map[String, Int], nuotinNimi: String, viivasto: Buffer[Array[Char]], x: Int){
       if(nuotit(nuotinNimi) < nuotit("h1"))      
          viivasto(nuotit(nuotinNimi)+3)(x) = '/'    // väkä 
       else   
          viivasto(nuotit(nuotinNimi)-3)(x+2) = '\\'
          
       piirraVarsi(nuotit, nuotinNimi, viivasto, x)
  }
  
 
