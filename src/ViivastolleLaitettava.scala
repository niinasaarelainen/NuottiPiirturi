import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

 trait ViivastolleLaitettava {  
  
   var viivasto = Buffer[String]()   
   val y = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)

   def kuva: Buffer[String]   
   def kuvanLeveys: Int   // tätä tarvitaan myöhemmin, kun laitetaan lyriikat paikoilleen
   def pituus : Double
   def soiva: Boolean    // tarvitaan tieto sanoja laittaessa, tauko= false
  
  
   def piirraTyhjaViivasto(pituus: Int) = {
    
      var viiva = ""
      for ( i <- 1 to pituus) viiva += "-"  // muodostetaan oikean mittainen viiva
      var vali = ""
      for ( i <- 1 to pituus) vali += " "   // muodostetaan oikean mittainen väli
    
      for ( i <- 1 to 3) viivasto += vali      // ylös tyhjää varsia varten   
      for(i <- 1 to 5){
        viivasto += vali      // ylin paikka on g2 = Map index 3
        viivasto += viiva      // 5 viivaa
      } 
      for ( i <- 1 to 4) viivasto += vali       // d1, c1, alavali & sanoille tila
        
    viivasto      
  }
}
  
 class Sointu(aanet: Buffer[ViivastolleLaitettava]) extends ViivastolleLaitettava{
     def nuotit = aanet   // korkeuden joutuu laskemaan jokaiselle nuotille erikseen
     def soiva =  true
     def pituus = aanet(0).pituus   // kaikkien soinnun sävelten tulee olla samanpituisia 
     def kuvanLeveys =  aanet(0).kuvanLeveys
     
   
     println("aanet.size" + aanet.size)
     println("aanet(0).kuva.size" + aanet(0).kuva.size)
     println("aanet(0).kuva(0).size" + aanet(0).kuva(0).size)
     
     
     def kuva = {                // Viivasto.liita kutsuu tätä sikamonta kertaa
//       for (aani <- aanet){
//          println(aani.asInstanceOf[Nuotti].korkeus)
//          println(aanet(0).asInstanceOf[Nuotti].nuppi)
//          println(aani.asInstanceOf[Nuotti].nimiMapissa)
//       }   
        
      var viivasto = piirraTyhjaViivasto(kuvanLeveys)
      for (aani <- aanet){
         val nimiMapissa = aani.asInstanceOf[Nuotti].nimiMapissa 
         val etumerkki = aani.asInstanceOf[Nuotti].etumerkki
         val nuppi = aani.asInstanceOf[Nuotti].nuppi
         if(etumerkki.size == 0)  // ei etumerkkiä
            viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 3) + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
         else
            viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 2) + etumerkki + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
         }
      
      /*
      var viivastoArray = Array[Char]()
      println("viivasto.size" + viivasto.size)
       for(a <- 0 until aanet.size){             // äänet
         for(r <- 0 until 17){   // rivit              // (aanet(a).kuva.size-2)
           for(i <- 0 until aanet(a).kuva(r).size){  // rivin merkit
              if (aanet(a).kuva(r).toCharArray().charAt(i) != '-' && aanet(a).kuva(r).toCharArray().charAt(i) != ' '){
                viivastoArray = viivasto(r).toCharArray()             // ("a: " + a + "r: " + r + "i " + i)
                viivastoArray(i) =  aanet(a).kuva(r).toCharArray().charAt(i)  
              }  
           }     
         } 
       }  */
     viivasto         //  aanet(0).kuva      
     }
    
  }
 

abstract class Nuotti extends ViivastolleLaitettava {
     def nuppi = "()"   
     def soiva = true
     def korkeus: String
     def nimiMapissa: String
     def etumerkki: String
     
     
     def piirraApuviiva = {            // TODO
      // if(nimiMapissa == "c1"){                 
       
     //  }
  }
  }
 
 abstract class Tauko extends ViivastolleLaitettava{
    def korkeus = "c2"    
    def soiva = false
  }

 
 class KokoNuotti(nuotinNimi: String) extends Nuotti{    
    def korkeus = nuotinNimi
    def pituus = 4.0
    def kuvanLeveys = 20
    def nimiMapissa = nuotinNimi.filter(_ !='#').filter(_ != 'b').filter(_ != '§')  // esim. gb1 --> g1
    def etumerkki: String = if (nuotinNimi.filter(_ !='-').size == 3) nuotinNimi(1).toString else ""
  
    viivasto = piirraTyhjaViivasto(kuvanLeveys)  
    def kuva = {
      
      if(etumerkki.size == 0)  // ei etumerkkiä
         viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 3) + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
      else
         viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 2) + etumerkki + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
      viivasto
    }
 }   
 

  class PuoliNuotti(nuotinNimi: String) extends KokoNuotti(nuotinNimi: String){
    override def korkeus = nuotinNimi
    override def pituus = 2.0
    override def kuvanLeveys = 12
    
    override def kuva = {
      super.kuva
      piirraVarsi
      viivasto
    }
    
    def piirraVarsi = {
     if(y(nimiMapissa) >= y("h1")){  // varsi ylös  
      for (i <- 1 to 3)
         viivasto(y(nimiMapissa)-i) = viivasto(y(nimiMapissa)-i).substring(0, 4) + "|" + viivasto(y(nimiMapissa)-i).substring(5, kuvanLeveys)  
     }  
     else {  // varsi alas 
       for (i <- 1 to 3)
       viivasto(y(nimiMapissa)+i) = viivasto(y(nimiMapissa)+i).substring(0, 3) + "|" + viivasto(y(nimiMapissa)+i).substring(4, kuvanLeveys)  
     }
   }  
 }
  

   class PisteellinenPuoliNuotti(nuotinNimi: String) extends PuoliNuotti(nuotinNimi: String){
      override def pituus = 3.0
      override def kuvanLeveys = 16
      
      override def kuva = {
        super.kuva
        viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 5) + "." + viivasto(y(nimiMapissa)).substring(6, kuvanLeveys)  
        viivasto
      }
      
   }
   
    
  class NeljasosaNuotti(nuotinNimi: String) extends PuoliNuotti(nuotinNimi: String){
    override def korkeus = nuotinNimi
    override def pituus = 1.0
    override def kuvanLeveys = 8
    override def nuppi = "@@"
    
     override def kuva = {
      super.kuva
      viivasto
    }
  }   
  
    
   class KahdeksasosaNuotti(nuotinNimi: String) extends NeljasosaNuotti(nuotinNimi: String){
    override def korkeus = nuotinNimi
    override def pituus = 0.5
    override def kuvanLeveys = 6
    
  override def kuva = {
      super.kuva
      piirraVarsi
      viivasto
    }
    
    override def piirraVarsi = {
     if(y(nimiMapissa) >= y("h1")){  // varsi ylös  
        for (i <- 1 to 3)
           viivasto(y(nimiMapissa)-i) = viivasto(y(nimiMapissa)-i).substring(0, 4) + "|" + viivasto(y(nimiMapissa)-i).substring(5, kuvanLeveys)  
        viivasto(y(nimiMapissa)-3) = viivasto(y(nimiMapissa)-3).substring(0, 5) + "\\" + viivasto(y(nimiMapissa)-3).substring(6, kuvanLeveys)  
     } else {  // varsi alas 
       for (i <- 1 to 3)
           viivasto(y(nimiMapissa)+i) = viivasto(y(nimiMapissa)+i).substring(0, 3) + "|" + viivasto(y(nimiMapissa)+i).substring(4, kuvanLeveys)  
       viivasto(y(nimiMapissa)+3) = viivasto(y(nimiMapissa)+3).substring(0, 4) + "/" + viivasto(y(nimiMapissa)+3).substring(5, kuvanLeveys)  
       }
     }  
  }   
  
   /*
  def piirraKahdeksasosaPari(nuotinNimi: String, toisenNuotinNimi: String) = {
    val toisenNuotinNimiTutk = tutkiEtumerkit(toisenNuotinNimi, x+5) 
    viivasto(nuotitYAkselilla(nuotinNimi))(x)='@'          // 1/8-nuottipari, varret ylös
    viivasto(nuotitYAkselilla(toisenNuotinNimiTutk))(x+5)='@'     
    if(nuotinNimi == "c1"){
       viivasto(nuotitYAkselilla(nuotinNimi))(x-1) = '-'    // keski-c:lle apuviiva
       viivasto(nuotitYAkselilla(nuotinNimi))(x+1) = '-'
    }
    if(toisenNuotinNimi == "c1"){
       viivasto(nuotitYAkselilla(toisenNuotinNimi))(x+4) = '-'    // keski-c:lle apuviiva
       viivasto(nuotitYAkselilla(toisenNuotinNimi))(x+6) = '-'
    }
    val korkeusero = nuotitYAkselilla(nuotinNimi) - nuotitYAkselilla(toisenNuotinNimiTutk)
    println(korkeusero)
    if(korkeusero >= 0 && nuotitYAkselilla(nuotinNimi) > nuotitYAkselilla("h1") || korkeusero < 0 && nuotitYAkselilla(nuotinNimi) < nuotitYAkselilla("h1")){   // parin eka nuotti on alempana, toinen korkeammalla
        piirraVarsi(nuotinNimi, x, korkeusero)
        piirraVarsi(toisenNuotinNimiTutk, x+5, 0)       
        piirraPalkki(nuotitYAkselilla(nuotinNimi) < nuotitYAkselilla("h1"), nuotitYAkselilla(toisenNuotinNimiTutk))  // palkki 2. nuotin korkeudelle
     
    }     
    else  {          // parin eka nuotti on korkeampi, toinen matalampi
       piirraVarsi(nuotinNimi, x, 0)
       piirraVarsi(toisenNuotinNimiTutk, x+5, Math.abs(korkeusero))
       piirraPalkki(nuotitYAkselilla(nuotinNimi) < nuotitYAkselilla("h1"), nuotitYAkselilla(nuotinNimi))    // palkki 1. nuotin korkeudelle
    }   
   }
  
  
  
  
  class NeljasosaTauko extends Tauko { 
      ='\\'
     = '/'
     ='\\'
      = '/'    
       
  }
  
   class KahdeksasosaTauko extends NeljasosaTauko{
      
          = '/'             // väkä        
          = '\\'
       
  }
  
 
  
  
 */
