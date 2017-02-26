import scala.collection.mutable.Buffer
import scala.collection.mutable.Map


trait ViivastolleLaitettava {  
  
   var viivasto = Buffer[String]()   
   val y = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)

   def kuva: Buffer[String]   
   def kuvanLeveys: Int   
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
     var korkeudet = Buffer[Int]()
     
    def kuva = {              
        
      viivasto = piirraTyhjaViivasto(kuvanLeveys)
      for (aani <- aanet){
         val nimiMapissa = aani.asInstanceOf[Nuotti].nimiMapissa 
         val etumerkki = aani.asInstanceOf[Nuotti].etumerkki
         val extraetumerkki = aani.asInstanceOf[Nuotti].extraetumerkkiDef
         val nuppi = aani.asInstanceOf[Nuotti].nuppi
         korkeudet += y(nimiMapissa)
         if(nimiMapissa == "c1")  viivasto(y("c1")) = viivasto(y("c1")).substring(0, 1) + "--" +  viivasto(y("c1")).substring(4, 6) + "--" + viivasto(y("c1")).substring(7, kuvanLeveys)         
         
         if(etumerkki.size == 0 && extraetumerkki.size == 0)  // ei etumerkkiä
            viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 3) + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
         else
            viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 2) + extraetumerkki + etumerkki + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
         if(aani.pituus== 1.5 || aani.pituus == 3)
            viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 5) + "." + viivasto(y(nimiMapissa)).substring(6, kuvanLeveys)  

      }  // end for
     
      var ylospain = true
      if (korkeudet.min - 0 < 15 - korkeudet.max )   // 0 on ylin piirtoindeksi, 15 alin, lasketaan missä on enemmän tilaa
          ylospain = false
      if (pituus < 4)   // kokonuottiin ei vartta 
          piirraVarsiJaMahdollisestiVaka(korkeudet.min, korkeudet.max, ylospain)
      viivasto          
    }
     
     def piirraVarsiJaMahdollisestiVaka(mista: Int, mihin:Int ,ylospain:Boolean) = {                                  ///////// @ Sointu
            if(ylospain){
               for (i <- 1 to mihin-mista+3)   // nuottien väli + kolmen mittainen ylimenevä osuus
                 if(!korkeudet.contains(mihin-i))  // nuppien kohdalle ei vartta
                   viivasto(mihin-i) = viivasto(mihin-i).substring(0, 4) + "|" + viivasto(mihin-i).substring(5, kuvanLeveys)  
            if (pituus == 0.5) viivasto(mista-3) =  viivasto(mista-3).substring(0, 5) + "\\" + viivasto(mista-3).substring(6, kuvanLeveys)       
            } else {
              for (i <-  1 to mihin-mista +3)
                  if(!korkeudet.contains(mista+i))
                     viivasto(mista+i) = viivasto(mista+i).substring(0, 3) + "|" + viivasto(mista+i).substring(4, kuvanLeveys)  
              if (pituus == 0.5) viivasto(mihin + 3) =  viivasto(mihin + 3).substring(0, 4) + "/" + viivasto(mihin + 3).substring(5, kuvanLeveys)                
            }        
     }     
}
 

abstract class Nuotti extends ViivastolleLaitettava {
     def nuppi = "()"   
     def soiva = true
     def korkeus: String
     def nimiMapissa: String
     def etumerkki: String
     def extraetumerkkiDef: String
       
     def piirraApuviiva = {                       
           viivasto(y("c1")) = viivasto(y("c1")).substring(0, 1) + "--" +  viivasto(y("c1")).substring(4, 6) + "--" + viivasto(y("c1")).substring(7, kuvanLeveys)         
     }
  }
 
abstract class Tauko extends ViivastolleLaitettava{
     def korkeus = "c2"    
     def soiva = false
}

 
class KokoNuotti(nuotinNimi: String, extraetumerkki: String = "") extends Nuotti{    
     def korkeus = nuotinNimi
     def pituus = 4.0
     def kuvanLeveys = 20
     def nimiMapissa = nuotinNimi.filter(_ !='#').filter(_ != 'b') // esim. gb1 --> g1
     def etumerkki = if(extraetumerkki == "n") "" else if (nuotinNimi.filter(_ !='-').size == 3) nuotinNimi(1).toString else ""
     def extraetumerkkiDef = if(extraetumerkki == "n") "" else extraetumerkki
    
  
     def piirraNuppi = { 
        if(nimiMapissa=="c1") piirraApuviiva
        if(etumerkki.size == 0 && extraetumerkkiDef.size == 0)  // ei etumerkkiä
           viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 3) + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
        else
         viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 2) + etumerkki + extraetumerkkiDef + nuppi + viivasto(y(nimiMapissa)).substring(5, kuvanLeveys)  
                                                                            // ikinä ei ole molempia, toinen "" 
      }
     
      def kuva = {
         viivasto = piirraTyhjaViivasto(kuvanLeveys) 
         piirraNuppi
         viivasto
      }
}   
 

class PuoliNuotti(nuotinNimi: String, extraetumerkki: String = "") extends KokoNuotti(nuotinNimi: String, extraetumerkki: String){
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
  

class PisteellinenPuoliNuotti(nuotinNimi: String, extraetumerkki: String = "") extends PuoliNuotti(nuotinNimi: String, extraetumerkki: String){
      override def korkeus = nuotinNimi
      override def pituus = 3.0
      override def kuvanLeveys = 16
      
      override def kuva = {
        super.kuva
        viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 5) + "." + viivasto(y(nimiMapissa)).substring(6, kuvanLeveys)  
        viivasto
      }
}
   
    
class NeljasosaNuotti(nuotinNimi: String, extraetumerkki: String = "") extends PuoliNuotti(nuotinNimi: String, extraetumerkki: String){
      override def korkeus = nuotinNimi
      override def pituus = 1.0
      override def kuvanLeveys = 8
      override def nuppi = "@@"
    
      override def kuva = {
        super.kuva
        viivasto
      }
}   
  

class PisteellinenNeljasosaNuotti(nuotinNimi: String, extraetumerkki: String = "") extends NeljasosaNuotti(nuotinNimi: String, extraetumerkki: String){
    override def korkeus = nuotinNimi
    override def pituus = 1.5
    override def kuvanLeveys = 8
    
    override def kuva = {
       super.kuva
       viivasto(y(nimiMapissa)) = viivasto(y(nimiMapissa)).substring(0, 5) + "." + viivasto(y(nimiMapissa)).substring(6, kuvanLeveys)  
       viivasto
    }
}   
  
    
class KahdeksasosaNuotti(nuotinNimi: String, extraetumerkki: String = "") extends NeljasosaNuotti(nuotinNimi: String, extraetumerkki: String){
     override def korkeus = nuotinNimi
     override def pituus = 0.5
     override def kuvanLeveys = 7
    
     override def kuva = {
        super.kuva
        piirraVarsiJaVaka
        viivasto
     }
    
     def piirraVarsiJaVaka = {
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
  
  
class KahdeksasosaPari(nuotinNimi: String, toisenNuotinNimi: String, extraetumerkki1: String, extraetumerkki2: String) extends NeljasosaNuotti(nuotinNimi: String, extraetumerkki1: String){

     override def korkeus = nuotinNimi
     def korkeus2 = toisenNuotinNimi
     override def pituus = 1.0
     override def kuvanLeveys = 12
     
     val tokaNuotti = new KahdeksasosaNuotti(toisenNuotinNimi, extraetumerkki2)
     
     /* muokkaa tätä soinnun logiikkaa:
      var ylospain = true
      if (korkeudet.min - 0 < 15 - korkeudet.max )   // 0 on ylin piirtoindeksi, 15 alin, lasketaan missä on enemmän tilaa
          ylospain = false
    */
    
     override def kuva = {
       viivasto = piirraTyhjaViivasto(kuvanLeveys)
       super.kuva
        if(tokaNuotti.etumerkki.size == 0 && tokaNuotti.extraetumerkkiDef.size == 0)  // ei etumerkkiä
            viivasto(y(tokaNuotti.nimiMapissa)) = viivasto(y(tokaNuotti.nimiMapissa)).substring(0, 8) + tokaNuotti.nuppi + viivasto(y(tokaNuotti.nimiMapissa)).substring(10, kuvanLeveys)  
         else
            viivasto(y(tokaNuotti.nimiMapissa)) = viivasto(y(tokaNuotti.nimiMapissa)).substring(0, 7) + tokaNuotti.extraetumerkkiDef + tokaNuotti.etumerkki + tokaNuotti.nuppi + viivasto(y(tokaNuotti.nimiMapissa)).substring(10, kuvanLeveys)  
       
        for(i<- 1 to 3) viivasto(y(tokaNuotti.nimiMapissa)-i) = viivasto(y(tokaNuotti.nimiMapissa)-i).substring(0, 9) + "|"  + viivasto(y(tokaNuotti.nimiMapissa)-i).substring(10, kuvanLeveys)    
     //    viivasto(y(tokaNuotti.nimiMapissa)-4) = viivasto(y(tokaNuotti.nimiMapissa)-4).substring(0, 3+1) + "="  + viivasto(y(tokaNuotti.nimiMapissa)-4).substring(5, kuvanLeveys)  
            viivasto(y(tokaNuotti.nimiMapissa)-4) = viivasto(y(tokaNuotti.nimiMapissa)-4).substring(0, 4) + "______"  + viivasto(y(tokaNuotti.nimiMapissa)-4).substring(10, kuvanLeveys)    
                       
        viivasto  
     }
  
    /*
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
    } */  
 }
 
  
class NeljasosaTauko extends Tauko {   
    def pituus = 1.0
    def kuvanLeveys = 7    
    
    def kuva = {                                    // korkeus on pelkkä piirtokorkeus
      viivasto = piirraTyhjaViivasto(kuvanLeveys) 
      viivasto(y(korkeus)) = viivasto(y(korkeus)).substring(0, 3) + "\\" +  viivasto(y(korkeus)).substring(4, kuvanLeveys)
      viivasto(y(korkeus)+1) = viivasto(y(korkeus)+1).substring(0, 3) + "/" +  viivasto(y(korkeus)+1).substring(4, kuvanLeveys)
      viivasto(y(korkeus)+2) = viivasto(y(korkeus)+2).substring(0, 3) + "\\" +  viivasto(y(korkeus)+2).substring(4, kuvanLeveys)
      viivasto(y(korkeus)+3) = viivasto(y(korkeus)+3).substring(0, 3) + "/" +  viivasto(y(korkeus)+3).substring(4, kuvanLeveys)
      viivasto 
    }
}   
  
class PisteellinenNeljasosaTauko extends NeljasosaTauko{      
      override def pituus = 1.5
      override def kuvanLeveys = 9
    
      override def kuva = {
        super.kuva  
        viivasto(y(korkeus)+3) = viivasto(y(korkeus)+3).substring(0, 4) + "." + viivasto(y(korkeus)+3).substring(5, kuvanLeveys)  
        viivasto   
      } 
}
  
class KahdeksasosaTauko extends NeljasosaTauko{      
      override def pituus = 0.5
      override def kuvanLeveys = 6
    
      override def kuva = {
         viivasto = piirraTyhjaViivasto(kuvanLeveys) 
         viivasto(y(korkeus)) = viivasto(y(korkeus)).substring(0, 2) + "/|" + viivasto(y(korkeus)).substring(4, kuvanLeveys)
         viivasto(y(korkeus)+1) = viivasto(y(korkeus)+1).substring(0, 3) + "|" + viivasto(y(korkeus)+1).substring(4, kuvanLeveys)
       viivasto   
      }
  
}
  