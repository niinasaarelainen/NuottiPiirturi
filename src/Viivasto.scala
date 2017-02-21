
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Viivasto(nuottiData: Buffer[ViivastolleLaitettava], lyricsBuffer: Buffer[String]) {
  
  var viivasto = piirraGavain   //Buffer[String]()
  
  val nuotitYAkselilla = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)
  
  var tahtiaMennyt = 0.0        // laskee iskuja
  var riviaMennytMontakoTahtia = 0             // halutaan max 4 tahtia riville
  var kappale = new Kappale
  
  
  def piirraNuotit(nuottiOliot: Buffer[ViivastolleLaitettava]) = {
    
    println("----@Viivasto-----")
    for(tavu <-lyricsBuffer)
      println(tavu)
    
      println("lyricsBuffer.size: " + lyricsBuffer.size)
    
     var lyricsInd = 0
     for (laitettava <- nuottiOliot) {
       
       if(!laitettava.isInstanceOf[Tauko] && lyricsBuffer.size != 0){
           laitettava.kuva(16) = laitettava.kuva(16).substring(0, 1) + lyricsBuffer(lyricsInd)  + laitettava.kuva(16).substring(1+ lyricsBuffer(lyricsInd).size, laitettava.kuvanLeveys)
           lyricsInd += 1
       }    
       liita(laitettava)
       tahtiaMennyt += laitettava.pituus
       if (tahtiaMennyt == 4.0){
     //     println("laitettava: " + laitettava + ", pit:" + laitettava.pituus + "riviaMennytMontakoTahtia " +riviaMennytMontakoTahtia)
          riviaMennytMontakoTahtia += 1
          tahtiaMennyt = 0.0
          lisaaTahtiviiva
       }
       if (riviaMennytMontakoTahtia == 4 ){      // printtaukseen 2, voisi kysyä käyttäjältä  //TODO      
           vaihdaRivi
       }
    } // for
    
    if(tahtiaMennyt != 0.0 || riviaMennytMontakoTahtia > 0 ){  // pelkkää G-avainta ei haluta mukaan kappaleeseen
        vaihdaRivi   
        lisaaTahtiviiva
    }    
  }
  
   def liita(liitosOlio: ViivastolleLaitettava) = {
     for (i <- 0 until this.viivasto.size)
         viivasto(i) += liitosOlio.kuva(i)
   }
  
  
   def lisaaTahtiviiva = {
       for(i<-nuotitYAkselilla("ylatila3") to nuotitYAkselilla("g2"))   
         viivasto(i) += " "   // tänne tyhjää, jotta mahdollisesti tänne tuleva nuotti/varsi asemoituu oikein    
       for(i<-nuotitYAkselilla("f2") to nuotitYAkselilla("e1"))   // tahtiviiva menee ylimmästä viivasta alimpaan
         viivasto(i) += "|"  
       for(i<-nuotitYAkselilla("d1") to nuotitYAkselilla("lyr"))  
         viivasto(i) += " "   // tänne tyhjää, jotta mahdollisesti tänne tuleva nuotti/lyriikka asemoituu oikein      
    }
   
   
   def vaihdaRivi = {     
     for (rivi <- this.viivasto)
         println(rivi)
     kappale.lisaaViivasto(this.viivasto)    
     viivasto = piirraGavain 
     this.riviaMennytMontakoTahtia = 0
   }
    
 
  def piirraGavain= {
    var g = Buffer[String]()                  

  g +="          "
  g +="          "
  g +="          "
  g +="    |\\    " 
  g +="----|/----" 
  g +="    /     " 
  g +="---/|-----" 
  g +="  / |     " 
  g +="-|--|__---" 
  g +=" | /|  \\  " 
  g +="-\\-\\|---|-" 
  g +="  \\_|__/  " 
  g +="----|-----" 
  g +="  \\_/     " 
  g += "          "
  g += "          "
  g += "          "
 g
}
 
  
    /*         // TODO
    def piirraLyriikkaTavu(tavu: String) = {
  
    for(c<-tavu){
       viivasto(apuX) = tavu   // ihan sekaisin
       apuX += 1
    }   
  }  */
  
 
}