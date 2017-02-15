
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Viivasto(nuottiData: Buffer[ViivastolleLaitettava]) {
  
  var viivasto = piirraGavain   //Buffer[String]()
  
  val nuotitYAkselilla = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)
  
  var tahtiaMennyt = 0.0        // laskee iskuja
  var riviaMennytMontakoTahtia = 0             // halutaan max 4 tahtia riville
  var kappale = new Kappale
  
  
  
  def piirraNuotit(nuottiOliot: Buffer[ViivastolleLaitettava]) = {
    
     for (laitettava <- nuottiOliot) {
       liita(laitettava)
       tahtiaMennyt += laitettava.pituus
       if (tahtiaMennyt == 4.0){
     //     println("laitettava: " + laitettava + ", pit:" + laitettava.pituus + "riviaMennytMontakoTahtia " +riviaMennytMontakoTahtia)
          riviaMennytMontakoTahtia += 1
          tahtiaMennyt = 0.0
          lisaaTahtiviiva
       }
       if (riviaMennytMontakoTahtia == 4 ){          
           vaihdaRivi
       }
    } // for
     
     if(tahtiaMennyt != 0.0)  // pelkkää G-avainta ei haluta mukaan kappaleeseen
        vaihdaRivi   // tulee ylimääräinen G-avain jos tahteja 4, 8, 12 etc   TODO
      
  }
  
   def liita(liitosOlio: ViivastolleLaitettava) = {
     for (i <- 0 until this.viivasto.size)
         viivasto(i) += liitosOlio.kuva(i)
   }
  
  
   def lisaaTahtiviiva = {
       for(i<-nuotitYAkselilla("f2") to nuotitYAkselilla("e1"))   // ylimmästä viivasta alimpaan
         viivasto(i) += "|"      
    }
   
   
   def vaihdaRivi = {     
     for (rivi <- this.viivasto)
              println(rivi)
     kappale.lisaaViivasto(this.viivasto)
     if (riviaMennytMontakoTahtia == 4) {  // ollaan tultu for-luupin sisältä, eli alkioita on vielä käsiteltävänä
       viivasto = piirraGavain 
       this.riviaMennytMontakoTahtia = 0
     }  
   }
    
 
  def piirraGavain= {
    var g = Buffer[String]()                  

  g +="           "
  g +="           "
  g +="           "
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
 
  
    /*   TODO
    def piirraLyriikkaTavu(tavu: String) = {
  
    for(c<-tavu){
       viivasto(apuX) = tavu   // ihan sekaisin
       apuX += 1
    }   
  }  */
  
 
}