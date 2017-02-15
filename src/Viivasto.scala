
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Viivasto(nuottiData: Buffer[ViivastolleLaitettava]) {
  
  var viivasto = Buffer[String]()
  
  val nuotitYAkselilla = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)
  
  val viiva = "----------"    // 10
  val vali =  "          "  
  var tahtiaMennyt = 0.0        // laskee iskuja
  var riviaMennytMontakoTahtia = 0             // halutaan max 4 tahtia riville
  var kappale = new Kappale
  
  muodostaViivasto
  
  
  def piirraNuotit(nuottiOliot: Buffer[ViivastolleLaitettava]) = {
    
     for (laitettava <- nuottiOliot) {
       liita(laitettava)
       tahtiaMennyt += laitettava.pituus
       if (tahtiaMennyt == 4.0){
          riviaMennytMontakoTahtia += 1
          tahtiaMennyt = 0.0
          lisaaTahtiviiva
       }
       if (riviaMennytMontakoTahtia == 4 ){
          // vaihdaRivi
       }
    } // for
     
     for (rivi <- this.viivasto)
       println(rivi)
  }
  
  
   def lisaaTahtiviiva = {
       for(i<-4 to 12)   // piirtää f2:sta e1:een
         viivasto(i) += "|"      
    }
   
   def liita(liitosOlio: ViivastolleLaitettava) = {
     for (i <- 0 until this.viivasto.size)
         viivasto(i) += liitosOlio.kuva(i)
   }
   
   def vaihdaRivi = {     
     kappale.lisaaViivasto(this.viivasto)
     
   }
   
    def muodostaViivasto = {     
      
      /*
      for ( i <- 1 to 3)
          viivasto += vali      // ylös tyhjää varsia varten   
      for(i <- 1 to 5){
        viivasto += vali        // ylin paikka on g2
        viivasto += viiva       // 5 viivaa
       }
       for ( i <- 1 to 4)
          viivasto += vali         // d1, c1, alavali & sanoille tila  */
     viivasto = piirraGavain          
  }    
  
  
  def piirraApuviiva = {
      // if(nuotinNimi.contains("c1")){                 
        //  viivasto.piirraApuviiva
     //  }
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
 
  
    /*
    def piirraLyriikkaTavu(tavu: String) = {
  
    for(c<-tavu){
       viivasto(apuX) = tavu   // ihan sekaisin
       apuX += 1
    }   
  }  */
  
  /*
  def mergeViivastoJaGavain = {
   val g = piirraGavain 
   for(rivi<-0 to 16){    // ylä- ja alatyhjät mukaanlukien nuottiviivaston korkeus == 15
          for(i<-0 until 10){     // nuottiavain on 10 merkkiä leveä
            if(g(rivi).charAt(i) != viivasto(rivi)(i))
              viivasto(rivi)(i) = g(rivi).charAt(i)
          }  
   }    
  }  */
  
}