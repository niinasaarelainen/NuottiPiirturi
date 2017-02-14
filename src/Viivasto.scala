
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Viivasto(nuottiData: Buffer[ViivastolleLaitettava]) {
  
  var viivasto = Buffer[String]()
  val nuotitYAkselilla = Map("lyr" -> 16, "alatila" ->15, "c1" -> 14, "d1" -> 13,  "e1" -> 12,  "f1" -> 11,  "g1"-> 10,  "a1"->9,  "h1" -> 8, "c2" -> 7, "d2" -> 6,  "e2" -> 5,  "f2" -> 4,  "g2"-> 3,  "ylatila1"-> 2, "ylatila2" -> 1, "ylatila3" -> 0)

  
  val viiva = "----------"    // 10
  val vali =  "          "  
  var x = 15  // nuottiavain vie tilaa
  var tahtiviivaLaskuri = 0.0        // laskee iskuja
  var tahtiviivaLkm = 0             // halutaan max 4 tahtia riville
  
  muodostaViivasto
  
  
  def muodostaViivasto = {        // tämä luokassa ViivastolleLaitettava 
      for ( i <- 1 to 3)
          viivasto += vali      // ylös tyhjää varsia varten   
      for(i <- 1 to 5){
        viivasto += vali        // ylin paikka on g2
        viivasto += viiva       // 5 viivaa
       }
       for ( i <- 1 to 4)
          viivasto += vali         // d1, c1, alavali & sanoille tila
   //    mergeViivastoJaGavain          
  }    
  
  
  def piirraApuviiva = {
    this.x += 1    // apuviiva saattaa tuhota muuten etumerkin
      // if(nuotinNimi.contains("c1")){                 
        //  viivasto.piirraApuviiva
     //  }
  }
  

  
  def piirraNuotit(nuottiOliot: Buffer[ViivastolleLaitettava]) = {
    
   var montakoOliotaMahtuuViivastolle = 0
 //  for (n <- 0 until nuottiOliot.size){     // luetaan n oliota, seuraava luenta alkaa n:stä   REKURSIO !?!?
  //  println(data(0).kuva.size + "; " + data.size)
    // tahtiviivaLaskuri += nuottiOliot(n).pituus
    // lopullinen :   for (i <- 0 until data.size){
      // 4 nuottia peräkkäin                   
        for(j <- 0 until nuottiOliot(0).kuva.size ){ 
           for (i <- 0 until nuottiOliot.size){                  // j= rivi    i = nuottikuva
              print(nuottiOliot(i).kuva(j)) 
           }  
        println()   
        }
        
        
        
 // } // endnuottiOliot.size
   
  }
  
  
   def lisaaTahtiviiva = {
       for(i<-4 to 12)   // piirtää f2:sta e1:een
         viivasto(i) = "|"      
    }
   
   def liita = {
     
   }
   
   def vaihdaRivi = {
     
   }
   
    def piirraLyriikkaTavu(tavu: String) = {
   
    var apuX = x
    for(c<-tavu){
       viivasto(apuX) = tavu   // ihan sekaisin
       apuX += 1
    }   
  }
 
    /*
  def tutkiEtumerkit(solu: String, x:Int) = {
    var nuotinNimiTemp = ""
    for(c<- solu){
        if (c != '-'){
          if(c == '#' )
            piirraYlennysMerkki(nuotinNimiTemp, x)            
          else if(c == 'b' )  
            piirraAlennusMerkki(nuotinNimiTemp, x) 
          else   nuotinNimiTemp += c.toString   
        }   
     }    
     nuotinNimiTemp    
  } */  
 
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