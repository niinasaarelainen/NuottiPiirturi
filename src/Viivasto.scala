
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map

class Viivasto(nuottiData: Buffer[ViivastolleLaitettava], lyricsBuffer: Buffer[String], tahtilaji:String, kappaleenNimi:String) {
  
  var viivasto = piirraGavain()   //Buffer[String]()
  
   val nuotitYAkselilla = Map("lyr" -> 18, "alatila" ->17, "c1" -> 16, "d1" -> 15,  "e1" -> 14,  "f1" -> 13,  "g1"-> 12,  "a1"->11,  "h1" -> 10, "b1" -> 10, "c2" -> 9, "d2" -> 8,  "e2" -> 7,  "f2" -> 6,  "g2"-> 5,  "a2"-> 4, "h2" -> 3, "b2" -> 3, "ylatila2" -> 2, "ylatila3" -> 1, "ylatila4" -> 0)

  var tahtiaMennyt = 0.0        // laskee iskuja
  var riviaMennytMontakoTahtia = 0             // halutaan max 4 tahtia riville
  var kappale = new Kappale
  this.kappale.lisaaKappaleenNimi(kappaleenNimi)
  var lyricsInd = 0
  
 
    
  def piirraNuotit() = {
     for (laitettava <- nuottiData) {      
                      // jos sanoja oli vähemmän kuin nuotteja, ei haluta kaataa ohjelmaa
        if(laitettava.soiva && lyricsBuffer.size != 0 && lyricsBuffer.size - lyricsInd >= 1 ){     // nuotti ja sointu => soiva= true, tauko=> soiva=false
           kasitteleLyriikat(laitettava) 
        }    
        liita(laitettava)
        tahtiaMennyt += laitettava.pituus
        if (tahtiaMennyt == tahtilaji.toDouble){
     //     println("laitettava: " + laitettava + ", pit:" + laitettava.pituus + "riviaMennytMontakoTahtia " +riviaMennytMontakoTahtia)
          riviaMennytMontakoTahtia += 1
          tahtiaMennyt = 0.0
          lisaaTahtiviiva()
        }
        if (riviaMennytMontakoTahtia == 2 ){      // printtaukseen 2, voisi kysyä käyttäjältä  //TODO      
           vaihdaRivi()
        }
     } // end for, kaikki nuottiData käsitelty
    
     lisaaTahtiviiva()   // biisin lopetusviiva, tulee vain vajaissa riveissä    TODO  : myös täyteen riviin    
    
     if(tahtiaMennyt != 0.0 || riviaMennytMontakoTahtia > 0 ){  // pelkkää G-avainta ei haluta mukaan kappaleeseen
        vaihdaRivi()   
        lisaaTahtiviiva()       
     }    
  }
  
  
  def kasitteleLyriikat(laitettava: ViivastolleLaitettava) = {
     
      var tavu, tavu2 = ""
      if(!laitettava.isInstanceOf[KahdeksasosaPari])
           kasitteleYksiTavu(laitettava.kuvanLeveys)
      else if(lyricsBuffer.size - lyricsInd >= 2 ) 
           kasitteleKaksiTavua()   
      else kasitteleYksiTavu(laitettava.kuvanLeveys)   
      
      
      def kasitteleYksiTavu(leveys: Int) = {
         // leikataan liian pitkien lyriikkatavujen loput:
         if ( lyricsBuffer(lyricsInd).size > leveys){
             tavu = lyricsBuffer(lyricsInd).substring(0, leveys)
         } else tavu = lyricsBuffer(lyricsInd)
         // keskitetään lyriikkatavuja lähemmäs nuottia:
         if (leveys - tavu.size < 2)
             laitettava.kuva(18) =  tavu  + laitettava.kuva(18).substring(tavu.size)
              else if (leveys - tavu.size == 2) 
              laitettava.kuva(18) = " " + tavu  + laitettava.kuva(18).substring(tavu.size+1)
         else if (leveys - tavu.size < 4) 
              laitettava.kuva(18) = "  " + tavu  + laitettava.kuva(18).substring(tavu.size+2)
         else  laitettava.kuva(18) = "   " + tavu  + laitettava.kuva(18).substring(tavu.size+3)
           lyricsInd += 1
      }     
      
     
      def kasitteleKaksiTavua() = {
         kasitteleYksiTavu(laitettava.kuvanLeveys/2)    // ekan tavun tila on puolet parin tilasta   TODO miksi  laitettava.kuvanLeveys/2 kaataa??
         
         if ( lyricsBuffer(lyricsInd).size > laitettava.kuvanLeveys/2){ 
              tavu2 = lyricsBuffer(lyricsInd).substring(0, laitettava.kuvanLeveys/2 -1)
         } else {
              tavu2 = lyricsBuffer(lyricsInd)
         }
         // keskitetään lyriikkatavuja lähemmäs nuottia:
         if (laitettava.kuvanLeveys/2 - tavu2.size < 2)              
             laitettava.kuva(18) = laitettava.kuva(18).substring(0,6) + tavu2 + laitettava.kuva(18).substring(6 + tavu2.size)
         else if (laitettava.kuvanLeveys/2 - tavu2.size < 3) 
              laitettava.kuva(18) =  laitettava.kuva(18).substring(0,6) + " " +  tavu2 + laitettava.kuva(18).substring(6 + tavu2.size + 1)
         else  laitettava.kuva(18) =  laitettava.kuva(18).substring(0,6) + "  " + tavu2 + laitettava.kuva(18).substring(6 + tavu2.size +2)
         lyricsInd += 1
      }
        
  }
  
  
  
  def liita(liitosOlio: ViivastolleLaitettava) = {
     for (i <- 0 until this.viivasto.size)
         viivasto(i) += liitosOlio.kuva(i)
  }
  
  
   
  def lisaaTahtiviiva() = {
       for(i<-nuotitYAkselilla("ylatila4") to nuotitYAkselilla("g2"))   
         viivasto(i) += " "                                       // tänne tyhjää, jotta mahdollisesti tänne tuleva nuotti/varsi asemoituu oikein    
       for(i<-nuotitYAkselilla("f2") to nuotitYAkselilla("e1"))   // tahtiviiva menee ylimmästä viivasta alimpaan
         viivasto(i) += "|"  
       for(i<-nuotitYAkselilla("d1") to nuotitYAkselilla("lyr"))  
         viivasto(i) += " "                                       // tänne tyhjää, jotta mahdollisesti tänne tuleva nuotti/lyriikka asemoituu oikein      
  }
   
   
   
  def vaihdaRivi() = {     
     for (rivi <- this.viivasto){
     }    
     kappale.lisaaViivasto(this.viivasto)    
     viivasto = piirraGavain 
     this.riviaMennytMontakoTahtia = 0
  }
    
 
   
  def piirraGavain() = {
     var g = Buffer[String]()                  

     g +="          "
     g +="          "
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
 
 
}