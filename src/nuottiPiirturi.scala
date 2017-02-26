import scala.collection.mutable.Buffer


class NuottiPiirturi(){
   
   var pituus = 0
   var lyricsBuffer = Buffer[String]()  
   var nuottiData= Buffer[ViivastolleLaitettava]()    
   var tahdinAikaisetEtumerkit = Buffer[String]()
   
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
   val tahtilaji = inputTiedostosta.tahtilaji.toDouble 
   var iskujaMennyt =  0.0
 
   kasitteleLyriikat
   
   val inputBuffer = inputTiedostosta.nuottiAlkiot.toBuffer  
   nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)        
  
   
   var ok= 0   // nollana/pos. ok kasvattaa iskujaMennyt. Sointu asettaa arvon soinnunsävelten määrä +1 (pituus halutaan kerran) negatiiviselle
  
   
   
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {        
     
    var minutOnJoPiirretty =  false
    for ( i<- 0 until inputBuffer.length ){   
       if(!minutOnJoPiirretty){  
          minutOnJoPiirretty =  false
          var extraetumerkki = ""
          var alkio = inputBuffer(i)    // esim. "g#1--"   
          if(alkio.head != '<')
             pituus = alkio.count(_ == '-')                  
          
          if (alkio.head == '<'){                                                // S O I N N U T
            val sointu =  alkio.tail.substring(0, alkio.size -2).split(",")    
            var sointuBuffer = Buffer[String]()
            var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()
            for(aani <- sointu) 
               sointuBuffer += aani
            ok = 0- sointuBuffer.size +1    
            nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
          }      
       
          else {                                                            // N U O T I T  J A   T A U O T
             var nuotinNimi = alkio.filter(_ != '-').filter(_ != '.')  
             extraetumerkki = tutkiEtumerkit(nuotinNimi)  
           
             if (nuotinNimi == "z"){                                         //   T A U O T
               pituus match{
                  case 0 => palautetaan += new KahdeksasosaTauko; if(ok >= 0) iskujaMennyt += 0.5
                  case 1 => if(alkio.contains(".")) {palautetaan += new PisteellinenNeljasosaTauko; if(ok >= 0) iskujaMennyt += 1.5 }  
                            else {palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 1.0}
                  case 2 =>  if(alkio.contains(".")) {for (i<- 1 to 3) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 3.0; } 
                            else {for (i<- 1 to 2) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 2.0; }                          
                  case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko ; if(ok >= 0) iskujaMennyt += 3.0
                  case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 4.0
               }
           
             } else if(pituus == 1 ){                                           // N U O T I T
                 if(alkio.contains(".")){
                    palautetaan += new PisteellinenNeljasosaNuotti(nuotinNimi, extraetumerkki) 
                    if(ok >= 0) iskujaMennyt += 1.5
                 }   
                 else {
                   palautetaan += new NeljasosaNuotti(nuotinNimi, extraetumerkki)    
                   if(ok >= 0) iskujaMennyt += 1.0
                 }
             } else if (pituus == 2 ){
                if(alkio.contains(".")){
                   palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki)  
                   if(ok >= 0) iskujaMennyt += 3.0
                }   
                else {
                   palautetaan += new PuoliNuotti (nuotinNimi, extraetumerkki)  
                   if(ok >= 0) iskujaMennyt += 2.0
                }
             } else if (pituus == 3 ){
                 palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki) 
                 if(ok >= 0) iskujaMennyt += 3.0
             } else if (pituus == 4 ){
               palautetaan += new KokoNuotti(nuotinNimi,extraetumerkki)     
               if(ok >= 0) iskujaMennyt += 4.0
             } else if (pituus == 0 ){        
               if( i < inputBuffer.length -1 && inputBuffer(i+1).count(_ == '-') == 0 ){      // tahdinosa 0.0, 1.0, 2.0 tai 3.0
                  val toisenNuotinNimi = inputBuffer(i+1).filter(_ != '-').filter(_ != '.')
                  palautetaan += new KahdeksasosaPari(nuotinNimi, toisenNuotinNimi, extraetumerkki, tutkiEtumerkit(toisenNuotinNimi) )   
                  if(ok >= 0) iskujaMennyt += 1.0
                  minutOnJoPiirretty = true
               }   
               else {               
                  palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)     
                  if(ok >= 0) iskujaMennyt += 0.5
               } 
             }    
             println(nuotinNimi + " " + iskujaMennyt)
        }   // iso else: ei-sointu.
        
        if (iskujaMennyt == tahtilaji) {
           println("-----------------")
           iskujaMennyt = 0.0  
           tahdinAikaisetEtumerkit = Buffer[String]()
        }
        ok += 1
     }  else minutOnJoPiirretty=  !minutOnJoPiirretty
   }  // for 
   palautetaan   // tätä tarvitaan sointuja muodostettaessa
   } 
  
   
   def tutkiEtumerkit(nuotinNimi: String): String = {     
     
      if(nuotinNimi.contains("#") || nuotinNimi.contains("b")){
              if(this.tahdinAikaisetEtumerkit.contains(nuotinNimi))
                  return "n"   // n = nuottikuva neutral, nuotti on ylennetty/alennettu mutta merkkiä ei piirretä
              else this.tahdinAikaisetEtumerkit += nuotinNimi    
      } else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size > 0  ){
              val tulokset = tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)
              for(tulos <- tulokset)  tahdinAikaisetEtumerkit -= tulos              // otetaan esim g#1 pois puskurista, koska se on nyt §-tilassa
              return "§"   // esim f#1 ja f1 peräkkäin, tarvitaan palautusmerkki                  
      }      
      return ""        
   }
   
   
   
   def kasitteleLyriikat = {
      if(inputTiedostosta.lyriikkadata.size != 0){
       
      var sanatPotkona = ""  
      for (rivi <-  inputTiedostosta.lyriikkadata){
        sanatPotkona += rivi.replaceAll("-", "- ")
        sanatPotkona += " "
      }   
      lyricsBuffer =  sanatPotkona.replaceAll("  ", " ").split(" ").toBuffer    // entä jos 3 välilyöntiä ?  TODO  trim? milloin?
      for (tavu <- lyricsBuffer)
        println(tavu)
      }
   }
 
  
  
   var viivasto = new Viivasto(nuottiData, lyricsBuffer, inputTiedostosta.tahtilaji, inputTiedostosta.kappaleenNimi)
   viivasto.piirraNuotit(nuottiData)
   
   if(!inputTiedostosta.MIDIPatch.equals("")){    // pelkkää Enteriä ei voi muuntaa Intiksi, ja se tulkitaan niin että käyttäjä ei halua kuunnella musaa
      if(inputTiedostosta.MIDIPatch.toInt != 0 )        // käyttäjä valitsi että ei kuunnella
        new simpleMIDIPlayerAdapter(nuottiData, inputTiedostosta.MIDIPatch.toInt, viivasto.kappale, inputTiedostosta.tahtilaji.toInt)
   } else viivasto.kappale.printtaaRuudulleIlmanAjastusta()
   
   new TiedostonTallennus(viivasto.kappale)    
    
}