import scala.collection.mutable.Buffer


class NuottiPiirturi(){
   
   var pituus = 0
   var lyricsBuffer = Buffer[String]()  
   var nuottiData= Buffer[ViivastolleLaitettava]()    
   var nuottiDataParitettu= Buffer[ViivastolleLaitettava]()    
   var tahdinAikaisetEtumerkit = Buffer[String]()
   
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
   val tahtilaji = inputTiedostosta.tahtilaji.toDouble 
   var iskujaMennyt =  0.0
 
   kasitteleLyriikat()
   
   val inputBuffer = inputTiedostosta.nuottiAlkiot.toBuffer  
   nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)        
  
   
   var ok= 0   // nollana/pos. ok kasvattaa iskujaMennyt. Sointu asettaa arvon soinnunsävelten määrä +1 (pituus halutaan kerran) negatiiviselle
  
   
   
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {        
     
   
    for ( i<- 0 until inputBuffer.length ){   
        
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
                  palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)     
                  if(ok >= 0) iskujaMennyt += 0.5
             }    
             println(nuotinNimi + " " + iskujaMennyt + " tahdinAikaisetEtumerkit: " + tahdinAikaisetEtumerkit + "extraetumerkki: " + extraetumerkki)
        }   // iso else: ei-sointu.
        
        if (iskujaMennyt == tahtilaji) {
           println("-----------------")
           iskujaMennyt = 0.0  
           tahdinAikaisetEtumerkit = Buffer[String]()
        }
        ok += 1
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
   
   
   
   def kasitteleLyriikat() = {
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
 
   
   def tehdaanKahdeksasosaParit() = {      // TODO  1/8-sointu !?!?!?!?
    
     iskujaMennyt =  0.0
     var minutOnJoKasitelty = false
     var paastiinTiedostonloppuun = false
     
     for (i <- 0 until nuottiData.size-1 ){      // vikalle alkiolle ei kannata kysyä seuraajaa
        if(!minutOnJoKasitelty){
          iskujaMennyt += nuottiData(i).pituus
          if(nuottiData(i).isInstanceOf[KahdeksasosaNuotti] && (iskujaMennyt % 1== 0.5)){
              if(nuottiData(i+1).isInstanceOf[KahdeksasosaNuotti]){
                 nuottiDataParitettu += new KahdeksasosaPari(nuottiData(i).asInstanceOf[KahdeksasosaNuotti], nuottiData(i+1).asInstanceOf[KahdeksasosaNuotti])
                 minutOnJoKasitelty = true
                 iskujaMennyt += nuottiData(i+1).pituus
                 if(i+1 == nuottiData.size-1) paastiinTiedostonloppuun = true
              } else {
                 nuottiDataParitettu += nuottiData(i)     // 1/8 talteen, jos se ei löytänyt paria
              }
          }
          else {
             nuottiDataParitettu += nuottiData(i)     // ei-1/8:kin talteen
          }
        
   //       println("iskujaMennyt: " +iskujaMennyt + nuottiData(i).asInstanceOf[KahdeksasosaNuotti].korkeus )
          if (iskujaMennyt == tahtilaji) {
             iskujaMennyt = 0.0  
          }  
      
        }  else  minutOnJoKasitelty=  false
      }
     if(!paastiinTiedostonloppuun)    
          nuottiDataParitettu += nuottiData(nuottiData.size-1)   // viimeinenkin nuottiolio messiin, jos se ei ollut 1/8-parin puolisko
   }  
  
  
   tehdaanKahdeksasosaParit()
   
  // for(nuottiOlio <- nuottiDataParitettu)
  //   println(nuottiOlio)
   
   val viivasto = new Viivasto(nuottiDataParitettu, lyricsBuffer, inputTiedostosta.tahtilaji, inputTiedostosta.kappaleenNimi)
   viivasto.piirraNuotit()
   
   if(!inputTiedostosta.MIDIPatch.equals(""))     // käyttäjä valitsi että ei kuunnella
        new simpleMIDIPlayerAdapter(nuottiData, inputTiedostosta.MIDIPatch.toInt, viivasto.kappale, inputTiedostosta.tahtilaji.toInt)
   else viivasto.kappale.printtaaRuudulleIlmanAjastusta()
   
   new TiedostonTallennus(viivasto.kappale)    
    
}