// package piirturi.src

import scala.collection.mutable.Buffer


class NuottiPiirturi(lukija: TiedostonLukeminen, MIDIPatch: String){
   
    
   var nuottiData= Buffer[ViivastolleLaitettava]()    
   var nuottiDataParitettu= Buffer[ViivastolleLaitettava]()    
   var tahdinAikaisetEtumerkit = Buffer[String]()     // Set olisi muuten hyvä mutta on immutable
   var lyricsBuffer = Buffer[String]()  
 //  val inputTiedostosta = new TiedostonLukeminen
  // inputTiedostosta.lueTiedosto()
   val tahtilaji = lukija.tahtilaji.toDouble 
   val inputBuffer = lukija.nuottiAlkiot.toBuffer  
   
 
     
       nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)    
       if(lukija.lyriikkadata.size != 0)
           kasitteleLyriikat() 
       tehdaanKahdeksasosaParit()
       
       
         
       val viivasto = new Viivasto(nuottiDataParitettu, lyricsBuffer, lukija.tahtilaji, lukija.kappaleenNimi)
       viivasto.piirraNuotit()
       
        // jos kuunnellaan, tallennuskäsky pitää antaa kuuntelun jälkeen, muuten se tulee ruudulle ennen nuotteja
       if(!MIDIPatch.equals(""))  // kuunnellaan  
            new simpleMIDIPlayerAdapter(nuottiData, MIDIPatch.toInt, viivasto.kappale, lukija.tahtilaji.toInt)
       else {        // käyttäjä valitsi että ei kuunnella
           viivasto.kappale.printtaaRuudulleIlmanAjastusta()
           new TiedostonTallennus(viivasto.kappale)    
       }                                               
  
 
     
 
///// F U N K T I O T  ja niihin liittyvät muuttujat (ei voi määritellä funktion sisällä rekursion takia): /////////////////////////  
   
   var iskujaMennyt =  0.0
   var ok= 0   // nollana/positiivisena ok kasvattaa iskujaMennyt. 
                //   Sointu asettaa negatiivisen arvon (soinnunsävelten määrä +1) - pituus halutaan kerran - 
  
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {   
  
    for ( alkio <- inputBuffer ){      // alkio esim. "g#1--"   tai   "<f1,d1>"
   
      // S O I N N U T
        if (alkio.head == '<')
            kasitteleSoinnut(alkio)
       
      // N U O T I T  J A   T A U O T 
        else {                                                            
            var nuotinNimi = alkio.filter(_ != '-').filter(_ != '.')  
            var pituus = alkio.count(_ == '-')     
            var extraetumerkki = tutkiEtumerkit(nuotinNimi)  
           
            /* jos tauot yrittää siirtää omaksi sisäkkäiseksi funktioksi, tulee valitus reassignment to val palautetaan */
            
            if (nuotinNimi == "z"){                                 //   T A U O T
               pituus match{
                  case 0 =>  palautetaan += new KahdeksasosaTauko; if(ok >= 0) iskujaMennyt += 0.5
                  case 1 =>  if(alkio.contains(".")) {palautetaan += new PisteellinenNeljasosaTauko; if(ok >= 0) iskujaMennyt += 1.5 }  
                             else {palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 1.0}
                  case 2 =>  if(alkio.contains(".")) {for (i<- 1 to 3) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 3.0; } 
                             else {for (i<- 1 to 2) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 2.0; }                          
                  case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko ; if(ok >= 0) iskujaMennyt += 3.0
                  case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko; if(ok >= 0) iskujaMennyt += 4.0
            }
           
            } else if(pituus == 1 ){                                  // N U O T I T
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
        //     println(nuotinNimi + " " + iskujaMennyt + " tahdinAikaisetEtumerkit: " + tahdinAikaisetEtumerkit + "tahtilaji: " + tahtilaji + "extraetumerkki: " + extraetumerkki)
        }   // iso else: ei-sointu.
        
        if (iskujaMennyt == tahtilaji) {
           println("-----------------")
           iskujaMennyt = 0.0  
           tahdinAikaisetEtumerkit = Buffer[String]()
        }
        ok += 1
     }  // for 
  
   
   
   def kasitteleSoinnut(alkio: String) = {
                                                     
            val sointu =  alkio.tail.substring(0, alkio.size -2).split(",")    
            var sointuBuffer = Buffer[String]()
            var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()
            for(aani <- sointu) 
               sointuBuffer += aani
            ok = 0- sointuBuffer.size +1    
            nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
   }
   
   
    palautetaan   // tätä tarvitaan sointuja muodostettaessa
   } 
  
   
   def tutkiEtumerkit(nuotinNimi: String): String = {  
     
      println(nuotinNimi + ", " + tahdinAikaisetEtumerkit)
     
      // tutkitaan ylennettyjä ja alennettuja nuotteja
      if(nuotinNimi.contains("#") || nuotinNimi.contains("b")){
              if(this.tahdinAikaisetEtumerkit.contains(nuotinNimi))
                  return "n"   // n = nuottikuva neutral, nuotti on ylennetty/alennettu mutta merkkiä ei piirretä
              // case: puskurissa oli sama nuotti eri etumerkillä varustettuna, se pois    
              else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size > 0 ){
                  tahdinAikaisetEtumerkit -= tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)(0)
                  this.tahdinAikaisetEtumerkit += nuotinNimi 
              }  // tai vain lisätään puskuriin
              else  this.tahdinAikaisetEtumerkit += nuotinNimi   
              
             
             
     // selvitetään tarvitaanko palautusmerkkiä         
      } else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size > 0  ){
             val tulokset = tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)
             for(tulos <- tulokset)  tahdinAikaisetEtumerkit -= tulos              // otetaan esim g#1 pois puskurista, koska se on nyt §-tilassa
              // vielä pari poikkeustapausta johtuen epäloogisesta nuotinnimestä b
             if ( this.tahdinAikaisetEtumerkit.contains("b1") && nuotinNimi == "h1"){
                tahdinAikaisetEtumerkit -= "b1"
             } else if ( this.tahdinAikaisetEtumerkit.contains("b2") && nuotinNimi == "h2"){
                tahdinAikaisetEtumerkit -= "b2"
             }  
             return "§"                  
      } 
      
      return ""        // muutoin ei tarvita puskuritoimenpiteitä tai extraetumerkkiä
   }
   
   
   
   def kasitteleLyriikat() = {
         for (rivi <-  lukija.lyriikkadata){
            var splitattuRivi = rivi.replaceAll("-", "- ").split(" ")
            for (alkio <- splitattuRivi) 
                if (alkio != "") lyricsBuffer += alkio
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
                 if(nuottiData.last.eq(nuottiData(i+1))) paastiinTiedostonloppuun = true
              } else {
                 nuottiDataParitettu += nuottiData(i)     // 1/8 talteen, jos se ei löytänyt paria
              }
          }
          
          
      /*    
          else if(nuottiData(i).isInstanceOf[Sointu] && nuottiData(i).asInstanceOf[Sointu].pituus == 0.5 && (iskujaMennyt % 1== 0.5)){
              if(nuottiData(i+1).isInstanceOf[KahdeksasosaNuotti]){
                 nuottiDataParitettu += new KahdeksasosaPariSointuNuotti(nuottiData(i).asInstanceOf[Sointu], nuottiData(i+1).asInstanceOf[KahdeksasosaNuotti])
                 minutOnJoKasitelty = true  
                 iskujaMennyt += nuottiData(i+1).pituus
                 if(nuottiData.last.eq(nuottiData(i+1))) paastiinTiedostonloppuun = true
              } else {
                 nuottiDataParitettu += nuottiData(i)     // 1/8 talteen, jos se ei löytänyt paria
              } 
             
          }   */
          
          
          
          else {
             nuottiDataParitettu += nuottiData(i)     // ei-1/8:kin talteen
          }
        
          if (iskujaMennyt == tahtilaji) {
             iskujaMennyt = 0.0  
          }  
        }  else  minutOnJoKasitelty=  false
      }
     if(!paastiinTiedostonloppuun)    
          nuottiDataParitettu += nuottiData(nuottiData.size-1)   // viimeinenkin nuottiolio messiin, jos se ei ollut 1/8-parin puolisko
   }  
  
  
}