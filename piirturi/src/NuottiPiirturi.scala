import scala.collection.mutable.Buffer


class NuottiPiirturi(lukija: TiedostonLukeminen){
    
   var nuottiData= Buffer[ViivastolleLaitettava]()    
   var nuottiDataParitettu= Buffer[ViivastolleLaitettava]()    
   var tahdinAikaisetEtumerkit = Buffer[String]()     // Set olisi muuten hyvä mutta on immutable
   var lyricsBuffer = Buffer[String]()  
   val tahtilaji = lukija.tahtilaji.toDouble 
   val inputBuffer = lukija.nuottiAlkiot.toBuffer  
   var viivasto:Viivasto = _   // object variable without initialization
 
   
   // P Ä Ä M E T O D I: 
   def execute() = {
       nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData) 
       if(lukija.lyriikkadata.size != 0) kasitteleLyriikat() 
       tehdaanKahdeksasosaParit()
              
       this.viivasto = new Viivasto(nuottiDataParitettu, lyricsBuffer, lukija.tahtilaji )
       this.viivasto.kappale.lisaaKappaleenNimi(lukija.kappaleenNimi)
       this.viivasto.piirraNuotit()
   }     
          
 
   /////  M U U T   M E T O D I T  ja niihin liittyvät muuttujat (ei voi määritellä metodin sisällä rekursion takia): /////////////////////////  
   
   var iskujaMennyt =  0.0
   var kasvataIskuja= 0   // nollana/positiivisena kasvataIskuja kasvattaa iskujaMennyt. 
                //   Sointu asettaa negatiivisen arvon (soinnunsävelten määrä +1) - pituus halutaan _VAIN_ kerran - 
  
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
             
              /* jos tauot yrittää siirtää omaksi sisäkkäiseksi metodiksi, tulee valitus reassignment to val palautetaan */
              
              if (nuotinNimi == "z"){                    //   T A U O T
                 pituus match{
                    case 0 =>  palautetaan += new KahdeksasosaTauko;  iskujaMennyt += 0.5
                    case 1 =>  if(alkio.contains(".")) {palautetaan += new PisteellinenNeljasosaTauko; iskujaMennyt += 1.5 }  
                               else {palautetaan += new NeljasosaTauko;  iskujaMennyt += 1.0}
                    case 2 =>  if(alkio.contains(".")) {for (i<- 1 to 3) palautetaan += new NeljasosaTauko;  iskujaMennyt += 3.0; } 
                               else {for (i<- 1 to 2) palautetaan += new NeljasosaTauko;  iskujaMennyt += 2.0; }                          
                    case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko; iskujaMennyt += 3.0
                    case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko; iskujaMennyt += 4.0
              }
             
              } else if(pituus == 1 ){                    // N U O T I T
                   if(alkio.contains(".")){
                      palautetaan += new PisteellinenNeljasosaNuotti(nuotinNimi, extraetumerkki) 
                      if(kasvataIskuja >= 0) iskujaMennyt += 1.5
                   }   
                   else {
                     palautetaan += new NeljasosaNuotti(nuotinNimi, extraetumerkki)    
                     if(kasvataIskuja >= 0) iskujaMennyt += 1.0
                   }
              } else if (pituus == 2 ){
                  if(alkio.contains(".")){
                     palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki)  
                     if(kasvataIskuja >= 0) iskujaMennyt += 3.0
                  }   
                  else {
                    palautetaan += new PuoliNuotti (nuotinNimi, extraetumerkki)  
                    if(kasvataIskuja >= 0) iskujaMennyt += 2.0
                  }
              } else if (pituus == 3 ){
                    palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki) 
                    if(kasvataIskuja >= 0) iskujaMennyt += 3.0
              } else if (pituus == 4 ){
                    palautetaan += new KokoNuotti(nuotinNimi,extraetumerkki)     
                    if(kasvataIskuja >= 0) iskujaMennyt += 4.0
              } else if (pituus == 0 ){           
                    palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)     
                    if(kasvataIskuja >= 0) iskujaMennyt += 0.5
              }    
          }   // iso else: ei-sointu.
          
          if (iskujaMennyt == tahtilaji) {
             iskujaMennyt = 0.0  
             tahdinAikaisetEtumerkit = Buffer[String]()
          }
          kasvataIskuja += 1
       }  // for 
    
     
     
       def kasitteleSoinnut(alkio: String) = {  
              var sointuBuffer = alkio.tail.substring(0, alkio.size -2).split(",").toBuffer   
              var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()           
              kasvataIskuja = 0- sointuBuffer.size +1    
              nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
       }
      
   palautetaan   
   } //end KasitteleNuottiTieto
  
   
   def tutkiEtumerkit(nuotinNimi: String): String = {  
     
      // tutkitaan ylennettyjä ja alennettuja nuotteja
      if(nuotinNimi.contains("#") || nuotinNimi.contains("b")){
              if(this.tahdinAikaisetEtumerkit.contains(nuotinNimi))
                  return "n"   // n = nuottikuva neutral, nuotti on ylennetty/alennettu mutta merkkiä ei piirretä
              
              // case: puskurissa oli sama nuotti eri etumerkillä varustettuna, se pois :   
              else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size > 0 ){
                  tahdinAikaisetEtumerkit -= tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)(0)
                  this.tahdinAikaisetEtumerkit += nuotinNimi 
              } 
              
              // nuotinnimestä b johtuen erikoistapaukset:
              else if ( this.tahdinAikaisetEtumerkit.contains("b1") && nuotinNimi == "h#1"){
                   tahdinAikaisetEtumerkit -= "b1"
                   this.tahdinAikaisetEtumerkit += nuotinNimi   
              } else if ( this.tahdinAikaisetEtumerkit.contains("b2") && nuotinNimi == "h#2"){
                   tahdinAikaisetEtumerkit -= "b2"  
                   this.tahdinAikaisetEtumerkit += nuotinNimi   
              } else if ( this.tahdinAikaisetEtumerkit.contains("h#1") && nuotinNimi == "b1"){
                   tahdinAikaisetEtumerkit -= "h#1"
                   this.tahdinAikaisetEtumerkit += nuotinNimi   
              } else if ( this.tahdinAikaisetEtumerkit.contains("h#2") && nuotinNimi == "b2"){
                   tahdinAikaisetEtumerkit -= "h#2"  
                   this.tahdinAikaisetEtumerkit += nuotinNimi       
              }     
              
              // tai vain lisätään puskuriin:
              else  this.tahdinAikaisetEtumerkit += nuotinNimi   
            
      // selvitetään tarvitaanko palautusmerkkiä         
      } else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size > 0  ){
             val tulokset = tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)
             for(tulos <- tulokset)  tahdinAikaisetEtumerkit -= tulos              // otetaan esim g#1 pois puskurista, koska se on nyt §-tilassa
             return "§"    
      }        
      // vielä pari poikkeustapausta johtuen epäloogisesta nuotinnimestä b
      else if ( this.tahdinAikaisetEtumerkit.contains("b1") && nuotinNimi == "h1"){
                tahdinAikaisetEtumerkit -= "b1"
                return "§"  
      } else if ( this.tahdinAikaisetEtumerkit.contains("b2") && nuotinNimi == "h2"){
                tahdinAikaisetEtumerkit -= "b2"
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
 
 
   
   def tehdaanKahdeksasosaParit() = {   
         iskujaMennyt =  0.0
         var minutOnJoKasitelty = false
         var paastiinTiedostonloppuun = false
         
         for (i <- 0 until nuottiData.size-1 ){      // vikalle alkiolle ei kannata kysyä seuraajaa
              if(!minutOnJoKasitelty){
                  iskujaMennyt += nuottiData(i).pituus
                  
                  nuottiData(i) match {
                      case k1: KahdeksasosaNuotti if(iskujaMennyt % 1 == 0.5) => nuottiData(i+1) match {
                           case k2: KahdeksasosaNuotti => nuottiDataParitettu += new KahdeksasosaPari(k1, k2)
                                                          minutOnJoKasitelty = true  
                                                          iskujaMennyt += k2.pituus
                                                          if(nuottiData.last.eq(k2)) paastiinTiedostonloppuun = true
                           case _ =>  nuottiDataParitettu += k1  // 1/8 talteen, jos se ei löytänyt paria
                      }
                      case _ =>  nuottiDataParitettu += nuottiData(i)     // ei-1/8:kin talteen                       
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