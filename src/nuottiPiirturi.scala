import scala.collection.mutable.Buffer


class NuottiPiirturi(input: String, var tahtilaji: String = "4", lyrics: String = ""){
   
   var pituus = 0
   var lyricsBuffer = Buffer[String]()  
   var nuottiData= Buffer[ViivastolleLaitettava]()    
   var tahdinAikaisetEtumerkit = Buffer[String]()
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
 
   kasitteleLyriikat
   
   val inputBuffer = inputTiedostosta.nuottiAlkiot.toBuffer  
   nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData, 0.0)      
  
  
   tahtilaji = inputTiedostosta.tahtilaji  
   var iskujaMennyt =  0.0
   
   var ok= 0   // nollana/pos. ok kasvattaa iskujaMennyt. Sointu asettaa arvon soinnunsävelten määrä +1 (pituus halutaan kerran) negatiiviselle
   
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava], iskujaMennytMuuttuja:Double ): Buffer[ViivastolleLaitettava] = {        
     
    var iskujaMennyt = iskujaMennytMuuttuja
    
     for ( i<- 0 until inputBuffer.length){        
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
          nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer, iskujaMennyt) ) 
        }      
       
        else {                                                            // N U O T I T  J A   T A U O T
           var nuotinNimi = alkio.filter(_ != '-').filter(_ != '.')  
           extraetumerkki = tutkiEtumerkit(nuotinNimi)  
           
           if (nuotinNimi == "z"){                                         //   T A U O T
              pituus match{
                 case 0 => palautetaan += new KahdeksasosaTauko
                 case 1 => if(alkio.contains(".")) palautetaan += new PisteellinenNeljasosaTauko  else palautetaan += new NeljasosaTauko
                 case 2 => for (i<- 1 to 2) palautetaan += new NeljasosaTauko
                 case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko
                 case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko
              }
           
           } else if(pituus == 1 ){                                           // N U O T I T
               if(alkio.contains(".")){
                  palautetaan += new PisteellinenNeljasosaNuotti(nuotinNimi, extraetumerkki) 
                  iskujaMennyt += 1.5
               }   
               else {
                 palautetaan += new NeljasosaNuotti(nuotinNimi, extraetumerkki)    
                 iskujaMennyt += 1.0
               }
            } else if (pituus == 2 ){
               if(alkio.contains(".")){
                  palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki)  
                  iskujaMennyt += 3.0
               }   
               else {
                 palautetaan += new PuoliNuotti (nuotinNimi, extraetumerkki)  
                 iskujaMennyt += 2.0
               }
            } else if (pituus == 3 ){
                palautetaan += new PisteellinenPuoliNuotti(nuotinNimi,extraetumerkki) 
                iskujaMennyt += 3.0
            } else if (pituus == 4 ){
               palautetaan += new KokoNuotti(nuotinNimi,extraetumerkki)     
               iskujaMennyt += 4.0
            } else if (pituus == 0 ){         // kahdeksasosa
      //      if( i < inputArray.length -1 && inputArray(i+1).count(_ == '-') == 0 ){
           //   piirraKahdeksasosaPari(nuotinNimi, inputArray(i+1))
       //    }   
        //  else {   
            
                palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)     // TODO  ei voi luoda ennen seuraavan alkion tutkimista !!)
                if(ok >= 0) iskujaMennyt += 0.5
            } 
           println(nuotinNimi + " " + iskujaMennyt)
      }   // iso else: ei-sointu.
      if (iskujaMennyt == tahtilaji.toInt) {
        println("--------------------------")
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
      } else if ( this.tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last).size == 1  ){
              val tulos = tahdinAikaisetEtumerkit.filter(_.head == nuotinNimi.head).filter(_.last == nuotinNimi.last)
              tahdinAikaisetEtumerkit -= tulos(0)              // otetaan esim g#1 pois puskurista, koska se on nyt §-tilassa
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
      lyricsBuffer =  sanatPotkona.replaceAll("  ", " ").split(" ").toBuffer  
      for (tavu <- lyricsBuffer)
        println(tavu)
      }
   }
 
 
  
   var viivasto = new Viivasto(nuottiData, lyricsBuffer, inputTiedostosta.tahtilaji, inputTiedostosta.kappaleenNimi)
   viivasto.piirraNuotit(nuottiData)
 
   if(inputTiedostosta.MIDIPatch.toInt != 0)  // käyttäjä valitsi että ei kuunnella
      new simpleMIDIPlayerAdapter(nuottiData, inputTiedostosta.MIDIPatch.toInt)
   
   val kappale =  new Kappale    //Buffer[Buffer[String]]()
   new TiedostonTallennus(viivasto.kappale)    
    
}