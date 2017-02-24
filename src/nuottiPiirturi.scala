import scala.collection.mutable.Buffer


class NuottiPiirturi(input: String, var tahtilaji: Int = 4, lyrics: String = ""){
   
   var pituus = 0
   var lyricsBuffer = Buffer[String]()  
   var nuottiData= Buffer[ViivastolleLaitettava]()    
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
 
   kasitteleLyriikat
   
   val inputBuffer = inputTiedostosta.nuottiAlkiot.toBuffer  
   nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)      
  
  
   
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {        
     
     val tahtilaji = inputTiedostosta.tahtilaji.toDouble    
     var iskujaMennyt =  0.0
     var tahdinAikaisetEtumerkit = Buffer[String]()
     
     
     for ( i<- 0 until inputBuffer.length){        
        var extraetumerkki = ""
        var alkio = inputBuffer(i)    // esim. "g#1--"   
        if(alkio.head != '<')
           pituus = alkio.count(_ == '-')                  
          
        if (alkio.head == '<'){
          val sointu =  alkio.tail.substring(0, alkio.size -2).split(",")    
          var sointuBuffer = Buffer[String]()
          var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()
          for(aani <- sointu) 
             sointuBuffer += aani
          nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
        }      
       
        else {        
           var nuotinNimi = alkio.filter(_ != '-').filter(_ != '.')    
           if(nuotinNimi.contains("#") || nuotinNimi.contains("b"))
              if(tahdinAikaisetEtumerkit.contains(nuotinNimi))
                  nuotinNimi += "n"   // n = neutral, nuotti on ylennetty/alennettu mutta merkkiä ei piirretä
              else if (tahdinAikaisetEtumerkit.contains(nuotinNimi.filter(_ != '#')) || tahdinAikaisetEtumerkit.contains(nuotinNimi.filter(_ != 'b')))
                  extraetumerkki = "§"
              else tahdinAikaisetEtumerkit += nuotinNimi
           if (nuotinNimi == "z"){
              pituus match{
                 case 0 => palautetaan += new KahdeksasosaTauko
                 case 1 => if(alkio.contains(".")) palautetaan += new PisteellinenNeljasosaTauko  else palautetaan += new NeljasosaTauko
                 case 2 => for (i<- 1 to 2) palautetaan += new NeljasosaTauko
                 case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko
                 case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko
              }
            } else if(pituus == 1 ){   
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
            if (nuotinNimi.size == 2)  {       // testing !!!!!!!
                palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)     // TODO  ei voi luoda ennen seuraavan alkion tutkimista !!)
                iskujaMennyt += 0.5
            } else  palautetaan +=  new KahdeksasosaNuotti (nuotinNimi, extraetumerkki)  // if ylennys ollut jo tässä tahdissa,  "N" = neutral, 
                                           //  ei palautusmerkkiä  vai pitäisikö tehdä g1, mutta silloin soittoData menee reisille
        //  }  
          }
      }   // iso else: ei-sointu.
      if (iskujaMennyt == tahtilaji) {
        iskujaMennyt = 0.0  
        tahdinAikaisetEtumerkit = Buffer[String]()
      }
    }  // for 
   palautetaan   // tätä tarvitaan sointuja muodostettaessa
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
 
   for(nuottialkio <- nuottiData)
     println(nuottialkio.asInstanceOf[Nuotti].korkeus)
 
  
   var viivasto = new Viivasto(nuottiData, lyricsBuffer, inputTiedostosta.tahtilaji, inputTiedostosta.kappaleenNimi)
   viivasto.piirraNuotit(nuottiData)
 
   if(inputTiedostosta.MIDIPatch.toInt != 0)  // käyttäjä valitsi että ei kuunnella
      new simpleMIDIPlayerAdapter(nuottiData, inputTiedostosta.MIDIPatch.toInt)
   
   val kappale =  new Kappale    //Buffer[Buffer[String]]()
   new TiedostonTallennus(viivasto.kappale)    
    
}