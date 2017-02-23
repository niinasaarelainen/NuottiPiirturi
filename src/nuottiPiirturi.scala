import scala.collection.mutable.Buffer



class NuottiPiirturi(input: String, var tahtilaji: Int, lyrics: String){
 
  
  def this(input: String, tahtilaji: Int) = this (input, tahtilaji, "")
  def this(input: String) = this (input, 4, "")
  
  var pituus = 0
  var kappale =  new Kappale    //Buffer[Viivasto]
  var lyricsBuffer = Buffer[String]()
  var sanatPotkona = ""
  var lyricsArray = lyrics.split(" ")     // muista: val lyricsNew = lyrics.replaceAll("-", "- ")
  var tunnisteet = Buffer[String]()
  var nuottiData= Buffer[ViivastolleLaitettava]() 
 
  var inputBuffer = Buffer[String]()
 //  var inputArray = Array[String]()  // saadaan splittaamalla, ei tule Buffer. JEEEEE! Tätähän ei tarvita kun tekee .toBuffer !
  
  var nuotinNimi = ""
  var sointu = Array[String]()   // saadaan splittaamalla, ei tule Buffer
  
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
 
   inputBuffer = inputTiedostosta.inputArray.toBuffer
   
   if(inputTiedostosta.lyriikkadata.size != 0){
        
      for (rivi <-  inputTiedostosta.lyriikkadata){
        sanatPotkona += rivi.replaceAll("-", "- ")
        sanatPotkona += " "
      }   
      lyricsBuffer =  sanatPotkona.replaceAll("  ", " ").split(" ").toBuffer  
      for (tavu <- lyricsBuffer)
        println(tavu)
   }
     
             
//    tahtilaji = inputBuffer(1).head - 31
//     println("tahtilaji :" + tahtilaji)
//       
       
             
      nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)      
  
  
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan: Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {        
 
  for ( i<- 0 until inputBuffer.length){        
     
       var solu = inputBuffer(i)    // esim. "g#1--"   
       if(solu.head != '<')
          pituus = solu.count(_ == '-')                  
          
       if (solu.head == '<'){
          sointu =  solu.tail.substring(0, solu.size -2).split(",")    
          var sointuBuffer = Buffer[String]()
          var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()
          for(aani <- sointu) 
             sointuBuffer += aani
          nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
       }      
       
       else {        
         nuotinNimi = solu.filter(_ != '-').filter(_ != '.')        
         if (nuotinNimi == "z"){
            pituus match{
              case 0 => palautetaan += new KahdeksasosaTauko
              case 1 => if(solu.contains(".")) palautetaan += new PisteellinenNeljasosaTauko  else palautetaan += new NeljasosaTauko
              case 2 => for (i<- 1 to 2) palautetaan += new NeljasosaTauko
              case 3 =>  for (i<- 1 to 3) palautetaan += new NeljasosaTauko
              case 4 =>  for (i<- 1 to 4) palautetaan += new NeljasosaTauko
            }
          } else if(pituus == 1 ){   
             if(solu.contains("."))
                palautetaan += new PisteellinenNeljasosaNuotti(nuotinNimi) 
             else palautetaan += new NeljasosaNuotti(nuotinNimi)        
          } else if (pituus == 2 ){
             if(solu.contains("."))
                 palautetaan += new PisteellinenPuoliNuotti(nuotinNimi)  
             else palautetaan += new PuoliNuotti (nuotinNimi)  
          } else if (pituus == 3 ){
               palautetaan += new PisteellinenPuoliNuotti(nuotinNimi) 
          } else if (pituus == 4 ){
               palautetaan += new KokoNuotti(nuotinNimi)           
          } else if (pituus == 0 ){         // kahdeksasosa
    //      if( i < inputArray.length -1 && inputArray(i+1).count(_ == '-') == 0 ){
          //   piirraKahdeksasosaPari(nuotinNimi, inputArray(i+1))
      //    }   
        //  else {   
            palautetaan +=  new KahdeksasosaNuotti (nuotinNimi)     // TODO  ei voi luoda ennen seuraavan solun tutkimista !!            
        //  }  
          }
      }   // iso else: ei-sointu
    }  // for */
  palautetaan   // tätä tarvitaan sointuja muodostetaaessa
} 
 
 
  var viivasto = new Viivasto(nuottiData, lyricsBuffer, inputTiedostosta.tahtilaji, inputTiedostosta.kappaleenNimi)
  viivasto.piirraNuotit(nuottiData)
 
  if(inputTiedostosta.MIDIPatch.toInt != 0)  // käyttäjä valitsi että ei kuunnella
      new simpleMIDIPlayerAdapter(nuottiData, inputTiedostosta.MIDIPatch.toInt)
  //val player = new simpleMIDIPlayerAdapter(soitettavatKorkeudet )  // tällä lailla tiedonkeruussa tuli soinnut myös yksitellen
  
  new TiedostonTallennus(viivasto.kappale) 
  
 
    
    
}