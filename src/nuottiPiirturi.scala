import scala.collection.mutable.Buffer



class nuottiPiirturi(input: String, var tahtilaji: Int, lyrics: String){
 
  
  def this(input: String, tahtilaji: Int) = this (input, tahtilaji, "")
  def this(input: String) = this (input, 4, "")
  
  var kappale =  new Kappale    //Buffer[Viivasto]
  var lyricsArray = lyrics.split(" ")   
  var biisiLoppu = false
  var tunnisteet = Buffer[String]()
  var nuottiData= Buffer[ViivastolleLaitettava]() 
 
  val inputBuffer = Buffer[String]()
  var inputArray = Array[String]()  // saadaan splittaamalla, ei tule Buffer
  
  var nuotinNimi = ""
  var sointu = Array[String]()   // saadaan splittaamalla, ei tule Buffer
  
  
   val inputTiedostosta = new TiedostonLukeminen
   inputTiedostosta.lueJaTarkistaVirheet()
 
   kasitteleTunnisteetJaLuoNuottiPuskuri(inputTiedostosta.inputArray)
 
     
 def kasitteleTunnisteetJaLuoNuottiPuskuri(data: Array[String]) = {
     for (i <- 0 until data.size ){  
        if (data(i).head == '#'){     // parillisiin rivinumero Stringinä, parittomiin tunnisteen nimi
           tunnisteet +=  i.toString()  
           tunnisteet += data(i).tail.toLowerCase().trim
        }   
        else inputBuffer += data(i)   // mieti vielä case   #4  ... nuotteja...  samalla rivillä
      }  
      tunnisteet += data.size.toString()    // vika rivinumero
      
      for (rivi <- tunnisteet)
          println("tunniste: " + rivi)
          
             
//    tahtilaji = inputBuffer(1).head - 31
//     println("tahtilaji :" + tahtilaji)
//       
      
//       if(lyricsArray.isDefinedAt(i)){
//           tavu = lyricsArray(i)
//       }           
             
      nuottiData = kasitteleNuottiTieto(inputBuffer, nuottiData)      
  }
  
  
  def kasitteleNuottiTieto(inputBuffer: Buffer[String], palautetaan : Buffer[ViivastolleLaitettava] ): Buffer[ViivastolleLaitettava] = {        
 
  for ( i<- 0 until inputBuffer.length){
        
     
       var solu = inputBuffer(i)    // esim. "g#1--"
       var pituus = solu.count(_ == '-')  
   //    println("solu: " + solu + ", pit:" + pituus)
       if (solu.head == '<'){
          sointu =  solu.tail.substring(0, solu.size -2).split(",")    // kesken
          var sointuBuffer = Buffer[String]()
          var viivastolleLaitettavaBuffer = Buffer[ViivastolleLaitettava]()
          for(aani <- sointu) 
             sointuBuffer += aani
          nuottiData += new Sointu(kasitteleNuottiTieto(sointuBuffer, viivastolleLaitettavaBuffer) ) 
          
    //      nuottiData += new Sointu(Buffer(new PuoliNuotti("d1"), new PuoliNuotti ("f1")))
       }      
       else {
         nuotinNimi = solu.filter(_ != '-')           //tutkiEtumerkit(solu, x)   
       }
        
       if (nuotinNimi == "z"){
   //      kasitteleTauot       // TODO
       }   
       
       else if(pituus == 1){       
          palautetaan += new NeljasosaNuotti(nuotinNimi) 
       
       } else if (pituus == 2){
         palautetaan += new PuoliNuotti (nuotinNimi)  
       }  
       else if (pituus == 3){
          palautetaan += new PisteellinenPuoliNuotti(nuotinNimi) 
         } 
          
       else if (pituus == 4){
          palautetaan += new KokoNuotti(nuotinNimi)  
         
       } else if (pituus == 0 ){         // kahdeksasosa
    //      if( i < inputArray.length -1 && inputArray(i+1).count(_ == '-') == 0 ){
          //   piirraKahdeksasosaPari(nuotinNimi, inputArray(i+1))
      //    }   
        //  else {   
         palautetaan +=  new KahdeksasosaNuotti (nuotinNimi)   
            
        //  }  
       }
       else if (pituus == 0){
       }  
     
  
    }  // for */
  palautetaan   // tätä tarvitaan sointuja muodostetaaessa
} 
  
  println("after kasitteleNuottiTieto: " )
     for(rivi <- nuottiData)     
         println(rivi) 
  
         
  biisiLoppu = true
  var viivasto = new Viivasto(nuottiData)
  viivasto.piirraNuotit(nuottiData)
  val out = new TiedostonTallennus(viivasto.kappale)
    
    
}