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
             
      kasitteleNuottiTieto(inputBuffer)       
  }
  
  
  def kasitteleNuottiTieto(inputBuffer: Buffer[String]) = {   ////////////////////////////////////////////////////////////     
      
  for ( i<- 0 until inputBuffer.length){
      var tavu = ""     
      
       var solu = inputBuffer(i)    // esim. "g1#--"
       var pituus = solu.count(_ == '-')  
       println("solu: " + solu + ", pit:" + pituus)
       if (solu.head == '<'){
          sointu = solu.split(",")    // kesken
          nuottiData += new Sointu(Buffer(new PuoliNuotti("d1"), new PuoliNuotti ("f1")))
       }      
       else nuotinNimi = solu.substring(0, 2)            //tutkiEtumerkit(solu, x)    
       
       if (nuotinNimi == "z"){
   //      kasitteleTauot       // TODO
       }   
       
       else if(pituus == 1){       
   //       nuottiData += new NeljasosaNuotti(nuotinNimi) 
       
       } else if (pituus == 2){
         nuottiData += new PuoliNuotti (nuotinNimi)  
       }  
       else if (pituus == 3){
          nuottiData += new PisteellinenPuoliNuotti(nuotinNimi) 
         } 
          
       else if (pituus == 4){
          nuottiData += new KokoNuotti(nuotinNimi)  
         
       } else if (pituus == 0 ){         // kahdeksasosa
          if( i < inputArray.length -1 && inputArray(i+1).count(_ == '-') == 0 ){
          //   piirraKahdeksasosaPari(nuotinNimi, inputArray(i+1))
          }   
          else {   
  //        nuottiData +=  new KahdeksasosaNuotti (nuotinNimi)   
            
          }  
       }
       else if (pituus == 0){
       }  
      
   //   for(rivi <- nuottiData(i).kuva)     //  PRINT!
    //        println(rivi) 
  
    }  // for */
  
} 
  
  
  biisiLoppu = true
  var viivasto = new Viivasto(nuottiData)
  viivasto.piirraNuotit(nuottiData)
  val out = new TiedostonTallennus(kappale)
    
    
}