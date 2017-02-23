  import scala.io.Source
  import scala.collection.mutable.Buffer
  import scala.io.StdIn._
  import java.io._

  
class TiedostonLukeminen  {
  
   var inputFromFile = Buffer[String]()     // kaikki input, paitsi tyhjät rivit
   val tunnisteet = Buffer[String]()       // edellisestä tunnisteet
   val nuottidata = Buffer[String]()       // loput, eli nuottidata
   val lyriikkadata = Buffer[String]()      // biisin sanat
   var nuottiAlkiot= Array[String]()         // splitattuna yllä oleva tiedosto
 
   var MIDIPatch = ""
   var tahtilaji = "4"
   var kappaleenNimi = "" 
  
  
  helppiTeksti

  val inputhakemisto = new File("./input")
  for ( tiedosto <- inputhakemisto.listFiles() ) {
     if ( tiedosto.isFile ) {
       println(tiedosto.getName)
     }
  }  
  val nimi = readLine("\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
  val tiedosto = Source.fromFile("input/" + nimi )
  
  do {
    MIDIPatch = readLine("\nMillä soundilla haluat kuulla kappaleen?\n" +
                              "0= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4=syna ")
  } while (!"01234".contains(MIDIPatch))                           
 
     
  def lueJaTarkistaVirheet() = {
     try {   
       for (rivi <- tiedosto.getLines) {
          if(rivi.trim.size != 0 ){
             inputFromFile += rivi.trim
          }
       }
     } finally {
        tiedosto.close()
     }
    
   kasitteleTunnisteet(inputFromFile)   // tämä pitää tehdä ennen splittaamista !!!! esim tunniste  #Let's get together
   println("nuottidata :" + nuottidata)
   println("tunnisteet :" + tunnisteet)
    
    // splittaus & virheiden tarkistus:  
   for (i <- 0 until nuottidata.size) { 
      var  splitattuRivi = nuottidata(i).split(" ")     // mieti tunnisteiden ja sointujen caset myöhemmin
      for (alkio <-   splitattuRivi){
     //    println("rivillä " + i + alkio)
         if(alkio == "" ) {}                            // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin
         else if(oikeellisuusTesti(alkio)){
           nuottiAlkiot = nuottiAlkiot :+ alkio
         } else {
           val nimi = readLine("\nvirhe xxx. Korjaa tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
         }         
      }
    } // end koko syöte
   
     println (nuottiAlkiot.size)
//     for (i <- 0 until nuottiAlkiot.size)
//        println (nuottiAlkiot(i))     
  }
  
   
  def  kasitteleTunnisteet(inputFromFile: Buffer[String]) = {
    
     var seuraavatrivitLyriikkaan = false
     for (i <- 0 until inputFromFile.size ){  
         if (inputFromFile(i).head == '#'){     
           if(inputFromFile(i).tail.toLowerCase().trim == "sanat")   //  T U N N I S T E E T
              seuraavatrivitLyriikkaan = true
           else if(seuraavatrivitLyriikkaan == false){   
              tunnisteet +=  i.toString()                              // parillisiin rivinumero Stringinä, parittomiin tunnisteen nimi
              tunnisteet += inputFromFile(i).tail.toLowerCase().trim
              if("2345678".contains(inputFromFile(i).tail))
                 tahtilaji = inputFromFile(i).tail
        //         if(inputFromFile(i).tail.trim.substring(1,inputFromFile(i).tail.size) != 0)  // samalla rivillä tahtilaji ja nuotteja
          //           nuottidata += inputFromFile(i).tail.trim.substring(1,inputFromFile(i).tail.size)
              if(inputFromFile(i).tail.toLowerCase().contains("nimi")){
                println(inputFromFile(i).size)
                 kappaleenNimi = inputFromFile(i).tail.substring(5, inputFromFile(i).tail.size)  
                 println("kappaleenNimi: " + kappaleenNimi)
              }       
           }  
           
       } else if (seuraavatrivitLyriikkaan)
          lyriikkadata += (inputFromFile(i))      // L Y R I I K A 
          
         else nuottidata += inputFromFile(i)      // L O P U T   ELI   N U O T I T
    }  
    tunnisteet += inputFromFile.size.toString()    // vika rivinumero     
  }
  
  
  def helppiTeksti = {
     val helpFile = Source.fromFile("help.txt")
   
     try {   
       for (rivi <- helpFile.getLines) {
         println(rivi)
       }
     } finally {
        helpFile.close()
     }
   }
  
  
   def oikeellisuusTesti(nuottiJaPituus: String) : Boolean = {
    
     true
   }  // end oikeellisuusTesti

}