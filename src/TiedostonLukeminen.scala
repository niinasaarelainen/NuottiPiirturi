  import scala.io.Source
  import scala.collection.mutable.Buffer
  import scala.io.StdIn._
  import java.io._

  
class TiedostonLukeminen  {
  
  var inputFromFile = Buffer[String]()     // kaikki input, paitsi tyhjät rivit
  val tunnisteet = Buffer[String]()       // edellisestä tunnisteet
  val nuottidata = Buffer[String]()       // loput, eli nuottidata
  val lyriikkadata = Buffer[String]()      // biisin sanat
  var inputArray= Array[String]()         // splitattuna yllä oleva tiedosto
 
    
  val inputhakemisto = new File("./input")
  val listaus = inputhakemisto.listFiles()
  
  helppiTeksti

  for ( tiedosto <- listaus ) {
    if ( tiedosto.isFile ) {
      println(tiedosto.getName)
    }
  }  
  val nimi = readLine("\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
  val tiedosto = Source.fromFile("input/" + nimi )
   
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
         if(alkio == "" ) {}                            // ylimääräisiä välilyöntejä ei inputArray:hin
         else if(oikeellisuusTesti(alkio)){
           inputArray = inputArray :+ alkio
         } else {
           val nimi = readLine("\nvirhe xxx. Korjaa tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
         }         
   
     }
      
     
   } // end koko syöte
   
     println (inputArray.size)
     for (i <- 0 until inputArray.size)
        println (inputArray(i))     
  }
  
  def  kasitteleTunnisteet(inputFromFile: Buffer[String]) = {
    for (i <- 0 until inputFromFile.size ){  
         if (inputFromFile(i).head == '#'){     // parillisiin rivinumero Stringinä, parittomiin tunnisteen nimi
                      
           tunnisteet +=  i.toString()  
           tunnisteet += inputFromFile(i).tail.toLowerCase().trim
       } 
         else nuottidata += inputFromFile(i)
    }  
    tunnisteet += inputFromFile.size.toString()    // vika rivinumero
    
    for(i <- 0 until tunnisteet.size ){
       if(tunnisteet(i) == "sanat"){
          for (j<- i-1 to i+1)
             lyriikkadata += tunnisteet(j)
       }
    }
    if(lyriikkadata.size != 0)
         println(lyriikkadata(0))
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