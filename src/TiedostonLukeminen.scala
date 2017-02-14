  import scala.io.Source
  import scala.collection.mutable.Buffer
  import scala.io.StdIn._
  import java.io._

  
class TiedostonLukeminen  {
  
  var inputArray= Array[String]() 
    
  val nykyinenHakemisto = new File("./input")
  val listaus = nykyinenHakemisto.listFiles()

  for ( tiedosto <- listaus ) {
    if ( tiedosto.isFile ) {
      println(tiedosto.getName)
    }
  }  
  val nimi = readLine("\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
//  val tiedosto = Source.fromFile("input/puolinuotteja.txt")   
  val tiedosto = Source.fromFile("input/" + nimi )
  var inputFromFile = Buffer[String]()
   
  def lueJaTarkistaVirheet() = {
    try {   
      for (rivi <- tiedosto.getLines) {
         if(rivi.trim.size != 0 )
         inputFromFile += rivi.trim
      }
    } finally {
      tiedosto.close()
    }
  
 
    // splittaus & virheiden tarkistus:  
   
    
   for (i <- 0 until inputFromFile.size) { 
     var  splitattuRivi = inputFromFile(i).split(" ")     // mieti tunnisteiden ja sointujen caset myöhemmin
     for (alkio <-   splitattuRivi){
       println("rivillä " + i + alkio)
     
       if(oikeellisuusTesti(alkio)){
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
  
  def oikeellisuusTesti(nuottiJaPituus: String) : Boolean = {
    
    true
  }  // end oikeellisuusTesti

 
  }