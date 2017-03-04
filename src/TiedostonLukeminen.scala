import scala.io.Source
import scala.collection.mutable.Buffer
import scala.io.StdIn._
import java.io._

class TiedostonLukeminen {

  var inputFromFile = Buffer[String]() // kaikki input, paitsi tyhjät rivit
  val nuottiDataRiveina = Buffer[String]() // loput, eli nuottiDataRiveina
  var nuottiDatanRivinumerot = Buffer[Int]()
  val lyriikkadata = Buffer[String]() // biisin sanat
  var nuottiAlkiot = Array[String]() // splitattuna yllä oleva tiedosto

  var MIDIPatch = ""
  var tahtilaji = "4"
  var kappaleenNimi = ""
  var tiedostonNimi = ""

  helppiTeksti()

  val inputhakemisto = new File("./input")
  for (tiedosto <- inputhakemisto.listFiles()) {
    if (tiedosto.isFile) {
      println(tiedosto.getName)
    }
  }

  do {
    tiedostonNimi = readLine("\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
  } while (!onkoListalla(tiedostonNimi))

  val tiedosto = Source.fromFile("input/" + tiedostonNimi)

  do {
    MIDIPatch = readLine("\nMillä soundilla haluat kuulla kappaleen?\n" +
      "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
  } while (!"1234567".contains(MIDIPatch))

  def onkoListalla(nimi: String): Boolean = {
    for (tiedosto <- inputhakemisto.listFiles())
      if (tiedosto.isFile && tiedosto.getName.toLowerCase() == nimi.toLowerCase())
        return true
    false
  }

  def lueJaTarkistaVirheet() = {
    try {
      for (rivi <- tiedosto.getLines) {
        if (rivi.trim.size != 0) {            
          inputFromFile += rivi.trim
        }
      }
    } finally {
      tiedosto.close()
    }

    kasitteleTunnisteet(inputFromFile) // tämä pitää tehdä ennen splittaamista !!!! esim tunniste  #Let's get together
    println("nuottiDataRiveina :" + nuottiDataRiveina)
 
    // splittaus & virheiden tarkistus:  
    for (i <- 0 until nuottiDataRiveina.size) {
      var splitattuRivi = nuottiDataRiveina(i).split(" ") // mieti tunnisteiden ja sointujen caset myöhemmin
      for (alkio <- splitattuRivi) {
        //    println("rivillä " + i + alkio)
        if (alkio == "") {} // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin
        else if (oikeellisuusTesti(alkio)) {
          nuottiAlkiot = nuottiAlkiot :+ alkio
        } else {
          val korjattuVersio = readLine("\nvirhe xxx rivillä : " + (i +nuottiDatanRivinumerot.min +1) + "  Korjaa tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
        }
      }
    } // end koko syöte

    println(nuottiAlkiot.size)
    //     for (i <- 0 until nuottiAlkiot.size)
    //        println (nuottiAlkiot(i))     
  }

  def kasitteleTunnisteet(inputFromFile: Buffer[String]) = {  // tekstisyöterivejä

    var seuraavatrivitLyriikkaan = false
    for (i <- 0 until inputFromFile.size) {
      if (inputFromFile(i).head == '#') {
        if (inputFromFile(i).tail.toLowerCase().trim.contains("sanat")) //  T U N N I S T E E T
          seuraavatrivitLyriikkaan = true
        else if (seuraavatrivitLyriikkaan == false) {
         if ("2345678".contains(inputFromFile(i).tail))
            tahtilaji = inputFromFile(i).tail
          //         if(inputFromFile(i).tail.trim.substring(1,inputFromFile(i).tail.size) != 0)  //  TODO samalla rivillä tahtilaji ja nuotteja
          //           nuottiDataRiveina += inputFromFile(i).tail.trim.substring(1,inputFromFile(i).tail.size)
          if (inputFromFile(i).tail.toLowerCase().contains("nimi")) {
            println(inputFromFile(i).size)
            kappaleenNimi = inputFromFile(i).tail.substring(5, inputFromFile(i).tail.size)
            println("kappaleenNimi: " + kappaleenNimi + "tahtilaji" + tahtilaji)
          }
        }

      } else if (seuraavatrivitLyriikkaan)
        lyriikkadata += (inputFromFile(i)) // L Y R I I K A 

      else {
        nuottiDataRiveina += inputFromFile(i).toLowerCase() // L O P U T   ELI   N U O T I T
        nuottiDatanRivinumerot += i
      }
    }
  }

  def helppiTeksti() = {
    val helpFile = Source.fromFile("help.txt")    // TODO 2 helppiä: basic ja advanced teksit. "Tarvitsetko ohjeita sanoituksiin? "

    try {
      for (rivi <- helpFile.getLines) {
        println(rivi)
      }
    } finally {
      helpFile.close()
    }
  }

  def oikeellisuusTesti(nuottiJaPituus: String): Boolean = {

    false
  } // end oikeellisuusTesti

}