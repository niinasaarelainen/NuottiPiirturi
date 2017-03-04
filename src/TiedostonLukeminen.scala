import scala.io.Source
import scala.collection.mutable.Buffer
import scala.io.StdIn._
import java.io._

class TiedostonLukeminen {

  var inputFromFile = Buffer[String]() // kaikki input, paitsi tyhjät rivit
  val nuottiDataRiveina = Buffer[String]() // loput, eli nuottiDataRiveina
  var nuottiAlkiot = Array[String]() // splitattuna yllä oleva tiedosto
  var nuottiDatanRivinumerot = Buffer[Int]()
  val lyriikkadata = Buffer[String]() // biisin sanat

  var MIDIPatch = ""
  var tahtilaji = "4"
  var kappaleenNimi = ""
  var tiedostonNimi = ""

  
 
  helppiTeksti()

  val inputhakemisto = new File("./input_virheita")
  var montakoNimeaRiville = 0
  for (tiedosto <- inputhakemisto.listFiles()) {     
     if (tiedosto.isFile) {
        print(tiedosto.getName + "         ")
        montakoNimeaRiville += 1
        if (montakoNimeaRiville == 7){
          println()
          montakoNimeaRiville = 0
        }
     }    
  }

  do {
    tiedostonNimi = readLine("\n\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
  } while (!onkoListalla(tiedostonNimi))

  val tiedosto = Source.fromFile("input_virheita/" + tiedostonNimi)

 

  def onkoListalla(nimi: String): Boolean = {
    for (tiedosto <- inputhakemisto.listFiles())
      if (tiedosto.isFile && tiedosto.getName.toLowerCase() == nimi.toLowerCase())
        return true
    false
  }

  def lueJaTarkistaVirheet() = {       // TODO   <  eli soinnut !?!?!?
     try {
       for (rivi <- tiedosto.getLines) {
          inputFromFile += rivi.trim
       }
     } finally {
        tiedosto.close()
     }

     kasitteleTunnisteet(inputFromFile) // tämä pitää tehdä ennen splittaamista !!!! esim tunniste  #Let's get together
     println("nuottiDataRiveina :" + nuottiDataRiveina + " nuottiDatanRivinumerot: " + nuottiDatanRivinumerot)
 
     // splittaus & virheiden tarkistus:  
     var virheitaNolla = true
     for (i <- 0 until nuottiDataRiveina.size) {
           var splitattuRivi = nuottiDataRiveina(i).split(" ") // mieti tunnisteiden ja sointujen caset myöhemmin
           for (alkio <- splitattuRivi) {
      //       println("rivillä " + i + ": " +  alkio)
             if (alkio == "") {} // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin
             else if (oikeellisuusTesti(alkio) == "") {
       //           if(filtteredNote.size == 3 && (filtteredNote.tail.contains("#")  || filtteredNote.tail.contains("b")))   e2# --> e#2 TODO
        //   else
                  nuottiAlkiot = nuottiAlkiot :+ alkio
             } else {
             virheitaNolla =  false  
             val korjattuVersio = readLine("\n syöte '" + alkio +"' on virheellinen: " + oikeellisuusTesti(alkio) + 
               "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i)+1)  +
               "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
             }
           }
    } // end for nuottiDataRiveina
     
    if (virheitaNolla) soundiValinta()
    println("nuottiAlkiot.size: " + nuottiAlkiot.size)
    //     for (i <- 0 until nuottiAlkiot.size)
    //        println (nuottiAlkiot(i))     
  }

  def kasitteleTunnisteet(inputFromFile: Buffer[String]) = {  // tekstisyöterivejä

    var seuraavatrivitLyriikkaan = false
    for (i <- 0 until inputFromFile.size) {
      if (inputFromFile(i).trim.size != 0){  
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
        } else if (seuraavatrivitLyriikkaan){
             lyriikkadata += (inputFromFile(i)) // L Y R I I K A 
        }
        else {    // L O P U T   ELI   N U O T I T      
          nuottiDatanRivinumerot += i
          nuottiDataRiveina += inputFromFile(i).toLowerCase() 
        }
      }// if   .size != 0 
    } 
  }

  def helppiTeksti() = {
    val helpFile = Source.fromFile("help.txt")    // TODO 2 helppiä: basic ja advanced teksit. "Tarvitsetko ohjeita sanoituksiin? "

    try {
      for (rivi <- helpFile.getLines) {
        println(rivi)
      }
      println()
    } finally {
      helpFile.close()
    }
  }
  
  def soundiValinta() = {
     do {
     MIDIPatch = readLine("\nMillä soundilla haluat kuulla kappaleen?\n" +
      "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
     } while (!"1234567".contains(MIDIPatch))
  }

  def oikeellisuusTesti(syote: String): String = {    // esim. g#1---
  
         var virheita = 0
         val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
         
         if(filtteredNote == "z")
            return ""
         else{
            if(!"cdefgahb".contains(filtteredNote.toLowerCase().head.toString()))
               return "nuotin/tauon pitää alkaa kirjaimilla cdefgahbz"   // väärä teksti jos "zz"
            else if(filtteredNote.size == 1 && !(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
               return "tarkoititko "+ syote + "1 vai " + syote + "2?"   
            else if(filtteredNote.size == 2 && (filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")) && !(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
               return "tarkoititko "+ syote + "1 vai " + syote + "2?"      
            else if(filtteredNote.tail.contains("#b") ||  filtteredNote.tail.contains("b#"))    
                return "nuotissa on ylennys- ja alennusmerkki"   
            else if( !(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
               return "sallitut oktaavialat ovat 1 ja 2"   
            else if(filtteredNote.size == 3 && !(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))   
                    return "nuotissa pitäisi varmaankin olla # tai b"   
            else if(filtteredNote.size > 3)
                return "liian pitkä nuotin nimi, puuttuukohan välilyönti?"        
           
     //       else if(filtteredNote.size < 2)      turha ?
       //        return "oktaaviala puuttuu"
                    
            else ""     
          
          } // iso else
    
  } // end oikeellisuusTesti

}