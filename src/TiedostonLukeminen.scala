import scala.io.Source
import scala.collection.mutable.Buffer
import scala.io.StdIn._
import java.io._

class TiedostonLukeminen {

  var inputFromFile = Buffer[String]()       // kaikki input, paitsi tyhjät rivit
  val nuottiDataRiveina = Buffer[String]()  
  var nuottiAlkiot = Array[String]()         // splitattuna yllä oleva data
  var nuottiDatanRivinumerot = Buffer[Int]() // syötetiedoston nuottidatarivit muistiin, ei tyhjiä rivejä
  val lyriikkadata = Buffer[String]()        // biisin sanat

  var MIDIPatch = ""
  var tahtilaji = "4"
  var kappaleenNimi = ""
  var tiedostonNimi = ""
  val inputhakemisto = new File("./input_virheita/")

  
 
  helppiTeksti()
  listaaTiedostot()
 
  do {
    tiedostonNimi = readLine("\n\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
  } while (!onkoListalla(tiedostonNimi))

  val kayttajanValitsemaTiedosto = Source.fromFile("./input_virheita/" + tiedostonNimi)

 
  
 ///// F U N K T I O T: ///////////////////////////////////////////////////////////////////////// 
  
  def listaaTiedostot() = {
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
  } 

  
  def onkoListalla(nimi: String): Boolean = {
    for (tiedosto <- inputhakemisto.listFiles())
      if (tiedosto.isFile && tiedosto.getName.toLowerCase() == nimi.toLowerCase())
        return true
    false
  }

  
  def lueTiedosto() = {      
     try {
       for (rivi <- this.kayttajanValitsemaTiedosto.getLines) {
          inputFromFile += rivi.trim
       }
     } finally {
        this.kayttajanValitsemaTiedosto.close()
     }
     kasitteleTunnisteet(inputFromFile) 
     tarkistaVirheet()
  }   
  
  
  def tarkistaVirheet() = {
     // println("nuottiDataRiveina :" + nuottiDataRiveina + " nuottiDatanRivinumerot: " + nuottiDatanRivinumerot)
 
     // splittaus & virheiden tarkistus:  
     var virheitaNolla = true
     for (i <- 0 until nuottiDataRiveina.size) {
           var splitattuRivi = nuottiDataRiveina(i).split(" ") // mieti tunnisteiden ja sointujen caset myöhemmin
           for (alkio <- splitattuRivi) {
      //       println("rivillä " + i + ": " +  alkio)
             if (alkio == "" ) {} // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin  
             
          // S O I N T U    
             else if (alkio.head == '<'){ 
                 if(alkio.last != '>'){
                      val korjattuVersio = readLine("\n\n syöte '" + alkio +"' on virheellinen:  puuttuu soinnun lopetussymboli '>' tai olet vahingossa laittanut välilyönnin soinnun sisään" + 
                       "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i)+1)  +
                       "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")  
                 }      // TODO Enter ei tee mitä lupaa
                 
                 val sointu =  alkio.tail.substring(0, alkio.size -2).split(",")  
                 for(aani <- sointu) {
                    if (oikeellisuusTesti(aani) == "") {}
                    else {
                       virheitaNolla =  false  
                       val korjattuVersio = readLine("\n\n syöte '" + aani +"' on virheellinen: " + oikeellisuusTesti(aani) + 
                       "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i)+1)  +
                       "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
                    }
                 }
                 nuottiAlkiot = nuottiAlkiot :+ alkio
             }    // end soinnnun käsittely
            
            //  N U O T T I   T A I   T A U K O
             else if (oikeellisuusTesti(alkio) == "") {  // ei virhettä alkiossa, tarpeeksi infoa nuotin tekemiseen
                 if(alkio.size == 3 && (alkio.tail.contains("#")  || alkio.tail.contains("b"))) { 
                     if(!alkio(2).isDigit){  // ei ole muotoa "c#2", mutta hyväksytään c2# ilman varoitusta kirjainten uudelleenjärjestelyn avulla
                       nuottiAlkiot = nuottiAlkiot :+ infoParempaanJarjestykseen(alkio)
                       println(infoParempaanJarjestykseen(alkio))
                     }  
                     else nuottiAlkiot = nuottiAlkiot :+ alkio
                 } 
                 else nuottiAlkiot = nuottiAlkiot :+ alkio
             } else {  // virheellinen alkio:
             virheitaNolla =  false  
             val korjattuVersio = readLine("\n\n syöte '" + alkio +"' on virheellinen: " + oikeellisuusTesti(alkio) + 
               "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i)+1)  +
               "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu input-kansioon. ")
             }
           }
    } // end for nuottiDataRiveina
     
    if (virheitaNolla) soundiValinta()
    //     for (i <- 0 until nuottiAlkiot.size)
    //        println (nuottiAlkiot(i))     
  }

  
  def infoParempaanJarjestykseen(alkio:String)=  "" + alkio(0) + alkio(2) +alkio(1)
 
  
  def kasitteleTunnisteet(inputFromFile: Buffer[String]) = {  // tekstisyöterivejä

    var seuraavatrivitLyriikkaan = false
    for (i <- 0 until inputFromFile.size) {
      if (inputFromFile(i).trim.size != 0){  
         var kelvollinenSyoteRivi = inputFromFile(i).replaceAll("\t", "")
         if (kelvollinenSyoteRivi.head == '#') {    //  T U N N I S T E E T
           if (kelvollinenSyoteRivi.tail.toLowerCase().trim.contains("sanat")){
             seuraavatrivitLyriikkaan = true
             // varaudutaan siihen että joku kirjoittaa sanoja jo samalle riville kuin missä tunniste:
//             if(kelvollinenSyoteRivi.tail.trim.substring(6) != 0)  {
//                      lyriikkadata += kelvollinenSyoteRivi.tail.trim.substring(6)
//             }   
           }  
           
           
           else if (seuraavatrivitLyriikkaan == false) {
              if ("2345678".contains(kelvollinenSyoteRivi(1))){
                tahtilaji = kelvollinenSyoteRivi(1).toString
                   // varaudutaan siihen että joku kirjoittaa nuotteja jo samalle riville kuin missä tahtilaji-tunniste:
                   if(kelvollinenSyoteRivi.tail.trim.substring(1) != 0)  {
                      nuottiDataRiveina += kelvollinenSyoteRivi.tail.trim.substring(1)   // kaatuu jos käyttäjä on laittanut 5/4 --> /4 on  "nuottidataa"
                      nuottiDatanRivinumerot += i
                   }   
              }         
              if (kelvollinenSyoteRivi.tail.toLowerCase().contains("nimi")) {
                kappaleenNimi = kelvollinenSyoteRivi.tail.substring(5, kelvollinenSyoteRivi.tail.size)
               
              }
           }  // end lyriikat false
           
        } else if (seuraavatrivitLyriikkaan){    // L Y R I I K K A 
             lyriikkadata += kelvollinenSyoteRivi
        }
         
        else {    // L O P U T   ELI   N U O T I T      
          nuottiDatanRivinumerot += i
          nuottiDataRiveina += kelvollinenSyoteRivi.toLowerCase() 
        }
      }// if   .size != 0 
    } 
    println("kappaleenNimi: " + kappaleenNimi + "tahtilaji" + tahtilaji)
  }

  
  def helppiTeksti() = {
    val helpFile = Source.fromFile("help.txt")   

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
  
    // ALUKSI TUTKITAAN  N U O T T I E N   S Y N T A K SI ,  EI PITUUDET
         val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
         
         if(filtteredNote == "z") {}  // taukojen syntaksi helppo, tehdään pituustesti myöhemmin
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
                    return "väärä formaatti. Muistathan syntaksin: esim. alennettu e on Eb, ei es"   
            else if(filtteredNote.size > 3)
                return "liian pitkä nuotin nimi, puuttuukohan välilyönti?"  
          } // iso else
     
     //  P I T U U D E T  
         val lkm = syote.count(_ == '-')
         if(lkm > 4)
            return "maksimipituus nuotille on 4, eli viivoja korkeintaan ----"
         else if(lkm == 3 && syote.contains("."))    // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
            return "väärä pituus"
         else if(lkm == 4 && syote.contains("."))   // max pituus 4
            return "pisteellistä kokonuottia ei ole määritelty, tee kokonuotti ja 2 taukoa"
         else if(lkm == 0 && syote.contains("."))    // ei pisteellistä kahdeksasosaa
            return "tämä ohjelma ei osaa käsitellä pisteellistä kahdeksasosaa"
    
        else ""     
          
  } // end oikeellisuusTesti

}