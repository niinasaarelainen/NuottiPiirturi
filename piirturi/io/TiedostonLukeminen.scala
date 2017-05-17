package io

import scala.io.Source
import java.io._
import scala.io.StdIn._
import scala.collection.mutable.Buffer
import io._


class TiedostonLukeminen(inputhakemistonNimi: String) {

  var inputFromFile = Buffer[String]()       // kaikki input, paitsi tyhjät rivit
  var nuottiDataRiveina = Buffer[String]()  
  var nuottiAlkiot = Array[String]()         // oikean syntaksin omaavat nuotit
  var nuottiDatanRivinumerot = Buffer[Int]() // syötetiedoston nuottidatarivit muistiin, ei tyhjiä rivejä
  var lyriikkadata = Buffer[String]()        // biisin sanat

  var tahtilaji = "4"
  var kappaleenNimi = ""
  var tiedostonNimi = ""
  var ekaKerta = true
  var tahtilajiOnJoLuettu = false 
  
 
  
 ///////   M E T O D I T   ///////////////////////////////////////////////////////////////////////// 
   
  def lueTiedosto(tiedostonNimi: String): Unit = {    // olisi järkevää palauttaa luettu tiedosto !! ei unit
    
     val kayttajanValitsemaTiedosto = Source.fromFile(inputhakemistonNimi + tiedostonNimi)
     // pitää nollata, jos tänne tullaan virheidentarkistuksesta tai toisesta kappaleesta:
     this.tiedostonNimi = tiedostonNimi
     this.inputFromFile = Buffer[String]()       
     this.nuottiDataRiveina = Buffer[String]() 
     this.nuottiDatanRivinumerot = Buffer[Int]()
     this.nuottiAlkiot = Array[String]() 
     this.lyriikkadata = Buffer[String]() 
     this.tahtilajiOnJoLuettu = false
     this.ekaKerta = true
     this.tahtilaji = "4"
     this.kappaleenNimi = ""   
       
      
     try {
        for (rivi <- kayttajanValitsemaTiedosto.getLines) {
           this.inputFromFile += rivi.trim
        }
     } finally {
        kayttajanValitsemaTiedosto.close()
     }
          
     if (this.inputFromFile.size != 0){
         kasitteleTunnisteet(this.inputFromFile) 
         if(nuottiDataRiveina.size ==0)         // case: esim pelkkä nimi, mutta ei nuotteja
             return
         else if(ekaKerta) 
           tarkistaVirheet()     // loput virheidentarkistukset do while-loopissa, kutsu rivillä 69
         else return
     }
     else  return      // case: täysin tyhjä file
  }   
  
  
  def tarkistaVirheet():Unit = {
 
     ekaKerta = false
     var virheitaNolla = true
     
     do {
       virheitaNolla = true
       tarkistaVirheetForLoop()
       return
     } while (!virheitaNolla) 
       
       
     def tarkistaVirheetForLoop(): Unit = {
         for (i <- 0 until nuottiDataRiveina.size) {
           
                val ylimaaraisetValilyonnitPois = nuottiDataRiveina(i).trim().replaceAll(" +", " ");
                val splitattuRivi = ylimaaraisetValilyonnitPois.replaceAll(", " , ",").replaceAll(" ," , ",").replaceAll("<>", "").replaceAll("< " , "<").replaceAll(" >" , ">").replaceAll("><", "> <").replaceAll(" -", "-").replaceAll(">-", ">").split(" ") 
                
                for (alkio <- splitattuRivi) {
                  
                   if(virheitaNolla){
                       if (alkio == "" ) {} // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin 
                       
                       else if (alkio.head == '<')
                          tarkistaSoinnunVirheet(alkio.trim(), i)
                         
                       else if (oikeellisuusTesti(alkio) == "")   // ei virhettä alkiossa, tarpeeksi infoa nuotin tekemiseen
                           nuottiAlkiot = nuottiAlkiot :+ erikoistapauksetNuotinNimessa(alkio) // alkio sellaisenaan tai fixattuna
                        
                       else {  // virheellinen alkio:
                           virheitaNolla =  false  
                           readLine("\n\n syöte '" + alkio +"' on virheellinen: " + oikeellisuusTesti(alkio) + 
                             "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i)+1)  +
                             "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")
                             
                           lueTiedosto(this.tiedostonNimi); return
                       }
                  }   
              }         
         } // end for nuottiDataRiveina
     }
     
     //nested method:  
     def tarkistaSoinnunVirheet(alkio:String, ind:Int): Unit = {
              if(alkio.last != '>'){
                    virheitaNolla = false
                    readLine("\n\n syöte '" + alkio +"' on virheellinen: soinnun sävelten väliin tulee kirjoittaa pilkku. "+
                               "\n Tai jos tarkoitit että sointu loppuu, niin muista laittaa >" + 
                     "\n\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind)+1)  +
                     "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")  
                     lueTiedosto(this.tiedostonNimi); return  
              }     
                 
              else {
                 var sointu =  alkio.tail.substring(0, alkio.size -2).split(",")  
                 for(i <- 0 until sointu.size) {
                     if (alkio != ""  && oikeellisuusTesti(sointu(i)) == "") {
                         sointu(i) = erikoistapauksetNuotinNimessa(sointu(i))  // korvataan soinnun alkiot mahdollisilla fixauksilla
                     }
                     else {
                        virheitaNolla =  false  
                        readLine("\n\n syöte '" + sointu(i) +"' on virheellinen: " + oikeellisuusTesti(sointu(i)) + 
                        "\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind)+1)  +
                        "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")
                        lueTiedosto(this.tiedostonNimi); return
                     }
                 }
                   
                 if(virheitaNolla){
                      var korjattuAlkio = "<"
                      for (aani<- sointu) korjattuAlkio += aani + ","
                      korjattuAlkio = korjattuAlkio.substring(0,korjattuAlkio.size-1) + ">" // ei vikaa pilkkua
                      nuottiAlkiot = nuottiAlkiot :+ korjattuAlkio  // alkio = <g1,h1>
                 }
              }  // end else: ei ole kyse koko alkiosta <....>
       }// end tarkistaSoinnunVirheet
  }  // end tarkistaVirheet

  
  
  def erikoistapauksetNuotinNimessa(alkio:String): String =  {
       val alkioPituustietoPois = alkio.filter(_ != '-').filter(_ != '.')
       // case  c2# --> c#2
       if(alkioPituustietoPois.size == 3 && (alkio.tail.contains("#")  || alkio.tail.contains("b")) && !alkioPituustietoPois(2).isDigit) 
            return "" + alkio(0) + alkio(2) +alkio(1) +alkio.substring(3)  
       else if(alkioPituustietoPois == "b#1"  )      // popmuusikot kutsuvat h:ta b:ksi
          return "h#1" 
       else if(alkioPituustietoPois == "b#2" )
          return "h#2"
       alkio             // palautetaan alkio muuttumattomana jos ei tehdä mitään ylläolevista toimenpiteistä
  }
 
  
  def kasitteleTunnisteet(inputFromFile: Buffer[String]) = {  
       var seuraavatrivitLyriikkaan = false
       for (i <- 0 until inputFromFile.size) {
          if (inputFromFile(i).trim.size != 0){  
              var kelvollinenSyoteRivi = inputFromFile(i).replaceAll("\t", " ") // jos korvaa pelkällä "", niin voi lopputulos olla esim "e2e2", jos nuotit oli erotelut toisitaan vain tabulaattorilla
               
              if (kelvollinenSyoteRivi.head == '#') {    //  T U N N I S T E E T
                  if (kelvollinenSyoteRivi.tail.toLowerCase().trim.contains("sanat")){
                     seuraavatrivitLyriikkaan = true
                    // varaudutaan siihen että joku kirjoittaa sanoja jo samalle riville kuin missä tunniste:
                    if(kelvollinenSyoteRivi.tail.trim.substring(5).length > 0) 
                            lyriikkadata += kelvollinenSyoteRivi.tail.trim.substring(5)
                  } else if (seuraavatrivitLyriikkaan == false) kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi, i)  // end lyriikat false
              } else if (seuraavatrivitLyriikkaan)    // L Y R I I K K A 
                   lyriikkadata += kelvollinenSyoteRivi
              else {    // L O P U T   ELI   N U O T I T      
                nuottiDatanRivinumerot += i
                nuottiDataRiveina += kelvollinenSyoteRivi.toLowerCase() 
              }
          }// if   .size != 0 
      }
  }
  
  
  def kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi:String, ind:Int) ={
         if (kelvollinenSyoteRivi.tail.toLowerCase().contains("nimi")) {
              kappaleenNimi = kelvollinenSyoteRivi.tail.substring(5, kelvollinenSyoteRivi.tail.size)
               
         }
         else if (!tahtilajiOnJoLuettu && "2345678".contains(kelvollinenSyoteRivi(1)) && ind < 2){  // jos ei OLE ind < 2, voidaan vahingossa kommentti "#2.teema" tuktkita tahtilajiksi 2/4 
              tahtilaji = kelvollinenSyoteRivi(1).toString      // tahtilaji pilalla jos myöhemmässä kommentissa on numero heti #:n jälkeen TODO
                 // varaudutaan siihen että joku kirjoittaa nuotteja jo samalle riville kuin missä tahtilaji-tunniste:
              if(kelvollinenSyoteRivi.tail.trim.substring(1) != 0)  {
                    nuottiDataRiveina += kelvollinenSyoteRivi.tail.trim.substring(1)   // kaatuu jos käyttäjä on laittanut 5/4 --> /4 on  "nuottidataa"  TODO
                    nuottiDatanRivinumerot += ind
              }   
              tahtilajiOnJoLuettu = true
         }
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
  
  
  def oikeellisuusTesti(syote: String) = {    // esim. g#1---
  
     // ALUKSI TUTKITAAN  N U O T T I E N   S Y N T A K SI ,  _EI_ PITUUDET
         val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
         val lkm = syote.count(_ == '-')
         
         def tutkiPituudet():String = {   
             if(lkm > 4)
               "maksimipituus nuotille on 4, eli viivoja korkeintaan ----"
             else if(lkm == 3 && syote.contains("."))    // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
                 "väärä pituus"
             else if(lkm == 4 && syote.contains("."))   // max pituus 4
                 "pisteellistä kokonuottia ei ole määritelty, tee kokonuotti ja tauko"
             else if(lkm == 0 && syote.contains("."))    // ei pisteellistä kahdeksasosaa
                 "tämä ohjelma ei osaa käsitellä pisteellistä kahdeksasosaa"
             
             else ""  // nuotin syntaksissa eikä pituuksissa ollut virhettä, jos päästiin tänne asti
         }      
        
         if (filtteredNote == "z")  tutkiPituudet()   // alla olevat koskevat vain nuotteja, taukoa emme tutki enempää
         else if(filtteredNote == "")  "pelkkä pituustieto, puuttuu nuotin nimi"
         else if(filtteredNote.count(_ == 'z') > 1)
                 "taukojen pituudet merkitään viivoilla, esim puolitauko z--"  
         else if(!"cdefgahb".contains(filtteredNote.toLowerCase().head.toString()))
                 "nuotin pitää alkaa kirjaimilla c,C, d,D e,E f,F g,G a,A h,H, b tai B"  
         else if(filtteredNote.size == 1 )   
                 "tarkoititko "+ filtteredNote + "1 vai " + filtteredNote + "2?"   
         else if(filtteredNote.size == 2 && (filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")) && !(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
                 "tarkoititko "+ filtteredNote + "1 vai " + filtteredNote + "2?"      
         else if(filtteredNote.tail.contains("#b") ||  filtteredNote.tail.contains("b#"))    
                 "nuotissa on ylennys- ja alennusmerkki"   
         else if( !(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
                 "nuotissa tulee olla oktaaviala: 1 tai 2, muita nuotteja ei osata piirtää"   
         else if(filtteredNote.size == 3 && !(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))   
                 "väärä formaatti. Muistathan syntaksin: esim. alennettu e on Eb, ei es"   
         else if(filtteredNote.size > 3)
                 "liian pitkä nuotin nimi" 
         else   tutkiPituudet()
         
         
          
  } // end oikeellisuusTesti

}