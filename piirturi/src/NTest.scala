import scala.collection.mutable.Buffer
import org.scalatest._
import scala.io.Source
import java.io._

 
class NTest extends FlatSpec with Matchers {
  
  

 // #1  
"TiedostonLukeminen.oikeellisuusTesti()" should "find non-valid note names and lengths" in {
     
     val luk = new TiedostonLukeminen
  
     val nuottejaVaarin = Buffer("t1-", "d3-", "f",  "p", "1c", "2f", "f2f2", "f2f", "f2g----", "hb22----", "c11", "d#b", "db#","b#", 
         "ddd", "d1d",  "#c", "dd1", "g##", "abb", "dis", "c1.", "c1---.", "c1-----", "c1----." )  // neljä vikaa virheelliset pituudet
         
     val nuottejaOikein = Buffer("d#1---", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--",  "f#1---", "d2b", "a----#1",
         "--c1", "----g2", "ab2", "a#2----", "b1", "bb1", "b2", "bb2", "b#2")  
  
     val taukojaVaarin= Buffer("za", "z#--", "zz", "zz-top", "tauko", " z", "az")     
     
     val taukojaOikein = Buffer( "z", "z-", "z-.", "z--",  "z--.", "z---", "z----")  // kaikki sallitut pituudet
         
     var virheitaHylattavillaNuoteilla, virheitaHyvaksyttavillaNuoteilla, virheitaHylattavillaTauoilla, virheitaHyvaksyttavillaTauoilla = 0
     virheitaHylattavillaNuoteilla     = laskeVirheet(nuottejaVaarin)
     virheitaHyvaksyttavillaNuoteilla  = laskeVirheet(nuottejaOikein)
     virheitaHylattavillaTauoilla      = laskeVirheet(taukojaVaarin)
     virheitaHyvaksyttavillaTauoilla   = laskeVirheet(taukojaOikein)
     
     
     def laskeVirheet(syotteet: Buffer[String]) = {   
        var virheita = 0 
        for (syote <- syotteet) {
          if (luk.oikeellisuusTesti(syote) != "")
             virheita += 1
        }  
        virheita
     }
     
      assert(virheitaHyvaksyttavillaNuoteilla == 0  &&  virheitaHylattavillaNuoteilla == nuottejaVaarin.size && 
          virheitaHyvaksyttavillaTauoilla == 0 &&  virheitaHylattavillaTauoilla == taukojaVaarin.size)  
   } 
  

// #2 
"NuottiPiirturi.lyricsBuffer" should "have right lyrics" in {
  
    val sanat = Buffer("Jaak-", "ko", "kul-", "ta,", "Jaak-", "ko", "kul-", "ta,", "he-", "rää", "jo,", "he-", "rää", "jo.", "Kel-", "lo-", "ja-", "si", "soi-", "ta,", "kel-", "lo-", "ja-", "si", "soi-", "ta,", "pium", "paum", "poum,","pium", "paum", "poum.")
    val luk = new TiedostonLukeminen
    
    luk.lueTiedosto("jaakko")
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()
    
    println("piirturi.lyricsBuffer: " + piirturi.lyricsBuffer.size)
    println("sanat.size" + sanat.size)
    
    for (i <-0 until sanat.size) {
        val success = sanat(i).equals(piirturi.lyricsBuffer(i))
        assert(success && sanat.size == piirturi.lyricsBuffer.size) // jälkimmäinen ehto tarvitaan tarkistamaan ettei pari.kuvan lopussa ole jotain ylimääräistä
    }  
  
}


 // #3
"NeljasosaNuotti" should "have right nuppi, nimiMapissa, etumerkki and extraetumerkki" in {
  
     val neljasOsa = new NeljasosaNuotti("c#2")
     assert(neljasOsa.nuppi == "@@" && neljasOsa.nimiMapissa == "c2" && neljasOsa.etumerkki == "#" && neljasOsa.getExtraetumerkki == "")
   
}

  // #4
"KahdeksasosaPari" should "draw eight note couple stems down and ignore second flat" in {
    
    var kuva = Buffer[String]()	
    
   kuva +="            " 
   kuva +="            " 
   kuva +="            " 
   kuva +="  b@@   @@  "    // etumerkkilogiikan mukaan toista alennusta ei saa piirtää
   kuva +=" --|----|-- " 
   kuva +="   |    |   "
   kuva +="---======---"
   kuva +="            "
   kuva +="------------"
   kuva +="            "
   kuva +="------------"
   kuva +="            "
   kuva +="------------"
   kuva +="            "
   kuva +="------------"
   kuva +="            " 
   kuva +="            " 
   kuva +="            " 
   kuva +="            " 
          
    val piirturi = new NuottiPiirturi(new TiedostonLukeminen)
    val nuottiAlkiot = Buffer("b2", "b2")
    var nuottiData = Buffer[ViivastolleLaitettava]()
    nuottiData = piirturi.kasitteleNuottiTieto(nuottiAlkiot, nuottiData)
    val pari = new KahdeksasosaPari(nuottiData(0), nuottiData(1))
     
    println(pari.kuva)     // TODO   ei toimi jos tämän rivin ottaa pois ?!?!?
    
    assertKuva(kuva, pari.kuva)

} 


 // #5
"Pisteellinen NeljasosaNuotti" should "draw dotted quarter note stem up" in {
      
      var odotettu = Buffer[String]()	
      
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="-----------" 
     odotettu +="    |      "
     odotettu +="----|------"
     odotettu +="    |      "              
     odotettu +="---@@.-----" 
     odotettu +="           " 
     odotettu +="-----------" 
     odotettu +="           " 
     odotettu +="-----------"  
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
     odotettu +="           " 
  
     assertKuva(odotettu, new PisteellinenNeljasosaNuotti("h1").kuva)
}



 // #6
"TiedostonLukeminen" should "find tahtilaji and kappaleenNimi" in {
  
      val luk = new TiedostonLukeminen()
      luk.lueTiedosto("kalevala")
      
       assert(luk.kappaleenNimi == " Kalevala - kuudestoista runo (ote)" && luk.tahtilaji == "5")
} 
 

 // #7
"Viivasto.kappale" should "have Title + 2 staffs" in {
  
      val luk = new TiedostonLukeminen()
      luk.lueTiedosto("jaakko_2rivia")    // kappaleessa on 4 kpl 4/4-tahteja ja ohjelma jakaa sen tasan kahdelle riville
      val piirturi = new NuottiPiirturi(luk)
      piirturi.execute()
     
       assert(piirturi.viivasto.kappale.kappale.size == 3 , "***kappale.size not 3")  // kappaleen nimi on eka entry  + 2 riviä musaa
       assert(piirturi.viivasto.kappale.kappale(0).last.contains("Jaakko") == true, "***kappaleen Title puuttuu/väärä")
                                                  // ennen nimeä on tyhjiä rivejä muotoilun vuoksi 
}     
 


// #8
"NuottiData and NuottiDataParitettu" should "have right info in them" in {
  // tutkitaan nuottidatan säilyttäjiä, molemmat tyyppiä Buffer[ViivastolleLaitettava]
  // NuottiDataParitettua ei voi luoda ennen NuottiData:n luontia. Kätevää tutkia molempia samalla.
  
      val luk = new TiedostonLukeminen()
      
      // tiedostossa on pelkkiä kahdeksasosan mittaisia "eventtejä" 2 tahtia = 8*2 = 16 nuottia/taukoa
      luk.lueTiedosto("kahdeksasosia")
      val piirturi = new NuottiPiirturi(luk)
      piirturi.execute()
      
      assert(piirturi.nuottiData.size== 16, "***nuottiDatan pituus ei ollut oikein")
      assert(piirturi.nuottiDataParitettu.size== 10, "***nuottiDataParitettu pituus ei ollut oikein")
      
      
      for(nuottiTaiTauko <- piirturi.nuottiData)
        assert(nuottiTaiTauko.pituus == 0.5, "***nuottiDatassa kaikkien elementtien pituus pitäisi olla 0.5")
        
      // ekassa tahdissa 4 paria(parin pituus 1.0), sitten testataan että tauon jälkeinen kahdeksasosa ei ota seuraavasta nuotista itselleen paria  
      val pariDatanPituudet = Buffer(1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 0.5, 0.5)  
      for(i <- 0 until piirturi.nuottiDataParitettu.size)
        assert(piirturi.nuottiDataParitettu(i).pituus == pariDatanPituudet(i), "***nuottiDataParitettu: pituusvirhe")  
      
      val nuottiDatanKorkeudet = Buffer("c#1", "d#1", "e#1",  "f#1", "g#1", "a#1", "h#1", "c#2", "z", "cb2", "db2", "eb2", "fb2", "gb2", "ab2", "b2" )  
      for(i <- 0 until piirturi.nuottiData.size)
         piirturi.nuottiData(i) match{
           case n: Nuotti => assert(n.asInstanceOf[KahdeksasosaNuotti].korkeus == nuottiDatanKorkeudet(i), "***nuottiData: nuotin korkeusvirhe")
           case t: Tauko =>  assert(t.asInstanceOf[KahdeksasosaTauko].korkeus == "c2", "***nuottiData: tauon korkeusvirhe")  // kaikkien taukojen piirtokorkeus on c2
           case _ => fail  // FAIL, koska syötteessä ei ole sointuja tai muuta kuin em. 2 kategoriaa
         }
      
      val nuottiDatParitettuKorkeudet = Buffer(Array("c#1", "d#1"), Array("e#1",  "f#1"), Array("g#1", "a#1"), Array("h#1", "c#2"), Array("z"), Array("cb2"), Array("db2", "eb2"), Array("fb2", "gb2"), Array("z"), Array("b2") )  
    
      for(i <- 0 until piirturi.nuottiDataParitettu.size)
         piirturi.nuottiDataParitettu(i) match{
           case n: KahdeksasosaNuotti => 
             assert(n.asInstanceOf[KahdeksasosaNuotti].korkeus == nuottiDatParitettuKorkeudet(i)(0), "***nuottiDataParitettu: kahdeksasosan korkeusvirhe")
           case p: KahdeksasosaPari => 
             assert(p.asInstanceOf[KahdeksasosaPari].korkeus == nuottiDatParitettuKorkeudet(i)(0) && p.asInstanceOf[KahdeksasosaPari].korkeus2 == nuottiDatParitettuKorkeudet(i)(1), "***nuottiDataParitettu: parin korkeusvirhe")
           case t: Tauko =>  
             assert(t.asInstanceOf[KahdeksasosaTauko].korkeus == "c2", "***nuottiDataParitettu: tauon korkeusvirhe")  // kaikkien taukojen piirtokorkeus on c2
           case _ => fail  // FAIL, koska syötteessä ei ole muuta kuin em. 3 kategoriaa
         }
 
}    



//#10
"The program" should "produce 'Bumble Bee' similarily as in correctly printed file 'bumble.txt'" in {
  // tämä testi on ylläpitoa varten, eli järjestelmätesti: 
  // musiikin maisterin koulutuksella ja silmilläni olen todennut tiedoston bumble.txt oikeaksi.
  // jos ohjelmaa tulevaisuudessa muutetaan, voi tämä testi lakata toimimasta.
  
      val luk = new TiedostonLukeminen()
      luk.lueTiedosto("bumble")   // (kansiosta input_virheita)
      val nuottipiirturi = new NuottiPiirturi(luk)
      nuottipiirturi.execute()
      
      var kappaleenKaikkiRivitPerakkain = Buffer[String]()
      for {
          viivasto <-  nuottipiirturi.viivasto.kappale.kappale
          rivi <- viivasto
      } kappaleenKaikkiRivitPerakkain += rivi
      
      println("kappaleenKaikkiRivitPerakkain.size: " + kappaleenKaikkiRivitPerakkain.size)
      
      val bumblePrinted = Source.fromFile("./output/bumble.txt")
      var i = 0
      try {
        for (rivi <- bumblePrinted.getLines) {
           assert(kappaleenKaikkiRivitPerakkain(i) == rivi, "ensimmäinen virhe rivillä: " + (i+1))
           i += 1
        }
     } finally {
        bumblePrinted.close()
     }
}

     
     //#9
"TiedostonLukeminen.tarkistaVirheet()" should "find 3 errors in file 3errors" in {
  // simuloidaan käyttäjän virheidenkorjausprosessia. Ensin annetaan syöte, jossa on 3 syntaksivirhettä nuottidatassa.
  // Sitten kuvitellaan tilanne jossa käyttäjä korjaa yhden, annetaan syöte jossa 2 virhettä jne.
  // Ohjelma niemenomaan löytää syötetiedoston ensimmäisen virheen ja odottaa käyttäjän korjaavan sen,
  // ennenkuin syötetiedoston voi tulkita nuoteiksi
  
  // alkuperäisessä syötteessä pelkkiä sointuja, TiedostonLukeminenMock-luokassa muutettu vain sitä
  //   käsittelevää koodia metodissa tarkistaSoinnunVirheet(), joka on metodin tarkistaVirheet()-sisällä
     val syotteet = Buffer("3errors", "2errors", "1error", "0errors")
     
     val lukMock = new TiedostonLukeminenMock(syotteet)
     println(lukMock.mockMessage)
     
    // assert(lukMock.mockMessages(i)  == "löydettiin virhe rivillä 3")
    
}
     
     


//////// A P U M E T O D I T :  ////////////////////////////////////////////////////////////////////

def assertKuva(odotettuKuva: Buffer[String], nuotinKuva: Buffer[String]) = {
  
      assertResult(odotettuKuva.size){   // jos nuotinKuvassa on ylimääräisiä rivejä lopussa
        nuotinKuva.size
      }
  
      for (i <-0 until odotettuKuva.size) {
        assert(odotettuKuva(i).equals(nuotinKuva(i)))
      }     
}

//////  M O C K   C L A S S E S: ///////////////////////////////////////////////////////////////////////


import scala.io.Source
import java.io._
import scala.collection.mutable.Buffer



class TiedostonLukeminenMock (tiedostojenNimet: Buffer[String]) {

  var inputFromFile = Buffer[String]()       // kaikki input, paitsi tyhjät rivit
  var nuottiDataRiveina = Buffer[String]()  
  var nuottiAlkiot = Array[String]()         // splitattuna yllä oleva data
  var nuottiDatanRivinumerot = Buffer[Int]() // syötetiedoston nuottidatarivit muistiin, ei tyhjiä rivejä
  val lyriikkadata = Buffer[String]()        // biisin sanat

  var tahtilaji = "4"
  var kappaleenNimi = ""
  
  val inputhakemistonNimi =  "./input_virheita/"
  val inputhakemisto = new File(inputhakemistonNimi)
  
  var ekaKerta = true
  var tahtilajiOnJoLuettu = false
  
  var mockMessage = "mockMessagen alustusteksti"
  var mockMessages = Buffer[String]()
  var mockMoneskokerta = 0  
  var tiedostonNimi = tiedostojenNimet(0)
 
  
  lueTiedosto(tiedostonNimi)
 
 
  
 ///// F U N K T I O T: ///////////////////////////////////////////////////////////////////////// 
  
  def listaaTiedostot() = {
    var montakoNimeaRiville = 0
    for (tiedosto <- inputhakemisto.listFiles()) {     
       if (tiedosto.isFile) {
          print(tiedosto.getName + '\t')
          montakoNimeaRiville += 1
          if (montakoNimeaRiville == 8){
            println()
            montakoNimeaRiville = 0
          }
       }    
    }
  } 

  
  def onkoListalla(nimi: String): Boolean = {
    for (tiedosto <- inputhakemisto.listFiles())
      if (tiedosto.isFile && tiedosto.getName.toLowerCase() == nimi.toLowerCase().trim())
        return true
    false
  }

  
  def lueTiedosto(tiedostonNimi: String): Unit = {  
    
     this.tiedostonNimi = tiedostonNimi
     this.inputFromFile = Buffer[String]()       // pitää nollata, jos tänne tullaan virheidentarkistuksesta
     this.nuottiDataRiveina = Buffer[String]() 
     this.nuottiDatanRivinumerot = Buffer[Int]()
     this.nuottiAlkiot = Array[String]() 
     val kayttajanValitsemaTiedosto = Source.fromFile(inputhakemistonNimi + tiedostonNimi)
     tahtilajiOnJoLuettu = false
      
     try {
        for (rivi <- kayttajanValitsemaTiedosto.getLines) {
           this.inputFromFile += rivi.trim
        }
     } finally {
        kayttajanValitsemaTiedosto.close()
     }
          
     if (this.inputFromFile.size != 0){
         kasitteleTunnisteet(this.inputFromFile) 
         if(nuottiDataRiveina.size ==0) {println("\n\nei nuottidataa, ei tehdä mitään."); System.exit(1)}
         else if(ekaKerta) tarkistaVirheet()     // loput virheidentarkistukset do while-loopissa, kutsu rivillä 94
     }
     else {
       println("\n\ntyhjästä tiedostosta ei voi tehdä nuotteja")
       System.exit(1)
     }
  }   
  
  
  def tarkistaVirheet() = {
 
     ekaKerta = false
     var virheitaNolla = true
     
     do {
       virheitaNolla = true
       tarkistaVirheetForLoop()
     } while (!virheitaNolla) 
       
       
     def tarkistaVirheetForLoop(): Unit = {
         for (i <- 0 until nuottiDataRiveina.size) {
           
                val ylimaaraisetValilyonnitPois = nuottiDataRiveina(i).trim().replaceAll(" +", " ");
                val splitattuRivi = ylimaaraisetValilyonnitPois.replaceAll(", " , ",").replaceAll(" ," , ",").replaceAll("< " , "<").replaceAll(" >" , ">").replaceAll("<>", "").replaceAll("><", "> <").replaceAll(" -", "-").split(" ") 
                
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
     
        //nested function:  
     def tarkistaSoinnunVirheet(alkio:String, ind:Int): Unit = {
        
            //  println(alkio) 
         
              if(alkio.last != '>'){
                    virheitaNolla = false
                    readLine("\n\n syöte '" + alkio +"' on virheellinen: soinnun sävelten väliin tulee kirjoittaa pilkku. "+
                               "\n Tai jos tarkoitit että sointu loppuu, niin muista laittaa >" + 
                     "\n\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind)+1)  +
                     "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")  
                     lueTiedosto(this.tiedostonNimi); return  
              }     
                 
              else {
                
                println("tarkistaSoinnunVirheet - else")
                
                 var sointu =  alkio.tail.substring(0, alkio.size -2).split(",")  
                 for(i <- 0 until sointu.size) {
                     if (alkio != ""  && oikeellisuusTesti(sointu(i)) == "") {
                         sointu(i) = erikoistapauksetNuotinNimessa(sointu(i))  // korvataan soinnun alkiot mahdollisilla fixauksilla
                     }
                     else {
                        virheitaNolla =  false  
//                        readLine("\n\n syöte '" + sointu(i) +"' on virheellinen: " + oikeellisuusTesti(sointu(i)) + 
//                        "\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind)+1)  +
//                        "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")
   
  // Mock START                  
                        this.mockMessage = " löydettiin virhe rivillä " + (nuottiDatanRivinumerot(ind)+1) 
                        mockMessages += this.mockMessage
                     //   System.exit(1)
                        mockMoneskokerta += 1
                        lueTiedosto(tiedostojenNimet(mockMoneskokerta)); return
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

  
  
  def erikoistapauksetNuotinNimessa(alkio:String): String=  {
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
 
  
  def kasitteleTunnisteet(inputFromFile: Buffer[String]) = {  // tekstisyöterivejä

      var seuraavatrivitLyriikkaan = false
      for (i <- 0 until inputFromFile.size) {
        if (inputFromFile(i).trim.size != 0){  
           var kelvollinenSyoteRivi = inputFromFile(i).replaceAll("\t", "")
           
           if (kelvollinenSyoteRivi.head == '#') {    //  T U N N I S T E E T
              if (kelvollinenSyoteRivi.tail.toLowerCase().trim.contains("sanat")){
                 seuraavatrivitLyriikkaan = true
                // varaudutaan siihen että joku kirjoittaa sanoja jo samalle riville kuin missä tunniste:
                if(kelvollinenSyoteRivi.tail.trim.substring(5).length > 0)  {
                        lyriikkadata += kelvollinenSyoteRivi.tail.trim.substring(5)
               }   
              } 
              else if (seuraavatrivitLyriikkaan == false) kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi, i)  // end lyriikat false
             
          } else if (seuraavatrivitLyriikkaan){    // L Y R I I K K A 
               lyriikkadata += kelvollinenSyoteRivi
          }
           
          else {    // L O P U T   ELI   N U O T I T      
            nuottiDatanRivinumerot += i
            nuottiDataRiveina += kelvollinenSyoteRivi.toLowerCase() 
          }
        }// if   .size != 0 
      } 
    //  println("kappaleenNimi: " + kappaleenNimi + ", tahtilaji" + tahtilaji)
  }
  
  
  def kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi:String, ind:Int) ={
    
           if (kelvollinenSyoteRivi.tail.toLowerCase().contains("nimi")) {
                kappaleenNimi = kelvollinenSyoteRivi.tail.substring(5, kelvollinenSyoteRivi.tail.size)
               
           }
           else if (!tahtilajiOnJoLuettu && "2345678".contains(kelvollinenSyoteRivi(1))){  
                tahtilaji = kelvollinenSyoteRivi(1).toString      
                   // varaudutaan siihen että joku kirjoittaa nuotteja jo samalle riville kuin missä tahtilaji-tunniste:
                if(kelvollinenSyoteRivi.tail.trim.substring(1) != 0)  {
                      nuottiDataRiveina += kelvollinenSyoteRivi.tail.trim.substring(1)   
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
  
  
  def oikeellisuusTesti(syote: String): String = {    // esim. g#1---
  
    // ALUKSI TUTKITAAN  N U O T T I E N   S Y N T A K SI ,  _EI_ PITUUDET
         val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
         
         if(filtteredNote == "z") {}  // taukojen syntaksi helppo, tehdään pituustesti myöhemmin
         else{
              if(filtteredNote.count(_ == 'z') > 1)
                 return "taukojen pituudet merkitään viivoilla, esim puolitauko z--"
              if(!"cdefgahb".contains(filtteredNote.toLowerCase().head.toString()))
                 return "nuotin pitää alkaa kirjaimilla c,C, d,D e,E f,F g,G a,A h,H, b tai B"   // väärä teksti jos "zz"
              else if(filtteredNote.size == 1 )   
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
                  return "liian pitkä nuotin nimi" 
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
}