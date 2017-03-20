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


"TiedostonLukeminen.tarkistaVirheet()" should "find 3 errors in file 3errors" in {
  
}


"The program" should "produce 'Bumble Bee' similarily as in correctly printed file 'bumble.txt'" in {
  // tämä testi on ylläpitoa varten, eli järjestelmätesti: 
  // musiikin maisterin koulutuksella ja silmilläni olen todennut tiedoston bumble.txt oikeaksi.
  // jos ohjelmaa tulevaisuudessa muutetaan, voi tämä testi lakata toimimasta.
  
      val luk = new TiedostonLukeminen()
      luk.lueTiedosto("bumble")
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
           assert(kappaleenKaikkiRivitPerakkain(i) == rivi)
           i += 1
        }
     } finally {
        bumblePrinted.close()
     }
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

}