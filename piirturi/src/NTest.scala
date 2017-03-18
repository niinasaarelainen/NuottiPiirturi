import scala.collection.mutable.Buffer
import org.scalatest._

 
class NTest extends FlatSpec with Matchers {
  
  
 // #1 
"TiedostonLukeminen.oikeellisuusTesti()" should "find non-valid note names and length" in {
     
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
        }  // for
        virheita
     }
     
      assert(virheitaHyvaksyttavillaNuoteilla == 0  &&  virheitaHylattavillaNuoteilla == nuottejaVaarin.size && 
          virheitaHyvaksyttavillaTauoilla == 0 &&  virheitaHylattavillaTauoilla == taukojaVaarin.size)  
   } 
  

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

 
"NeljasosaNuotti" should "have right nuppi, nimiMapissa and etumerkki" in {
  
     val neljasOsa = new NeljasosaNuotti("c#2")
     assert(neljasOsa.nuppi == "@@" && neljasOsa.nimiMapissa == "c2" && neljasOsa.etumerkki == "#")
   
}

 
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
    val nuottiData = Buffer("b2", "b2")
    var palautetaan = Buffer[ViivastolleLaitettava]()
    piirturi.kasitteleNuottiTieto(nuottiData, palautetaan)
    val pari = new KahdeksasosaPari(palautetaan(0).asInstanceOf[KahdeksasosaNuotti], palautetaan(1).asInstanceOf[KahdeksasosaNuotti])
     
    println(pari.kuva)
    println(kuva)
    println(kuva.size)
    println(pari.kuva.size)
    
    for (i <-0 until kuva.size) {
        val success = pari.kuva(i).equals(kuva(i))
        assert(success && kuva.size == pari.kuva.size) // jälkimmäinen ehto tarvitaan tarkistamaan ettei pari.kuvan lopussa ole jotain ylimääräistä
    }   
} 


"Pisteellinen NeljasosaNuotti" should "draw dotted quarter note stem up" in {
      
      var kuva = Buffer[String]()	
      
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
     kuva +="-----------" 
     kuva +="    |      "
     kuva +="----|------"
     kuva +="    |      "              
     kuva +="---@@.-----" 
     kuva +="           " 
     kuva +="-----------" 
     kuva +="           " 
     kuva +="-----------" 
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
     kuva +="           " 
            
      
      val n = new PisteellinenNeljasosaNuotti("h1")
       
       for (i <-0 until kuva.size) {
          val success = n.kuva(i).equals(kuva(i))
          assert(success && kuva.size == n.kuva.size)
       }   
}



"NuottiPiirturi" should "draw chords right" in {
  
 //  assert()
}


"TiedostonLukeminen" should "find tahtilaji and kappaleenNimi" in {
  
      val luk = new TiedostonLukeminen()
      luk.lueTiedosto("kalevala")
      
       assert(luk.kappaleenNimi == " Kalevala - kuudestoista runo (ote)" && luk.tahtilaji == "5")
} 

}