

 import scala.collection.mutable.Buffer
 import org.scalatest._
 /*
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Assertions._  */
 
 

// @RunWith(classOf[JUnitRunner])
class NTest extends FlatSpec with Matchers {
  
  
 // #1 
"NuottiPiirturi" should "find non-valid note names (not investigating length)" in {
     
     val nuottejaOikein = Buffer("d1#.----", "e1----.#", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--",  "f#1---", "d2b", "a-----1#",
         "--c1", "----g2", "ab2", "a#2----", "b1", "bb1", "b#1", "b2", "bb2", "b#2")  // 
  
     val nuottejaVaarin = Buffer("t1-", "d3-", "f",  "p", "1c", "2f", "f2f2", "f2f", "f2g----", "hb22----", "c11", "d#b", "b#", 
         "ddd", "d1d",  "#c", "dd1", "g##", "abb", "dis" ) 
     
     val taukojaOikein= Buffer("z", "z--", "z----", "z.", "-z", "z--.", "--.z", "z.-")   
     
     val taukojaVaarin= Buffer("za", "z#--", "zz", "zz-top", "tauko", " z", "az")     
    
   
     var virheitaHylattavillaNuoteilla, virheitaHyvaksyttavillaNuoteilla, virheitaHylattavillaTauoilla, virheitaHyvaksyttavillaTauoilla = 0
    
     virheitaHylattavillaNuoteilla     = laskeVirheet(nuottejaVaarin)
     virheitaHyvaksyttavillaNuoteilla  = laskeVirheet(nuottejaOikein)
     virheitaHylattavillaTauoilla      = laskeVirheet(taukojaVaarin)
     virheitaHyvaksyttavillaTauoilla   = laskeVirheet(taukojaOikein)
     
     
     def laskeVirheet(syotteet: Buffer[String]) = {   
        var virheita = 0 
        for (syote <- syotteet) {
         
           val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
           
           if(filtteredNote == "z")
              {}
           else{
              if(!"cdefgahb".contains(filtteredNote.toLowerCase().head.toString()))
                 virheita += 1
              else if(!(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
                 virheita += 1  
              else if(filtteredNote.size < 2)
                 virheita += 1
              else if(filtteredNote.size > 3)
                 virheita += 1  
              else if(filtteredNote.tail.contains("#b") ||  filtteredNote.tail.contains("b#"))    
                 virheita += 1 
              else if(filtteredNote.size == 3 && !(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))   
                       virheita += 1  
                 
            } // iso else
        }  // for
        
        virheita
}     
      assert(virheitaHyvaksyttavillaNuoteilla == 0  &&  virheitaHylattavillaNuoteilla == nuottejaVaarin.size && 
          virheitaHyvaksyttavillaTauoilla == 0 &&  virheitaHylattavillaTauoilla == taukojaVaarin.size)  
   } 
  

// #2   
"NuottiPiirturi" should "find non-valid lengths" in {
     val pituuksiaVaarin = Buffer("d1#.----", "f2-----", "g1.", "h1---------", "a1---."  )
     val pituuksiaOikein = Buffer( "c1", "c1-", "c1-.", "e1--#",  "H#2--.", "Gb1---", "Ab1----")  // kaikki sallitut pituudet
    
     var virheitaHylattavillaPituuksilla, virheitaHyvaksyttavillaPituuksilla = 0
     
     virheitaHylattavillaPituuksilla = laskeVirheet( pituuksiaVaarin)
     virheitaHyvaksyttavillaPituuksilla =  laskeVirheet( pituuksiaOikein)
    
     
     def laskeVirheet(syotteet: Buffer[String] ) = {
       var virheita = 0 
       for (syote <- syotteet) {
          val lkm = syote.count(_ == '-')
          println(lkm)
          if(lkm > 4)
              virheita += 1
          else if(lkm == 3 && syote.contains("."))    // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
            virheita += 1   
          else if(lkm == 4 && syote.contains("."))   // max pituus 4
            virheita += 1  
          else if(lkm == 0 && syote.contains("."))    // ei pisteellistä kahdeksasosaa
            virheita += 1     
       }     
      
       virheita
   }  
    
      assert(virheitaHyvaksyttavillaPituuksilla == 0  &&  virheitaHylattavillaPituuksilla == pituuksiaVaarin.size )   
   } 

// #3   
"NeljasosaNuotti" should "have right nuppi, nimiMapissa and etumerkki" in {
  
   val neljasOsa = new NeljasosaNuotti("c#2")
   neljasOsa.nuppi
   assert(neljasOsa.nuppi == "@@" && neljasOsa.nimiMapissa == "c2" && neljasOsa.etumerkki == "#")
   
}

// #   
"NuottiPiirturi" should "draw eight note couple stems up" in {
  
 //  assert()
}

// #  
"NuottiPiirturi" should "draw chords right" in {
  
 //  assert()
}

// #

"TiedostonLukeminen" should "find tahtilaji and kappaleen nimi" in {
  
  val luk = new TiedostonLukeminen()
  luk.lueTiedosto("kalevala")
  
   assert(luk.kappaleenNimi == " Kalevala - kuudestoista runo (ote)" && luk.tahtilaji == "5")
} 

}