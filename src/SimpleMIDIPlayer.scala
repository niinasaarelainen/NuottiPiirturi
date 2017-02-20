import javax.sound.midi.MidiSystem
import javax.sound.midi.Synthesizer
import javax.sound.midi.MidiChannel

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer


class simpleMIDIPlayer (nuotit: Buffer[(Buffer[Int], Int)]) {   // Tuple (korkeus/korkeudet, pituus)
  
  val synth = MidiSystem.getSynthesizer()
  
    val channels  =  synth.getChannels()
		val channel = channels(0)
  
		synth.open()
		Thread.sleep(300)   // jos ei tätä, eka nuotti tulee liian pitkänä, kun synalla/MIDISysteemillä käynnistymiskankeutta
  
    for(nuottiTaiSointu <- nuotit){
      
        for (soinnunNuotti <- nuottiTaiSointu._1)
           channel.noteOn(soinnunNuotti, 115)         // 115 = velocity (127 = max)
           
        Thread.sleep(nuottiTaiSointu._2 * 300)  // ms   
        
        for (soinnunNuotti <- nuottiTaiSointu._1)              
           channel.noteOff(soinnunNuotti)
    }
    Thread.sleep(300)   // parempi soundi vikaan ääneen
    synth.close()
}

class simpleMIDIPlayerAdapter (nuottiData: Buffer[ViivastolleLaitettava]) {   //  used to be: extends App
  
   val MIDINoteNumber = Map("c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  "e1" -> 64,  
       "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, "a1" -> 69,  
       "a#1" -> 70, "hb1" -> 70, "b1" -> 70, "h1" -> 71,  "c2" -> 72, "c#2" -> 73, "db2" -> 73, "d2" -> 74, 
       "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79, "z" -> 0)

//  var nuottinimet = Buffer(Buffer("c1"), Buffer("eb1"), Buffer("g1"), Buffer("c2"), Buffer("eb2"), Buffer("g2"))  
//   var nuottinimet = Buffer(Buffer("c1"), Buffer("eb1"), Buffer("g2", "f2"), Buffer("c2", "eb2", "g2"), Buffer("b1", "d2", "f2") )  
 //   var nuottinimet = Buffer(Buffer("c1","eb1","g1"), Buffer("c2", "eb2", "g2"), Buffer("c1","e1","g1"), Buffer("c2", "e2", "g2") ) 
   
   var nuottinimet = Buffer[Buffer[String]]() 
   var nuottiNumberit = Buffer[Buffer[Int]]() 
   var apubufferInt = Buffer[Int]()
   var pituudet = Buffer[Double]()
   
   
   println(nuottiData)
   
       for (alkio<- nuottiData) {
         var apuBufferStr = Buffer[String]()
         alkio.isInstanceOf[Sointu] match{
           case true  => kasitteleSoinnut(alkio)
           case false => if (alkio.isInstanceOf[Nuotti]){
               //  apuBufferStr += alkio.asInstanceOf[Nuotti].korkeus 
                 apubufferInt += MIDINoteNumber(alkio.asInstanceOf[Nuotti].korkeus)
                 nuottiNumberit += apubufferInt
                 pituudet += alkio.asInstanceOf[Nuotti].pituus
           }     else if (alkio.isInstanceOf[Tauko]) pituudet += alkio.asInstanceOf[Tauko].pituus   // Double
         }     
       }
   
   def kasitteleSoinnut(alkio: ViivastolleLaitettava) = {
     pituudet += alkio.asInstanceOf[Sointu].pituus            // yhteinen pituus talteen vain kerran
     for(nuotti <- alkio.asInstanceOf[Sointu].nuotit)
        apubufferInt += MIDINoteNumber(alkio.asInstanceOf[Nuotti].korkeus)
        nuottiNumberit += apubufferInt
   }
   
   println(nuottiNumberit)
   
//     var apuBufferStr = Buffer[String]()
//       
//          
//          for (soinnunSavel <- sointu)
//             apuBufferStr += soinnunSavel.filter(_ != '-')
//          nuottinimet += apuBufferStr
//        }
//        else {
//          apuBufferStr += alkio.filter(_ != '-')
//          nuottinimet += apuBufferStr
//        }
           
   
  // println(nuottinimet)
   
   //  noteNumbereiksi(nuottinimet)
   
   
   
//       for (buffer <- nuottiData){
//         pituudet += buffer(0).count(_ == '-')       // kaikilla soinnun sävelillä on sama pituus
//                           // ei siis haluta kaikkien nuottien pituuksia muistiin
//            apubuffer += MIDINoteNumber(nuotti.filter(_ != '-'))
//          
//          nuottiNumberit += apubuffer
//          apubuffer = Buffer[Int]()
//       }     

   
     
//  var nuotit =  nuottinimet.map(MIDINoteNumber(_))    // toimii jos ei sointuja
//  var pituudet = Buffer(0, 0, 1, 1, 2, 4)
 //   var pituudet = Buffer( 1, 1, 1, 1)
    pituudet = pituudet.map { x => if(x== 0) x + 1 else x*2 }   // helpompi kertoa millisekunneilla kun kahdeksasosa on 1, ja muut sen moninkertoja
    val nuotitJaPituudet = nuottiNumberit.zip(pituudet)
    
//    val nuotitJaPituudet2 = nuottiNumberit.zip(soitettavatPituudet)  // parametrina
 //   val player = new simpleMIDIPlayer(nuotitJaPituudet) 
  
}