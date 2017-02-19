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
  
    for(nuottiTaiSointu <- nuotit){
      
        for (soinnunNuotti <- nuottiTaiSointu._1)
           channel.noteOn(soinnunNuotti, 110)         // 110 = velocity (128 = max)
        Thread.sleep(nuottiTaiSointu._2 * 300)  // ms   
        for (soinnunNuotti <- nuottiTaiSointu._1)              
           channel.noteOff(soinnunNuotti)
    }
    synth.close()
}

object testi extends App{
  
   val MIDINoteNumber = Map("c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  "e1" -> 64,  
       "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, "a1" -> 69,  
       "a#1" -> 70, "hb1" -> 70, "b1" -> 70, "h1" -> 71,  "c2" -> 72, "c#2" -> 73, "db2" -> 73, "d2" -> 74, 
       "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79)

//  var nuottinimet = Buffer(Buffer("c1"), Buffer("eb1"), Buffer("g1"), Buffer("c2"), Buffer("eb2"), Buffer("g2"))  
   var nuottinimet = Buffer(Buffer("c1"), Buffer("eb1"), Buffer("g2", "f2"), Buffer("c2", "eb2", "g2"), Buffer("b1", "d2", "f2") )  
   var nuottiNumberit = Buffer[Buffer[Int]]() 
   var apubuffer = Buffer[Int]()
   
   noteNumbereiksi(nuottinimet)
   
   def noteNumbereiksi (nuotteja : Buffer[Buffer[String]])= {
     for (buffer <- nuottinimet){
        for (nuotti <- buffer){
          apubuffer += MIDINoteNumber(nuotti)
        }  
        nuottiNumberit += apubuffer
        apubuffer = Buffer[Int]()
     }
     
   }
   
   println(nuottiNumberit(2))
     
//  var nuotit =  nuottinimet.map(MIDINoteNumber(_))    // toimii jos ei sointuja
  var pituudet = Buffer(1, 1, 1, 1, 1, 1)
  pituudet = pituudet.map { x => x+1 }   // helpompi kertoa millisekunneilla kun kahdeksasosa on 1, ja muut sen moninkertoja
  val nuotitJaPituudet = nuottiNumberit.zip(pituudet)
  val player = new simpleMIDIPlayer(nuotitJaPituudet)

}