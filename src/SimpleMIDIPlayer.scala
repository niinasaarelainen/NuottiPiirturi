import javax.sound.midi.MidiSystem;
import javax.sound.midi.Synthesizer;
import javax.sound.midi.MidiChannel;

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer


class simpleMIDIPlayer (nuotit: Buffer[(Int, Int)]) {   // Tuple (korkeus, pituus)
  
  val synth = MidiSystem.getSynthesizer()
  
    val channels  =  synth.getChannels()
		val channel = channels(0)
  
		synth.open();
  
    for(nuotti <- nuotit){
	   	 channel.noteOn(nuotti._1, 100)  // 60 = keski-c, 80= velocity
		   Thread.sleep(nuotti._2 * 300);  // ms
       channel.noteOff(nuotti._1);
    }
    synth.close();
}

object testi extends App{
  
   val MIDINoteNumber = Map("c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  "e1" -> 64,  
       "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, "a1" -> 69,  
       "a#1" -> 70, "hb1" -> 70, "b" -> 70, "h1" -> 71,  "c2" -> 72, "c#2" -> 73, "db2" -> 73, "d2" -> 74, 
       "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79)

  var nuottinimet = Buffer("c1", "eb1", "g1", "c2", "eb2", "g2")  
  var nuotit =  nuottinimet.map(MIDINoteNumber(_))
 // val nuotit = Array(60, 72, 64)
  var pituudet = Buffer(0, 1, 2, 2, 4, 4)
  pituudet = pituudet.map { x => x+1 }   // helpompi kertoa millisekunneilla kun kahdeksasosa on 1, ja muut sen moninkertoja
  val nuotitJaPituudet = nuotit.zip(pituudet)
  val player = new simpleMIDIPlayer(nuotitJaPituudet)

}