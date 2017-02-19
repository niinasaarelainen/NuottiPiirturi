import javax.sound.midi.MidiSystem;
import javax.sound.midi.Synthesizer;
import javax.sound.midi.MidiChannel;

class simpleMIDIPlayer (nuotit: Array[(Int, Int)]) {   // Tuple (korkeus, pituus)
  
  val synth = MidiSystem.getSynthesizer()
  
    val channels  =  synth.getChannels()
		val channel = channels(0)
  
		synth.open();
  
    for(nuotti <- nuotit){
	   	 channel.noteOn(nuotti._1, 80)  // 60 = keski-c, 80= velocity
		   Thread.sleep(nuotti._2 * 400);
       channel.noteOff(nuotti._1);
    }
    synth.close();
}

object testi extends App{
  
   val MIDINoteNumber = Map("c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  "e1" -> 64,  
       "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67, "d2" -> 6,  "g#1" -> 68, "ab1" -> 68)

    
  val nuotit = Array(60, 72, 64)
  var pituudet = Array(0, 1, 2)
  pituudet = pituudet.map { x => x+1 }   // helpompi kertoa millisekunneilla kun kahdeksasosa on 1, ja muut sen moninkertoja
  val nuotitJaPituudet = nuotit.zip(pituudet)
  val player = new simpleMIDIPlayer(nuotitJaPituudet)

}