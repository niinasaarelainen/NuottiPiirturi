import javax.sound.midi.MidiSystem
import javax.sound.midi.Synthesizer
import javax.sound.midi.MidiChannel

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer


class simpleMIDIPlayer (nuotit: Buffer[(Buffer[Int], Double)], MIDIPatch:Int) {   // Tuple (korkeus/korkeudet, pituus)
  
  val synth = MidiSystem.getSynthesizer()
  synth.open()  

    val channels  =  synth.getChannels()
		var ch1 = channels(0)
	//	val ch10 = synth.getChannels()(9)       rummut
		
	//	for(patch <- synth.getAvailableInstruments)
	//	  println(patch)
		
		 MIDIPatch match {
        case 1 => ch1.programChange(0)    //program #0 = Piano
        case 2 => ch1.programChange(11)   //program #11 = Vibraphone
        case 3 => ch1.programChange(18)   //program #19 = Rock Organ
        case 4 => ch1.programChange(1024, 50)   //program #19 = Syn.Strings3 ,  eri bank:sta
    }
		
		Thread.sleep(300)   // jos ei tätä, eka nuotti tulee liian pitkänä, kun synalla/MIDISysteemillä käynnistymiskankeutta
  
    for(nuottiTaiSointu <- nuotit){
      
        if (nuottiTaiSointu._1(0) != 0)  //taukojen "korkeus"
          for (i <- 0 until nuottiTaiSointu._1.size)
            if(i <  nuottiTaiSointu._1.size-1)
              ch1.noteOn(nuottiTaiSointu._1(i), 65)         // 60 = velocity (127 = max), säestysäänet, jos niitä on
            else  ch1.noteOn(nuottiTaiSointu._1(i), 115)  // oltiin sortattu, eli melodia on vikana (ylin ääni = isoin numero)
           
        Thread.sleep((nuottiTaiSointu._2 * 500).toInt)  // ms   
        
        if (nuottiTaiSointu._1(0) != 0)
          for (soinnunNuotti <- nuottiTaiSointu._1)              
             ch1.noteOff(soinnunNuotti)
    }
    Thread.sleep(300)   // parempi soundi vikaan ääneen
    synth.close()
}

class simpleMIDIPlayerAdapter (nuottiData: Buffer[ViivastolleLaitettava], MIDIPatch:Int) {   //  used to be: extends App
  
   val MIDINoteNumber = Map("cb1" -> 59, "c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  "e1" -> 64,  
       "e#1" -> 65, "fb1"-> 64, "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, "a1" -> 69,  
       "a#1" -> 70, "hb1" -> 70, "b1" -> 70, "h1" -> 71, "cb2" -> 71, "c2" -> 72, "c#2" -> 73, "db2" -> 73, "d2" -> 74, 
       "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "e#2" -> 77, "fb2" -> 76, "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79, "g#2" -> 80)

   var nuottiNumberit = Buffer[Buffer[Int]]() 
   var apubufferInt = Buffer[Int]()
   var pituudet = Buffer[Double]()   // [0.5 ... 4.0]
   
   
  // println(nuottiData)
   
       for (alkio<- nuottiData) {
         var apubufferInt = Buffer[Int]()   // luodaan aina tyhjä buffer
         
           alkio.isInstanceOf[Sointu] match {
           case true  => pituudet += alkio.asInstanceOf[Sointu].pituus            // yhteinen pituus talteen vain kerran
                         for(nuotti <- alkio.asInstanceOf[Sointu].nuotit){
                              apubufferInt += MIDINoteNumber(nuotti.asInstanceOf[Nuotti].korkeus)  // Map("nuotinnimi" --> Int)     
                         }   
                         nuottiNumberit += apubufferInt.sorted  // melodia menee vikaksi
           case false => if (alkio.isInstanceOf[Nuotti]){
                 apubufferInt += MIDINoteNumber(alkio.asInstanceOf[Nuotti].korkeus)
                 nuottiNumberit += apubufferInt
                 pituudet += alkio.asInstanceOf[Nuotti].pituus
           }     else if (alkio.isInstanceOf[Tauko]){
                      pituudet += alkio.asInstanceOf[Tauko].pituus   // Double
                      apubufferInt += 0  // tauon "korkeus" on 0
                      nuottiNumberit += apubufferInt
           }
         } 
       }
   
   
  // println(nuottiNumberit)
   
    val nuotitJaPituudet = nuottiNumberit.zip(pituudet)
    val player = new simpleMIDIPlayer(nuotitJaPituudet, MIDIPatch) 
  
}