import javax.sound.midi.MidiSystem
import javax.sound.midi.Synthesizer
import javax.sound.midi.MidiChannel

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.io.StdIn._


class simpleMIDIPlayer (nuotit: Buffer[(Buffer[Int], Double)], MIDIPatch:Int, kappale: Kappale, tahtilaji: Int) {   // Tuple (korkeus/korkeudet, pituus)
  
    val ms = 160     // biisin nopeus:  200= nopea, 500 = normaali,  900= hidas
    val synth = MidiSystem.getSynthesizer()
    var uudestaan = "0"
      
   do{	
    synth.open()  

    val channels  =  synth.getChannels()
		val ch1 = channels(0)
		val ch2 = channels(1)
		
//		for(patch <- synth.getAvailableInstruments)
//	 	  println(patch)
		
		MIDIPatch match {
        case 1 => ch1.programChange(0)    //program #0 = Piano
        case 2 => ch1.programChange(11)   //program #11 = Vibraphone
        case 3 => ch1.programChange(18)   //program #19 = Rock Organ
        case 4 => ch1.programChange(1024, 50)   //program #19 = Syn.Strings3 ,  eri bank:sta
        case 5 => ch1.programChange(24)    // nylon guitar
        case 6 => ch1.programChange(29) ; ch2.programChange(1024, 81)    // myös 30   ERI DEF ?????   TODO
        case 7 => ch1.programChange(10)   // music box
     }
	
  
    var olisiAikaSkrollata = 0
		var riviInd = 0
    skrollaaa(riviInd)     // laitetaan näytölle valmiiksi biisin nimi... 
		riviInd += 1
		skrollaaa(riviInd)    // ... ja eka rivi
		riviInd += 1
		olisiAikaSkrollata += ms     // ja alkuarvo, jotta skrollaus tapahtuu hieman ennen kuin rivi oikeasti vaihtuu
		
		Thread.sleep(900)   // jos ei tätä, eka nuotti tulee liian pitkänä, kun synalla/MIDISysteemillä käynnistymiskankeutta
  
		
	
	  for(nuottiTaiSointu <- nuotit){      
        if (nuottiTaiSointu._1(0) != 0)   //taukojen "korkeus", eli tauoille tehdään vain sleep ja skrollausrutiinit
              for (nuotti <-  nuottiTaiSointu._1)
              if(nuotti !=  nuottiTaiSointu._1.last){
                 ch1.noteOn(nuotti, 75)         // 68 = velocity (127 = max), säestysäänet, jos niitä on
               }   
              else { 
                ch1.noteOn(nuotti, 114)  // oltiin sortattu, eli melodia on vikana (ylin ääni = isoin numero)  
               }
           
        Thread.sleep((nuottiTaiSointu._2 * ms).toInt)  // ms 
        olisiAikaSkrollata += (nuottiTaiSointu._2 * ms).toInt
        if(olisiAikaSkrollata >= ms*tahtilaji*2 ){            // rivillä on 2 tahtia 
           if ( riviInd < kappale.kappale.size){
              skrollaaa(riviInd)
              riviInd += 1
              olisiAikaSkrollata = 0
           }
        }   
        
        if (nuottiTaiSointu._1(0) != 0)
          for (nuotti <- nuottiTaiSointu._1)  {            
             ch1.noteOff(nuotti)
          }   
    }
    
    Thread.sleep(900)   // parempi soundi vikaan ääneen
    synth.close()
    
    uudestaan = readLine("\n\nSoitetaanko uudestaan? ENTER = Kyllä,  0 = Ei ")
	} while (uudestaan != "0")
    
    new TiedostonTallennus(kappale)  
    
    
    def skrollaaa(riviInd: Int)= {
         for (i <- 0 until kappale.kappale(riviInd).size)
           println(kappale.kappale(riviInd)(i))
    }
    
}
 ////// end   simpleMIDIPlayer   ///////////////////////////////////////////////


   
class simpleChordPlayer (sointumerkit: Buffer[(Buffer[Int], Int)]) {   // Tuple ("Cm", pituus)
  
    val synth = MidiSystem.getSynthesizer()
    synth.open()  

    val channels  =  synth.getChannels()
		val ch1 = channels(0)
		ch1.programChange(33)  // basso
		
		val ch2 = channels(1)
		ch2.programChange(89)  // warm pad
	
		
	//	for(patch <- synth.getAvailableInstruments)
	//	  println(patch)
	
		
		Thread.sleep(895)   // jos ei tätä, eka nuotti tulee liian pitkänä, kun synalla/MIDISysteemillä käynnistymiskankeutta
  
       for(sointumerkki <- sointumerkit){
         
        // säestysäänet päälle: 
           if(sointumerkki._1(0) -24 < 36)                // bassoääni ekana    -24 = 2 okt. alaspäin
              ch1.noteOn(sointumerkki._1(0) -12,  50)   
           else ch1.noteOn(sointumerkki._1(0) -24,  50)  
          
           for (soinnunaani <- sointumerkki._1)
              ch2.noteOn(soinnunaani, 60)      // synasointu
                   
        // soi:   
          Thread.sleep(sointumerkki._2 * 500)  // ms       // tässä pitää olla sama ms kuin MIDIPlayerissä !!
        
        // säestysäänet pois päältä:   
          if(sointumerkki._1(0) -24 < 36)
             ch1.noteOff(sointumerkki._1(0) -12,  60)  
          else ch1.noteOff(sointumerkki._1(0) -24,  60)  
          
          for (soinnunaani <- sointumerkki._1)
              ch2.noteOff(soinnunaani)
       }        

    Thread.sleep(900)   // parempi soundi vikaan ääneen
    synth.close()
}
 /////////    end  simpleChordPlayer   /////////////////////////////////////////////////////////////////



class simpleMIDIPlayerAdapter (nuottiData: Buffer[ViivastolleLaitettava], MIDIPatch:Int, kappale: Kappale, tahtilaji:Int) {   
  
   val MIDINoteNumber = Map("cb1" -> 59,  "c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  
       "e1" -> 64, "e#1" -> 65, "fb1"-> 64, "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, 
       "a1" -> 69, "a#1" -> 70, "hb1" -> 70, "b1" -> 70, "bb1" -> 70, "h1" -> 71, "cb2" -> 71, "h#1" -> 72, "c2" -> 72, 
       "c#2" -> 73, "db2" -> 73, "d2" -> 74,  "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "fb2" -> 76, "e#2" -> 77, 
       "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79, "g#2" -> 80, "ab2" -> 80, "a2" -> 81, "a#2" -> 82, 
       "b2" -> 82, "hb2" -> 82, "bb2" -> 82, "h2" -> 83, "h#2" -> 84)

   var nuottiNumberit = Buffer[Buffer[Int]]()    // tänne nuottien korkeudet
   var pituudet = Buffer[Double]()   // [0.5 ... 4.0]
   
   
       for (alkio<- nuottiData) {
         var apubufferInt = Buffer[Int]()   // luodaan aina tyhjä buffer
         
         
           alkio.isInstanceOf[Sointu] match {
           case true  => pituudet += alkio.asInstanceOf[Sointu].pituus         // yhteinen pituus talteen vain kerran
                         for(nuotti <- alkio.asInstanceOf[Sointu].nuotit){
                              apubufferInt += MIDINoteNumber(nuotti.asInstanceOf[Nuotti].korkeus)  // Map("nuotinnimi" --> Int)     
                         }   
                         nuottiNumberit += apubufferInt.sorted  // melodia menee vikaksi
                         
           case false => if (alkio.isInstanceOf[Nuotti]){
                 apubufferInt += MIDINoteNumber(alkio.asInstanceOf[Nuotti].korkeus)
                 nuottiNumberit += apubufferInt
                 pituudet += alkio.asInstanceOf[Nuotti].pituus
                 } 
                 else if (alkio.isInstanceOf[Tauko]){
                      pituudet += alkio.asInstanceOf[Tauko].pituus   
                      apubufferInt += 0  // tauon "korkeus" on 0
                      nuottiNumberit += apubufferInt
                 }
            } 
       } // end for
   
   
  // println(nuottiNumberit)
   
   val nuotitJaPituudet = nuottiNumberit.zip(pituudet)
   
 
   for (i <- 1 to 2) {                        
    val thread = new Thread {
        override def run {  
          i match{        
            case 1 => new simpleMIDIPlayer(nuotitJaPituudet, MIDIPatch, kappale, tahtilaji) 
            case 2 => // new simpleChordPlayerAdapter(Buffer("C", "G", "F", "C", "G", "C",    "C", "G", "F", "C", "G", "C"))           
          }
        }
    }
    thread.start
   }  
    
}  ///////////////////// end   simpleMIDIPlayerAdapter  /////////////////////////////////////////////////////////



class simpleChordPlayerAdapter (sointumerkit: Buffer[String]){
  
  val MIDINoteNumber = Map("c" -> 60, "c#" ->61, "db" -> 61, "d" -> 62, "d#" -> 63, "eb" -> 63,  
       "e" -> 64, "f"-> 65,  "f#"->66,  "gb" -> 66, "g" -> 67,  "g#" -> 68, "ab" -> 68, 
       "a" -> 69, "a#" -> 70, "hb" -> 70, "b" -> 70, "bb" -> 70, "h" -> 71)

  
  val soinnut = Buffer[Buffer[Int]]() 
  var rummut = Buffer[Int]()
  
  for(sointumerkki <- sointumerkit)
    
     if(sointumerkki.toLowerCase().contains("m"))      
        soinnut += molli(MIDINoteNumber(sointumerkki.toLowerCase().head.toString))
     else  soinnut += duuri(MIDINoteNumber(sointumerkki.toLowerCase()))  
  
    
  def duuri(perussavel: Int) = {
    Buffer(perussavel, perussavel+4, perussavel+7)
  }
  
  def molli(perussavel: Int) = {
     Buffer(perussavel, perussavel+3, perussavel+7)
  }
   
     
    var soinnunPituudet = Buffer(8,8,4,4,4,4, 8,8,4,4,4,4)   // 8 iskua = 2 tahtia  
    val soinnutJaPituudet = soinnut.zip(soinnunPituudet)   
    new simpleChordPlayer(soinnutJaPituudet)
   
}