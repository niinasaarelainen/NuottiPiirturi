import javax.sound.midi.MidiSystem
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.io.StdIn._


class simpleMIDIPlayer (nuotit: Buffer[(Buffer[Int], Double)], MIDIPatch:Int, kappale: Kappale, tahtilaji: Int) {   // Tuple (korkeus/korkeudet, pituus)
  
    val ms = 440     // biisin nopeus millisekunneissa:  200= nopea,  500 = normaali,  900= hidas
    val synth = MidiSystem.getSynthesizer()
    val channels  =  synth.getChannels()
		val ch1 = channels(0); val ch2 = channels(1);  val ch3 = channels(2);	val ch4 = channels(3);  val ch5 = channels(4);  val ch6 = channels(5)
      
		var uudestaan = "0"
    var olisiAikaSkrollata = 0
		var riviInd = 0
				
//		for(patch <- synth.getAvailableInstruments)
//	 	  println(patch)
		
    def soita() = {
    		do{	
        synth.open()  
    		
    		MIDIPatch match {
            case 1 => ch1.programChange(0)    //program #0 = Piano
            case 2 => ch1.programChange(11)   //program #11 = Vibraphone
            case 3 => ch1.programChange(18)   //program #19 = Rock Organ
            case 4 => ch1.programChange(1024, 50)   //program #19 = Syn.Strings3 ,  eri bank:sta
            case 5 => ch1.programChange(24)    // nylon guitar
            case 6 => ch1.programChange(30); ch2.programChange(1024, 81); ch3.programChange(35); ch4.programChange(29); ch5.programChange(1024, 81); ch6.programChange(39); 
            case 7 => ch1.programChange(10)   // music box
         }
    	
        olisiAikaSkrollata = 0
        riviInd = 0
        println(); println()
        skrollaaa(riviInd)     // laitetaan näytölle valmiiksi biisin nimi... 
    		riviInd += 1
    		skrollaaa(riviInd)    // ... ja eka rivi
    		riviInd += 1
    		olisiAikaSkrollata += ms     // ja alkuarvo, jotta skrollaus tapahtuu hieman ennen kuin rivi oikeasti vaihtuu
    		
    		Thread.sleep(ms)   // jos ei tätä, eka nuotti tulee liian pitkänä, kun synalla/MIDISysteemillä käynnistymiskankeutta
      
    		
    	  if(MIDIPatch != 6)  //normaali soitto, yksikanavainen
    	     normaalisoitto()
    	  else rocknroll()   // rokkibändi, 6-kanavainen, delay:      
        
        Thread.sleep(1900)   // parempi soundi vikaan ääneen
        synth.close()
        
        uudestaan = readLine("\n\nSoitetaanko uudestaan? ENTER = Kyllä,  0 = Ei ")
    	  } while (uudestaan != "0")
		}  
 
    
    def skrollaaa(riviInd: Int)= {
         for (i <- 0 until kappale.kappale(riviInd).size)
           println(kappale.kappale(riviInd)(i))
    }
	
	  def normaalisoitto() = {
	     for(nuottiTaiSointu <- nuotit){      
        
	     // noteOn:
	       if (nuottiTaiSointu._1(0) != 0)   //taukojen "korkeus", eli tauoille tehdään vain sleep ja skrollausrutiinit
              for (nuotti <-  nuottiTaiSointu._1)
              if(nuotti !=  nuottiTaiSointu._1.last){
                 ch1.noteOn(nuotti, 75)         // 75 = velocity (127 = max), säestysäänet, jos niitä on
               }   
              else { 
                ch1.noteOn(nuotti, 114)  // oltiin sortattu, eli melodia on vikana (ylin ääni = isoin numero)  
               }
           
        
      // nuotin pituus & skrollaus:            // TODO entä jos tahtiviivat eivät mene kuin Strömssöössä = ollut "liian" pitkiä nuotteja --> ei tahtiviivaa
	      if(nuottiTaiSointu._2 !=  nuotit.last){
            olisiAikaSkrollata += (nuottiTaiSointu._2 * ms).toInt
            if(olisiAikaSkrollata == ms*tahtilaji*2 ){            // rivillä on 2 tahtia (4000ms)
              Thread.sleep((nuottiTaiSointu._2 * ms).toInt)  // ms 
               if ( riviInd < kappale.kappale.size){
                  skrollaaa(riviInd)
                  riviInd += 1
               }
               olisiAikaSkrollata = 0
            }   
            
            else if(olisiAikaSkrollata > ms*tahtilaji*2 ){ 
              val paljonkoMentiinYliSkrollausRajan = olisiAikaSkrollata - ms*tahtilaji*2
              Thread.sleep((nuottiTaiSointu._2 * ms).toInt - paljonkoMentiinYliSkrollausRajan )  
               if ( riviInd < kappale.kappale.size){
                  skrollaaa(riviInd)
                  riviInd += 1
               }
               olisiAikaSkrollata = ms
              Thread.sleep(paljonkoMentiinYliSkrollausRajan )
            }
            
            else Thread.sleep((nuottiTaiSointu._2 * ms).toInt)
	      }  // end (if nuotit.last)
         
	      else Thread.sleep((nuottiTaiSointu._2 * ms).toInt)
	      
    // noteOff:
        if (nuottiTaiSointu._1(0) != 0)
          for (nuotti <- nuottiTaiSointu._1)  {            
             ch1.noteOff(nuotti)
          }   
      }
	  }
	
	  
    def rocknroll() = {                                  // R O K K I B Ä N D I    S T A R T
      
     
      var delayedNotes = Buffer[Int]()
      var vol = 70
      
	    for(nuottiTaiSointu <- nuotit){      
          if (nuottiTaiSointu._1(0) != 0)  
              for (nuotti <-  nuottiTaiSointu._1){
                
                 delayedNotes +=  nuotti 
                 if (delayedNotes.size > 3)
                   delayedNotes -= delayedNotes(0)
                
                  if(nuotti !=  nuottiTaiSointu._1.last){
                     ch1.noteOn(nuotti, 90)  
                     ch1.noteOn(nuotti, 90)   
                     ch2.noteOn(nuotti -12, 85)   // okt. alas
                     ch2.noteOn(nuotti -12, 85)
                  }   
                  else { 
                    ch1.noteOn(nuotti, 114)      // guit1
                    ch1.noteOn(nuotti, 114)      // guit1 , "chorus"-efektiä vähän, kun sama info x 2
                    ch2.noteOn(nuotti -12, 114)  // guit2
                    ch3.noteOn(nuotti -24, 127)  // bass, -2okt., basso tuplaa vain melodian
                    ch6.noteOn(nuotti -24, 127)  // bass2
                    ch6.noteOn(nuotti -36, 107)  // bass3
                  }
              }
        
          val montakoKertaaEhtiiSoittaaKahdeksasosan = (nuottiTaiSointu._2 / 0.5).toInt
        
          if (nuottiTaiSointu._1(0) != 0) vol = 73      // tauon aikana jatketaan volan "feidautumista"
          
          for( i<- 0 until montakoKertaaEhtiiSoittaaKahdeksasosan){
              
              for (delayedNuotti <- delayedNotes){
                ch4.noteOn(delayedNuotti -12, vol)
                ch5.noteOn(delayedNuotti -24 , vol-10)  
                if (vol > 30) vol -= 2    // jätetään tauoille hiljainen delay-jumitus
                else  delayedNotes = Buffer[Int]()   // pitkän nuotin tai tauon jälkeen ei haluta delayata vanhoja ääniä
              }
              Thread.sleep(ms/2)   // tämä rivi for delayedNuotti:n sisään --> ReallyWeirdDelay  :->>
        }	  
        
     
        Thread.sleep(((nuottiTaiSointu._2 * ms) - (montakoKertaaEhtiiSoittaaKahdeksasosan*ms/2)) .toInt)   
    
        olisiAikaSkrollata += (nuottiTaiSointu._2 * ms).toInt          // R O K K I B Ä N D I 
        if(olisiAikaSkrollata >= ms*tahtilaji*2 ){            
           if ( riviInd < kappale.kappale.size){
              skrollaaa(riviInd)
              riviInd += 1
              olisiAikaSkrollata = 0
           }
        }   
   
   
        // noteOff:
        if (nuottiTaiSointu._1(0) != 0)
            for (nuotti <- nuottiTaiSointu._1)  {            
                ch1.noteOff(nuotti); ch2.noteOff(nuotti -12); ch3.noteOff(nuotti -24); ch4.noteOff(nuotti -12); ch5.noteOff(nuotti -24) ; ch6.noteOff(nuotti -24) ; ch6.noteOff(nuotti -36) 
            } 
            for (delayedNuotti <- delayedNotes)  {            
                ch4.noteOff(delayedNuotti -12); ch5.noteOff(delayedNuotti -24)          
            } 
        
      }	//end for
    } // end rocknroll
    
}
 ///////////////////// end   simpleMIDIPlayer   ///////////////////////////////////////////////////////////////////////////



class simpleMIDIPlayerAdapter (nuottiData: Buffer[ViivastolleLaitettava], MIDIPatch:Int, kappale: Kappale, tahtilaji:Int, soitetaanko:Boolean = true) { // viimeinen boolean vain testaamista varten   
  
   val MIDINoteNumber = Map("cb1" -> 59,  "c1" -> 60, "c#1" ->61, "db1" -> 61, "d1" -> 62, "d#1" -> 63, "eb1" -> 63,  
       "e1" -> 64, "e#1" -> 65, "fb1"-> 64, "f1"-> 65,  "f#1"->66,  "gb1" -> 66, "g1" -> 67,  "g#1" -> 68, "ab1" -> 68, 
       "a1" -> 69, "a#1" -> 70, "hb1" -> 70, "b1" -> 70, "bb1" -> 70, "h1" -> 71, "cb2" -> 71, "h#1" -> 72, "c2" -> 72, 
       "c#2" -> 73, "db2" -> 73, "d2" -> 74,  "d#2" -> 75, "eb2" -> 75, "e2" -> 76, "fb2" -> 76, "e#2" -> 77, 
       "f2" -> 77, "f#2" -> 78, "gb2" -> 78, "g2" -> 79, "g#2" -> 80, "ab2" -> 80, "a2" -> 81, "a#2" -> 82, 
       "b2" -> 82, "hb2" -> 82, "bb2" -> 82, "h2" -> 83, "h#2" -> 84)

   
   def muunnaMIDInuoteiksi() = {    
       var nuottiNumberit = Buffer[Buffer[Int]]()    // tänne nuottien korkeudet
       var pituudet = Buffer[Double]()               // [0.5 ... 4.0]
       
           for (alkio<- nuottiData) {
               pituudet += alkio.pituus         
               var apubufferInt = Buffer[Int]()   // luodaan aina tyhjä buffer
               
               alkio match {
               case s: Sointu  => 
                           for(nuotti <- s.nuotit)
                                  apubufferInt += MIDINoteNumber(nuotti.asInstanceOf[Nuotti].korkeus)  // Map("nuotinnimi" --> Int)     
                           nuottiNumberit += apubufferInt.sorted  // melodia menee vikaksi
                             
               case n: Nuotti => 
                           apubufferInt += MIDINoteNumber(n.korkeus)
                           nuottiNumberit += apubufferInt
                     
               case t: Tauko =>  
                          apubufferInt += 0  // sovin itseni kanssa että tauon "korkeus" on 0
                          nuottiNumberit += apubufferInt
                } // end match
               
           } // end for
       
      
       val nuotitJaPituudet = nuottiNumberit.zip(pituudet)
       if(soitetaanko){   // tämä on vain UnitTestiä varten
           val player = new simpleMIDIPlayer(nuotitJaPituudet, MIDIPatch, kappale, tahtilaji) 
           player.soita()
       }
       nuotitJaPituudet  // palautetaan yksikkötestille tämä
   } 
  
}  ///////////////////// end   simpleMIDIPlayerAdapter  /////////////////////////////////////////////////////////