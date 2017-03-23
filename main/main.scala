// import piirturi.src

object test extends App{
  
     val ui = new UI()
     val lukija = new TiedostonLukeminen
     
    
    lukija.helppiTeksti()
    kelvollisenTiedostonKysyminenJaTarkistusLoop()
    ui.kayttajaValitseeMIDIPatchin()
   
                 
    val n = new NuottiPiirturi(lukija)
 println(n.inputBuffer.size)
    n.execute()
    if(!ui.MIDIPatch.equals(""))  // kuunnellaan  
         new simpleMIDIPlayerAdapter(n.nuottiData, ui.MIDIPatch.toInt, n.viivasto.kappale, lukija.tahtilaji.toInt)
    else {        // käyttäjä valitsi että ei kuunnella
         n.viivasto.kappale.printtaaRuudulleIlmanAjastusta()
    }  
    
    new TiedostonTallennus(n.viivasto.kappale, ui.kayttajaValitseeTiedostonTallennusnimen())    
    
    //// Happy End ////
    
    def kelvollisenTiedostonKysyminenJaTarkistusLoop():Unit = {
       lukija.listaaTiedostot()
       ui.kayttajaValitseeTiedoston(lukija)
       lukija.lueTiedosto(ui.tiedostonNimi.trim())
       println("lukija.nuottiAlkiot: " +lukija.nuottiAlkiot.size)
       if(lukija.lukemisenJalkeenEiNuottiDataa || lukija.nuottiAlkiot.isEmpty ){   // 
          println("\nvalitsemassasi tiedostossa ei ollut nuottidataa. Valitse toinen tiedosto tai muokkaa äsken valitsemaasi.\n\n")
          kelvollisenTiedostonKysyminenJaTarkistusLoop()
       }   
    }
  
}  