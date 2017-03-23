// import piirturi.src

object test extends App{
  
     val ui = new UI()
     val lukija = new TiedostonLukeminen
     
    
    lukija.helppiTeksti()
    
    lukija.listaaTiedostot()
    ui.kayttajaValitseeTiedoston(lukija)
    lukija.lueTiedosto(ui.tiedostonNimi.trim())
    ui.valitetaanKayttajalleTyhjastaNuottiDatasta(lukija)
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
    
    //// The End ////
  
}  