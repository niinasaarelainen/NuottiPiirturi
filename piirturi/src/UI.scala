import scala.io.StdIn._
import java.io.PrintWriter


class UI {
  
  
    var tiedostonNimi = ""
    var MIDIPatch = ""
    val lukija = new TiedostonLukeminen
    
    
    lukija.helppiTeksti()
    lukija.listaaTiedostot()
    
    do {
         tiedostonNimi = readLine("\n\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
    } while (!lukija.onkoListalla(tiedostonNimi))
  
    
    lukija.lueTiedosto(tiedostonNimi)
    
    do {
         MIDIPatch = readLine("\n\nMillä soundilla haluat kuulla kappaleen?\n" +
          "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
    } while (!"1234567".contains(MIDIPatch))
    
  
    val n = new NuottiPiirturi(lukija, MIDIPatch)
    n.execute()
    
    
    // simpleMIDIPlayerAdapter --> MIDI-soitto myös kyselee käyttäjältä inputtia ! 
    if(!MIDIPatch.equals(""))  // kuunnellaan  
         new simpleMIDIPlayerAdapter(n.nuottiData, MIDIPatch.toInt, n.viivasto.kappale, lukija.tahtilaji.toInt)
    else {        // käyttäjä valitsi että ei kuunnella
         n.viivasto.kappale.printtaaRuudulleIlmanAjastusta()
    }  
    
    val nimi = readLine("\nMillä nimellä talletetaan? Pelkkä ENTER ei tallenna mitään.")
    val kohdetiedosto = new PrintWriter("output/" + nimi+".txt")
    new TiedostonTallennus(n.viivasto.kappale, kohdetiedosto)    
    
    println("muista painaa F5, jotta tiedosto päivittyy Package Explorerissa.")
    
}