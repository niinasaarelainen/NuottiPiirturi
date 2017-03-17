import scala.io.Source
import scala.collection.mutable.Buffer
import scala.io.StdIn._
import java.io._


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
  

  
  new NuottiPiirturi(lukija)
}