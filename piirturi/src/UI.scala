import scala.io.StdIn._
import java.io.PrintWriter


class UI {
  
  
    var tiedostonNimi = ""
    var MIDIPatch = ""
    
    
    
    
    def kayttajaValitseeTiedoston(lukija: TiedostonLukeminen) = {
        do {
           tiedostonNimi = readLine("\n\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
        } while (!lukija.loytyykoInputHakemistosta(tiedostonNimi.trim()))   
        
    }
    
    def valitetaanKayttajalleTyhjastaNuottiDatasta(lukija: TiedostonLukeminen) = {
      do{
          if(lukija.nuottiAlkiot.isEmpty)     // case: tiedostossa on dataa, jota yritetään tulkita nuoteiksi/soinnuiksi, esim. pelkkä <>, eli ei ole tyhjä tiedosto. Mutta lopputulos on: ei saatu yhtään nuottia aikaiseksi
        println("\nValitsemassasi tiedostossa ei ollut yhtään hyväksyttävää nuottia. Ei voi piirtää.\n" +
                 "Valitse jokin toinen tiedosto tai muokkaa äsken valitsemaasi tiedostoa.\n\n")
        }while(!lukija.nuottiAlkiot.isEmpty)  
        
    }
    
    
    def kayttajaValitseeMIDIPatchin() = {
        do {
           MIDIPatch = readLine("\n\nMillä soundilla haluat kuulla kappaleen? \nValitse 1-7 ja paina ENTER. Pelkkä ENTER ei tuota mitään ääntä.\n" +
           "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
        } while (!"1234567".contains(MIDIPatch))
    }
    
    def kayttajaValitseeTiedostonTallennusnimen() = {
          val nimi = readLine("\nMillä nimellä talletetaan? Pelkkä ENTER ei tallenna mitään.")
          val kohdetiedosto = new PrintWriter("output/" + nimi+".txt")
          println("muista painaa F5, jotta tiedosto päivittyy Package Explorerissa.")
          kohdetiedosto
    }
    
}