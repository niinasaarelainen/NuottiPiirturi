import scala.io.StdIn._
import java.io.PrintWriter
import java.io._


class UI(inputhakemistonNimi: String) {
  
    val inputhakemisto = new File(inputhakemistonNimi)
    var tiedostonNimi = ""
    var MIDIPatch = ""
    
    
    
    def terveTuloa() = {
         var valinta = ""
        do {
           valinta = readLine( "\n\n\t !! Tervetuloa käyttämään nuotinnusohjelmaa !! \n\nValitse 1, jos haluat itse tehdä syötetiedoston, eli kirjoittaa kappaleen nuotit  "+
                               "\nValitse 2, jos haluat nähdä nuotteina jonkin valmiiksikirjoitetun kappaleen " +
                               "\nja paina ENTER. \n" +
             "\neli 1(+ENTER)= ohjeet,  2(+ENTER)= listaa kappaleet ja nuotinna ")
        } while (!"12".contains(valinta))
        valinta  
    }
    
    
    def listaaTiedostot() = {
        var montakoNimeaRiville = 0
        println("\n")
        for (tiedosto <- inputhakemisto.listFiles()) {     
           if (tiedosto.isFile) {
              print(tiedosto.getName + '\t')
              montakoNimeaRiville += 1
              if (montakoNimeaRiville == 8){
                println()
                montakoNimeaRiville = 0
              }
           }    
        }
    } 
    
          
    def kayttajaValitseeTiedoston(lukija: TiedostonLukeminen) = {
        do {
           tiedostonNimi = readLine("\n\nMinkä nimisen tiedoston haluat nuoteiksi? Valitse ylläolevista. ")
        } while (!loytyykoInputHakemistosta(tiedostonNimi.trim()))  
    }

  
    def loytyykoInputHakemistosta(nimi: String): Boolean = {
        for (tiedosto <- inputhakemisto.listFiles())
          if (tiedosto.isFile && tiedosto.getName.toLowerCase() == nimi.toLowerCase().trim())
            return true
        false
    }
    
    
    def kayttajaValitseeMIDIPatchin() = {
        do {
           MIDIPatch = readLine("\n\nMillä soundilla haluat kuulla kappaleen? \nValitse 1-7 ja paina ENTER. Pelkkä ENTER ei tuota mitään ääntä.\n" +
           "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
        } while (!"1234567".contains(MIDIPatch))
    }

    
    def mitaTehdaanSeuraavaksi() = {
        var valinta = ""
        do {
           valinta = readLine("\n\nMitä tehdään seuraavaksi? \nValitse 1 tai 2 ja paina ENTER. Pelkkä ENTER lopettaa ohjelman.\n" +
             "1= ohjeet,  2= listaa kappaleet ja nuotinna,  ENTER= lopetetaan tältä erää ")
        } while (!"12".contains(valinta))
        valinta  
    }
     
    
    def kayttajaValitseeTiedostonTallennusnimen() = {
        val nimi = readLine("\nMillä nimellä talletetaan? Pelkkä ENTER ei tallenna mitään.")
        val kohdetiedosto = new PrintWriter("output/" + nimi+".txt")
        println("muista painaa F5, jotta tiedosto päivittyy Package Explorerissa.")
        kohdetiedosto
    }
    
    
}