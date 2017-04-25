
object main extends App{
  
     val inputhakemistonNimi =  "./input/"
     val ui = new UI(inputhakemistonNimi)
     val lukija = new TiedostonLukeminen(inputhakemistonNimi)
    
     
     terveTuloa()
     
     
       
    def terveTuloa(): Unit = {
        var valinta = ""
        do {
           ui.terveTuloa() match {
              case "1" => lukija.helppiTeksti(); valitseToiminto()
              case "2" => ohjelmanRunko()
              case _   =>  terveTuloa()
           } 
        } while (!"12".contains(valinta))
    }
  
    
    
    def  ohjelmanRunko(): Unit = {
        kelvollisenTiedostonKysyminenJaTarkistusLoop()
        ui.kayttajaValitseeMIDIPatchin()
        val n = new NuottiPiirturi(lukija)
        n.execute()
        if(!ui.MIDIPatch.equals("")){  // kuunnellaan  
            val adapter = new simpleMIDIPlayerAdapter(n.nuottiData, ui.MIDIPatch.toInt, n.viivasto.kappale, lukija.tahtilaji.toInt)
            adapter.muunnaMIDInuoteiksi
        } else         // käyttäjä valitsi että ei kuunnella
            n.viivasto.kappale.printtaaRuudulleIlmanAjastusta()
        new TiedostonTallennus(n.viivasto.kappale, ui.kayttajaValitseeTiedostonTallennusnimen())   
        valitseToiminto()
    }    
    
     
    def kelvollisenTiedostonKysyminenJaTarkistusLoop(): Unit = {
        ui.listaaTiedostot()
        ui.kayttajaValitseeTiedoston(lukija)
        lukija.lueTiedosto(ui.tiedostonNimi.trim())
        if(lukija.nuottiAlkiot.isEmpty ){     // case: oli pelkkää virheellistä nuottidataa, esim. <>, joista generoitui kuitenkin "", eli ei sama asia kuin tyhjä tiedosto
           println("\nvalitsemassasi tiedostossa ei ollut nuottidataa. Valitse toinen tiedosto tai muokkaa äsken valitsemaasi.\n\n")
           kelvollisenTiedostonKysyminenJaTarkistusLoop()
        }   
    }
    
    
    def valitseToiminto(): Unit = {
        ui.mitaTehdaanSeuraavaksi() match{
            case ""  => println("\nKiitos ja näkemiin."); System.exit(0)
            case "1" => lukija.helppiTeksti(); valitseToiminto()
            case "2" => ohjelmanRunko()
        }      
    }
  
}  