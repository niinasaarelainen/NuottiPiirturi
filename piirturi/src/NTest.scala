import org.scalatest.Assertions._
import org.scalatest._

import scala.collection.mutable.Buffer
import scala.io.Source
import java.io._
import java.io.Console._
import scala.io.StdIn._

class NTest extends FlatSpec with Matchers {

  "TiedostonLukeminen.oikeellisuusTesti()" should "find non-valid note names and lengths" in {

    val luk = new TiedostonLukeminen
    val nuottejaVaarin = Buffer("t1-", "d3-", "f", "dc1", "p", "1c", "2f", "f2f2", "f2f", "f2g----", "hb22----", "c11", "d#b", "db#", "b#",
      "ddd", "d1d", "-.", "----", "#c", "dd1", "g##", "abb", "dis", "c1.", "c1---.", "c1-----", "c1------------", "c1----.") // viisi vikaa virheelliset pituudet

    assert(laskeVirheet(luk, nuottejaVaarin)(0)  == nuottejaVaarin.size, "***oikeellisuusTesti claims that " + laskeVirheet(luk, nuottejaVaarin)(2) + " is non-valid input, when it isn't")
   }
  
  it should "find non-valid rest names and lengths" in {

    val luk = new TiedostonLukeminen
    val taukojaVaarin = Buffer("za", "z#--", "zz", "zz-top", " z", "tauko", "az")

     assert(laskeVirheet(luk, taukojaVaarin)(0) == taukojaVaarin.size, "***oikeellisuusTesti claims that " + laskeVirheet(luk, taukojaVaarin)(2)+ " is non-valid input, when it isn't")
  }

  it should "find valid note names and lengths" in {

    val luk = new TiedostonLukeminen
    val nuottejaOikein = Buffer("d#1---", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--", "f#1---", "d2b", "a----#1",
      "--c1", "----g2", "ab2", "a#2----", "b1", "bb1", "b2", "bb2", "b#2")

    assert(laskeVirheet(luk, nuottejaOikein)(0) == 0, "***oikeellisuusTesti claims that " + laskeVirheet(luk, nuottejaOikein)(1)+" is valid input, when it isn't")
  }
  
  it should "find valid rest names and lengths" in {

    val luk = new TiedostonLukeminen
    val taukojaOikein = Buffer("z", "z-", "z-.", "z--", "z--.", "z---", "z----") // kaikki sallitut pituudet

    assert(laskeVirheet(luk, taukojaOikein)(0) == 0, "***oikeellisuusTesti claims that " + laskeVirheet(luk, taukojaOikein)(1) + " is valid input, when it isn't")
  }

  
  "NuottiPiirturi.lyricsBuffer" should "have right lyrics" in {

    val sanat = Buffer("Jaak-", "ko", "kul-", "ta,", "Jaak-", "ko", "kul-", "ta,", "he-", "rää", "jo,", "he-", "rää", "jo.", "Kel-", "lo-", "ja-", "si", "soi-", "ta,", "kel-", "lo-", "ja-", "si", "soi-", "ta,", "pium", "paum", "poum,", "pium", "paum", "poum.")
    val luk = new TiedostonLukeminen

    luk.lueTiedosto("jaakko")
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()

    assertResult(sanat.size) {
      piirturi.lyricsBuffer.size
    }

    for (i <- 0 until sanat.size) {
      assert(sanat(i).equals(piirturi.lyricsBuffer(i)))
    }

  }

  "NeljasosaNuotti" should "have right nuppi, nimiMapissa, etumerkki and extraetumerkki" in {

    val neljasOsa = new NeljasosaNuotti("c#2")
    assert(neljasOsa.nuppi == "@@" && neljasOsa.nimiMapissa == "c2" && neljasOsa.etumerkki == "#" && neljasOsa.getExtraetumerkki == "")

  }

  "KahdeksasosaPari" should "draw eight note couple stems down and ignore second flat" in {

    var kuva = Buffer[String]()

    kuva += "            "
    kuva += "            "
    kuva += "            "
    kuva += "  b@@   @@  " // etumerkkilogiikan mukaan toista alennusta ei saa piirtää
    kuva += " --|----|-- "
    kuva += "   |    |   "
    kuva += "---======---"
    kuva += "            "
    kuva += "------------"
    kuva += "            "
    kuva += "------------"
    kuva += "            "
    kuva += "------------"
    kuva += "            "
    kuva += "------------"
    kuva += "            "
    kuva += "            "
    kuva += "            "
    kuva += "            "

    val piirturi = new NuottiPiirturi(new TiedostonLukeminen)
    val nuottiAlkiot = Buffer("b2", "b2")
    var nuottiData = Buffer[ViivastolleLaitettava]()
    nuottiData = piirturi.kasitteleNuottiTieto(nuottiAlkiot, nuottiData)
    val pari = new KahdeksasosaPari(nuottiData(0), nuottiData(1))

    println(pari.kuva) // TODO   ei toimi jos tämän rivin ottaa pois ?!?!?

    assertKuva(kuva, pari.kuva)

  }

  "Pisteellinen NeljasosaNuotti" should "draw dotted quarter note stem up" in {

    var odotettu = Buffer[String]()

    odotettu += "           "
    odotettu += "           "
    odotettu += "           "
    odotettu += "           "
    odotettu += "           "
    odotettu += "           "
    odotettu += "-----------"
    odotettu += "    |      "
    odotettu += "----|------"
    odotettu += "    |      "
    odotettu += "---@@.-----"
    odotettu += "           "
    odotettu += "-----------"
    odotettu += "           "
    odotettu += "-----------"
    odotettu += "           "
    odotettu += "           "
    odotettu += "           "
    odotettu += "           "

    assertKuva(odotettu, new PisteellinenNeljasosaNuotti("h1").kuva)
  }

  "TiedostonLukeminen.kasitteleKappaleenNimiJaTahtilaji()" should "find tahtilaji and kappaleenNimi when both hash-tagged in inputfile" in {

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("kalevala")

    assert(luk.kappaleenNimi == " Kalevala - kuudestoista runo (ote)", "***kappaleenNimi-muuttuja väärin")
    assert(luk.tahtilaji == "5", "***tahtilajin pitäisi olla 5 (eli 5/4 määritelty ohjelmassa 5)")
  }

  it should "use defaul values for tahtilaji and kappaleenNimi when neither hash-tagged in inputfile" in {
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("jaakko")

    assert(luk.kappaleenNimi == "", "***kappaleenNimi-muuttuja väärin")
    assert(luk.tahtilaji == "4", "***tahtilajin pitäisi olla oletusarvo eli 4")
  }

  "Viivasto.kappale" should "have Title + 2 staffs" in {

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("jaakko_2rivia") // kappaleessa on 4 kpl 4/4-tahteja ja ohjelma jakaa sen tasan kahdelle riville
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()

    assert(piirturi.viivasto.kappale.kappale.size == 3, "***kappale.size not 3") // kappaleen nimi on eka entry  + 2 riviä musaa
    assert(piirturi.viivasto.kappale.kappale(0).last.contains("Jaakko") == true, "***kappaleen Title puuttuu/väärä")
    // ennen nimeä on tyhjiä rivejä muotoilun vuoksi 
  }

  "NuottiData and NuottiDataParitettu" should "be of right length" in {
    // tutkitaan nuottidatan säilyttäjiä, molemmat tyyppiä Buffer[ViivastolleLaitettava]
    // NuottiDataParitettua ei voi luoda ennen NuottiData:n luontia. Kätevää tutkia molempia samalla.

    val luk = new TiedostonLukeminen()

    // tiedostossa on pelkkiä kahdeksasosan mittaisia "eventtejä" 2 tahtia = 8*2 = 16 nuottia/taukoa
    luk.lueTiedosto("kahdeksasosia")
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()

    assert(piirturi.nuottiData.size == 16, "***nuottiDatan pituus ei ollut oikein")
    assert(piirturi.nuottiDataParitettu.size == 10, "***nuottiDataParitettu pituus ei ollut oikein")
  }

  it should "have right lengths" in {

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("kahdeksasosia")
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()
    for (nuottiTaiTauko <- piirturi.nuottiData)
      assert(nuottiTaiTauko.pituus == 0.5, "***nuottiDatassa kaikkien elementtien pituus pitäisi olla 0.5")

    // ekassa tahdissa 4 paria(parin pituus 1.0), sitten testataan että tauon jälkeinen kahdeksasosa ei ota seuraavasta nuotista itselleen paria  
    val pariDatanPituudet = Buffer(1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 0.5, 0.5)
    for (i <- 0 until piirturi.nuottiDataParitettu.size)
      assert(piirturi.nuottiDataParitettu(i).pituus == pariDatanPituudet(i), "***nuottiDataParitettu: pituusvirhe")
  }

  it should "have right hights(=note names)" in {

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("kahdeksasosia")
    val piirturi = new NuottiPiirturi(luk)
    piirturi.execute()

    val nuottiDatanKorkeudet = Buffer("c#1", "d#1", "e#1", "f#1", "g#1", "a#1", "h#1", "c#2", "z", "cb2", "db2", "eb2", "fb2", "gb2", "ab2", "b2")
    for (i <- 0 until piirturi.nuottiData.size)
      piirturi.nuottiData(i) match {
        case n: Nuotti => assert(n.asInstanceOf[KahdeksasosaNuotti].korkeus == nuottiDatanKorkeudet(i), "***nuottiData: nuotin korkeusvirhe")
        case t: Tauko  => assert(t.asInstanceOf[KahdeksasosaTauko].korkeus == "c2", "***nuottiData: tauon korkeusvirhe") // kaikkien taukojen piirtokorkeus on c2
        case _         => fail // FAIL, koska syötteessä ei ole sointuja tai muuta kuin em. 2 kategoriaa
      }

    val nuottiDatParitettuKorkeudet = Buffer(Array("c#1", "d#1"), Array("e#1", "f#1"), Array("g#1", "a#1"), Array("h#1", "c#2"), Array("z"), Array("cb2"), Array("db2", "eb2"), Array("fb2", "gb2"), Array("z"), Array("b2"))

    for (i <- 0 until piirturi.nuottiDataParitettu.size)
      piirturi.nuottiDataParitettu(i) match {
        case n: KahdeksasosaNuotti =>
          assert(n.asInstanceOf[KahdeksasosaNuotti].korkeus == nuottiDatParitettuKorkeudet(i)(0), "***nuottiDataParitettu: kahdeksasosan korkeusvirhe")
        case p: KahdeksasosaPari =>
          assert(p.asInstanceOf[KahdeksasosaPari].korkeus == nuottiDatParitettuKorkeudet(i)(0) && p.asInstanceOf[KahdeksasosaPari].korkeus2 == nuottiDatParitettuKorkeudet(i)(1), "***nuottiDataParitettu: parin korkeusvirhe")
        case t: Tauko =>
          assert(t.asInstanceOf[KahdeksasosaTauko].korkeus == "c2", "***nuottiDataParitettu: tauon korkeusvirhe") // kaikkien taukojen piirtokorkeus on c2
        case _ => fail // FAIL, koska syötteessä ei ole muuta kuin em. 3 kategoriaa
      }
  }

  "TiedostonLukeminen.tarkistaVirheet() --as stub--" should "find 3 syntax errors(= wrongly formulated note data) in file '3errors'" in {
    /*
       Simuloidaan käyttäjän virheidenkorjausprosessia. Ensin annetaan syöte, jossa on 3 syntaksivirhettä 
       nuottidatassa.
       Sitten kuvitellaan tilanne jossa käyttäjä korjaa yhden, tallettaa tiedoston ja nyt syötteessä on 2 virhettä jne.
       Testattava ohjelma löytää syötetiedoston ensimmäisen virheen ja odottaa käyttäjän korjaavan sen,
       tutkii talletetun tiedoston ja joko löytää uuden virheen tai voi aloittaa nuottidatan jatkokäsittelyn.
      
       Syötteessä pelkkiä sointuja, TiedostonLukeminenStub-luokassa muutettu vain sointuja
       käsittelevää koodia metodissa tarkistaSoinnunVirheet(), joka on metodin tarkistaVirheet()-sisällä
     * 
     */
    val syotteet = Buffer("3errors", "2errors", "1error", "0errors", "0errors")
    // viimeinen 0errors ohjelman kannalta ylimääräinen, mutta testaa ettei lueTiedostoa lueta 5 kertaa, vaan 4, koska virheettömän tiedoston jälkeen pitäisi poistua virheenkorjaus-luupista

    val lukStub = new TiedostonLukeminenStub(syotteet) // TiedostonLukeminenStub löytyy tämän tiedoston lopusta

    // kuten tiedostosta 3errors voi havaita, on rivi 1 virheetön, sen jälkeen yksi virhe riveillä 2, 3 ja 4
    assert(lukStub.stubMessages(0) == "löydettiin virhe rivillä 2", "eka syötevirhe-ongelma")
    assert(lukStub.stubMessages(1) == "löydettiin virhe rivillä 3", "toka syötevirhe-ongelma")
    assert(lukStub.stubMessages(2) == "löydettiin virhe rivillä 4", "kolmas syötevirhe-ongelma")
    intercept[IndexOutOfBoundsException] { lukStub.stubMessages(3) } // tällä testataan, että tiedostolla "0errors" ei generoidu virheilmoitusta, eli stubMessages on kolmen alkoin mittainen
    assert(lukStub.stubMoneskokerta == 3, "lueTiedosto()-metodia kutsuttiin 4 kertaa")
  } // stubMoneskokerta alkuarvo oli 0 => arvo 3 = 4.kutsukerta

  "TiedostonLukeminen.loytyykoInputHakemistosta" should "give false when file not found in directory" in {
    val luk = new TiedostonLukeminen

    assert(luk.loytyykoInputHakemistosta("kkk") == false, "valitussa input-hakemistossa ei ole tiedostoa nimeltä \"kkk\"")
 
  }
  
  it should "give true when file found in directory" in {
    val luk = new TiedostonLukeminen

    assert(luk.loytyykoInputHakemistosta("§") == true, "valitussa input-hakemistossa on tiedosto nimeltä \"§\"")

  }

  "UI.kayttajaValitseeMIDIPatchin() --as stub--" should "accept user key presses 1-7 and ENTER, nothing else" in {

    /* alkuperäinen koodi luokassa UI:
       do {
           MIDIPatch = readLine("\n\nMillä soundilla haluat kuulla kappaleen?\n" +
           "ENTER= en millään,  1= piano,  2= vibrafoni,  3= rock-urut,  4= syna,  5= akustinen kitara,  6= rokkibändi,  7=music box  ")
        } while (!"1234567".contains(MIDIPatch))
     */

    // stubina, case 1, jossa käyttäjä syöttää 2 hylättävää ja kolmantena hyväksyttävän arvon:  
    var MIDIPatch = "alkuarvo"
    MIDIPatch = MIDIPatchinValinta(Array("8", "11", "3"), MIDIPatch) // "3" on hyväksyttävä arvo
    assert(MIDIPatch == "3", "käyttäjän syoteSekvenssi 1:n jälkeen MIDIPatch pitäisi olla 3")

    // case 2:   käyttäjä painaa ENTER (=ei halua kuunnella kappaletta)
    MIDIPatch = "alkuarvo"
    MIDIPatch = MIDIPatchinValinta(Array(""), MIDIPatch)
    assert(MIDIPatch == "", "käyttäjän syoteSekvenssi2:n jälkeen MIDIPatch pitäisi olla tyhjä merkkijono eli painettiin ENTER")

    // case 3: simuloi tilannetta, jossa pelkkiä virheellisiä syötteitä "loputtomasti", i kasvaa Array:n pituuden ulkopuolelle, koska while-looppi vain pyörii
    MIDIPatch = "alkuarvo"
    intercept[IndexOutOfBoundsException] {
      MIDIPatch = MIDIPatchinValinta(Array("9", "-1", "t", "0", " ", "77", "moi", "#", "."), MIDIPatch)
    }
  }

  "simpleMIDIPlayerAdapter" should "transform nuottiData to correct Buffer[(Buffer[Int], Double)]" in {

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("jaakko") // (kansiosta input_virheita)
    val n = new NuottiPiirturi(luk)
    n.execute() // MIDI-Patch 3 valittu käyttäjän puolesta
    val MIDIAdapter = new simpleMIDIPlayerAdapter(n.nuottiData, 3, n.viivasto.kappale, luk.tahtilaji.toInt, false) // false= ei varsinaisesti haluta kuunnella Jaakko-kultaa

    /* Jaakko-kulta, tarkoituksella sointuja(2 ääntäkin on määritelty soinnuksi) ja taukoja seassa, 
        jotta testidatassa on kaikki 3 pääkategoriaa: nuotti, tauko, sointu 
           	c2- d2- e2- c2-   				c2- d2- e2- c2- 
 						<e2-, c2-> f2- g2- z-    	<e2-,c2-> f2- g2- z-
 						<g2,c2> a2 g2 f2 e2- c2- 	<g2,c2> a2 g2 f2 e2- c2-
 						c2- g1- c2-. z 						c2- g1- c2-- 
 			* 
 			*   huom! koodi stää soinnut nousevaan stykseen .sorted, eli <e2-, c2-> onkin (72,76)   
 			*   tauon olen määritellyt MIDI-korkeudeksi 0 */
    val jaakkoKullanMIDINumbers = Array(Array(72), Array(74), Array(76), Array(72), Array(72), Array(74), Array(76), Array(72),
      Array(72, 76), Array(77), Array(79), Array(0), Array(72, 76), Array(77), Array(79), Array(0),
      Array(72, 79), Array(81), Array(79), Array(77), Array(76), Array(72), Array(72, 79), Array(81), Array(79), Array(77), Array(76), Array(72),
      Array(72), Array(67), Array(72), Array(0), Array(72), Array(67), Array(72), Array(0))

    for (i <- 0 until MIDIAdapter.nuotitJaPituudet.size) {
      for (korkeus <- 0 until MIDIAdapter.nuotitJaPituudet(i)._1.size) { // Tuple._1 = Buffer[Int]  eventin korkeus tai korkeudet soinnun tapauksessa
        assert(MIDIAdapter.nuotitJaPituudet(i)._1(korkeus) == jaakkoKullanMIDINumbers(i)(korkeus), "***virhe : " + (i + 1) + ". eventti, " + (korkeus + 1) + ". korkeus eventissä")
        assert(MIDIAdapter.nuotitJaPituudet(i)._2 == n.nuottiData(i).pituus, "***pituusongelmaa pukkasi: " + (i + 1) + ". eventti") // Tuple._2 = Double, sama kuin nuottiolion pituus-kenttä
      }
    }
  }

  "The program" should "produce song 'Flight of the Bumble Bee' similarily as in correctly printed file 'bumble.txt'" in {
    // tämä testi on ylläpitoa varten, eli integraatiotesti, monien ohjelman osien tulee toimia oikein, jotta oikea output saadaa aikaan. 
    // only excludes classes UI, SimpleMIDIPlayer and SimpleMIDIPlayerAdapter, and some of Note Values, which are tested in the following test

    // musiikin maisterin koulutuksella ja silmilläni olen todennut tiedoston bumble.txt oikeaksi.
    // jos ohjelmaa tulevaisuudessa muutetaan, voi tämä testi lakata toimimasta.

    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("bumble") // (kansiosta input_virheita)
    val nuottipiirturi = new NuottiPiirturi(luk)
    nuottipiirturi.execute()
    val bumblePrinted = Source.fromFile("./output/bumble.txt")

    assertFileVersusKappale(nuottipiirturi.viivasto.kappale.kappale, bumblePrinted)

  }

  it should "produce test file 'allViivastolleLaitettavaClasses' similarily as in correctly printed file 'all.txt'" in {
    // in test file there are all the different classes that inherit trait ViivastolleLaitettava,
    // both stem up and down from each type - excluding rests which don't have stems and are always placed in constant height in staff  

    // class Sointu is tested with all the note values as well, and 2-13 notes in chord

    // The last row of music in all.txt is correct in terms of program logic. It cannot find place for bar line (one bar has now 8 beats), since 
    // time signature is 4/4 and no note ends on 4, the closest is 4 and half beats. So no bar line is possible to draw, 
    // which is actually good indicator for the user that the rhythm he/she thought probably is wrong, or that she/he made an error in input file by accident
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("allViivastolleLaitettavaClasses") // (kansiosta input_virheita)
    val nuottipiirturi = new NuottiPiirturi(luk)
    nuottipiirturi.execute()
    val allPrinted = Source.fromFile("./output/all.txt")

    assertFileVersusKappale(nuottipiirturi.viivasto.kappale.kappale, allPrinted)
  }

  "Viivasto.kasitteleLyriikat()" should "cope with more lyrics than notes" in {

    // vaihteeksi luodaan lyriikat ja nuottiData manuaalisesti (hankalasti), jotta aina ei jouduta kutsumaan luokkia 
    // TiedostonLukeminen & NuottiPiirturi

    // case: 4 syllables & 3 notes 
    val sanat = Buffer("Jaak-", "ko", "kul-", "ta,")
    assertLyricsDifferentSizeThanNoteData(sanat)

  }

  it should "cope with less lyrics than notes" in {

    // case: 2 syllables & 3 notes 
    val sanat = Buffer("Jaak-", "ko")
    assertLyricsDifferentSizeThanNoteData(sanat)

  }

  it should "cope with more letters in syllables than print area" in {
    // case: actually long words or case: where user has forgotten to make syllables  (should be f.ex for-ward, a-head)
    val sanat = Buffer("Straight", "forward,", "straight", "ahead!!!")

    // 1/8-note couple has the smallest print area for lyrics: 6 chars per note
    val pari = new KahdeksasosaPari(new KahdeksasosaNuotti("c1"), new KahdeksasosaNuotti("d1")).asInstanceOf[ViivastolleLaitettava]
    var nuottiData = Buffer(pari, pari)

    val v = new Viivasto(nuottiData, sanat, "4")  // "4" refers to Time Signature 4/4
    v.piirraNuotit()
    assert(v.unitTestLiitosCounter == 2, "***adding 2 eight couples FAIL")
    assert(v.kappale.kappale(0).last.contains("Straigforwarstraigahead!"), "***wrong lyrics") // joka sanasta 6 ekaa kirjainta
  }

  "TiedostonLukeminen.lueTiedosto__Recover Badly Formulated Input" should "#case1: too much white space" in {
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("_spaces")

    assert(luk.nuottiAlkiot.size == 6, "***syötetiedostossa on 6 nuottitapahtumaa") // sointu on 1 tapahtuma. 
    //Jos välilyönneistä ei oltaisi selvitty, ei nuottialkiota olisi lisätty nuottiAlkiot:hin, vaan vaadittu käyttäjää korjaamaan virhe
  }

  it should "#case2: empty chords" in {
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("_emptyChords")

    assert(luk.nuottiAlkiot.size == 0, "***syötetiedostossa pitäisi olla 0 nuottitapahtumaa")
    // tyhjiä sointuja ei laiteta nuottiAlkiot:hin, koska sitä ei voi soittaa. Point: ohjelma ei kaatunut
  }

  it should "#case3:  chords written together without space" in {
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("_chordsWrittenTogetherWithoutSpace")

    // the note/chord data is split from white space, so it is crucial to add space if user
    // didn't do it in case <c1,d1><e1,f1>   =>  must be correscted to  <c1,d1> <e1,f1>
    assert(luk.nuottiAlkiot.size == 3, "***syötetiedostossa pitäisi olla 3 sointua")
  }

  it should "#case4: use of tabulator" in {
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("_tabs") // the file is full of tabs (hard to see)

    // the note/chord data is split from white space, so it is crucial to add space if user
    // didn't do it in case <c1,d1><e1,f1>   =>  must be correscted to  <c1,d1> <e1,f1>
    assert(luk.nuottiAlkiot.size == 6, "***syötetiedostossa pitäisi olla 6 nuottia/sointua")
  }
  

  "NuottiPiirturi.tutkiEtumerkit()" should "give out right accidentals" in {
    //  testing all the cases for how to draw accidentals within a bar. 
    //  testing how extraEtumerkki behaves. That indicates that some extra rule is in hand when drawing or when omit drawing an b, #, §
    //  § = this sign is needed when note is made natural after being # or b, not needed after bar line
    //  n =  is my own invented syntax for a note that is actually # or b but the sign is not drawn, since it is only necessary to draw an accidental only once in a bar
    //  assuming it is not overruled by an oppisite sign(#, b) or made natural (§)
    
                 // 1.bar:      b2  b2    h2  h2  b2 h2    b2  h2   h#2  b2   h2  h#2
    var extraEtumerkit = Buffer("", "n", "§", "", "", "§", "", "§", "", "",  "§", "")   // indeksit 0-11, Time Signature 6/4, thus one bar consists of 12 eigth notes
         // 2.bar:    //h2 h#2  b2 h2   h2  b2    b2 h#2   b2  h2   h2  b2
    extraEtumerkit += ("", "", "", "§", "", "",   "n", "", "", "§", "", "")  // indeksit 12-23
    // 3.bar:          h2  b2  h#2 b2 h#2  h2 	 h2  h2  b2 b2 		 h#2 h#2
     extraEtumerkit += ("", "", "", "", "", "§", "", "", "", "n",  "", "n")  // indeksit 24-35
    // 4.bar:          ab2 ab2   a2 a2   ab2 a2    a2 ab2   a2  a#2   ab2 a2
      extraEtumerkit += ("", "n", "§","", "","§",  "", "",  "§", "",  "", "§")  // indeksit 36-47
    // 5.bar:         a2 a#2  ab2 a2   a2 ab2    ab2 a#2   ab2  a2   a2  a2
   extraEtumerkit += ("","",  "", "§", "","",    "n", "",  "",  "§", "", "")  // indeksit 48-59
       
    val luk = new TiedostonLukeminen()
    luk.lueTiedosto("_testAccidentals")
    val nuottipiirturi = new NuottiPiirturi(luk)
    nuottipiirturi.execute()

    for (i <- 0 until nuottipiirturi.nuottiData.size)
      assert(nuottipiirturi.nuottiData(i).asInstanceOf[Nuotti].getExtraetumerkki == extraEtumerkit(i), "virhe indeksissä: " + i)
  }
  
/*  EI NÄIN !!    odottaa testin aikana syötettä
  "ReadLine" should "work" in {
  val in = new ByteArrayInputStream("abc".getBytes)
  System.setIn(in)
  readLine() === "abc"
}   */

  ////////   A P U M E T O D I T :    /////////////////////////////////////////////////////////////////////////////////////////////////////////

  def laskeVirheet(luk: TiedostonLukeminen, syotteet: Buffer[String]) = {
    var virheita = 0
    var huonoSyote = ""
    var oikeaSyoteVaikkaPitiOllaVainVirheita = ""
    for (syote <- syotteet) {
        if (luk.oikeellisuusTesti(syote) != ""){
          virheita += 1
          huonoSyote = syote
        }  
        else oikeaSyoteVaikkaPitiOllaVainVirheita = syote
    }
    Buffer(virheita, huonoSyote, oikeaSyoteVaikkaPitiOllaVainVirheita)
    // only the last huonoSyote and oikeaSyoteVaikkaPitiOllaVainVirheita are kept in memory
  }

  def assertKuva(odotettuKuva: Buffer[String], nuotinKuva: Buffer[String]) = {

    assertResult(odotettuKuva.size) { // jos nuotinKuvassa on ylimääräisiä rivejä lopussa
      nuotinKuva.size
    }

    for (i <- 0 until odotettuKuva.size) {
      assert(odotettuKuva(i).equals(nuotinKuva(i)))
    }
  }

  def MIDIPatchinValinta(sekvenssi: Array[String], MIDIPatchParametrina: String) = {
    var i = 0;
    var MIDIPatch = MIDIPatchParametrina
    do {
      MIDIPatch = sekvenssi(i)
      i += 1
    } while (!"1234567".contains(MIDIPatch))
    MIDIPatch
  }

  def assertFileVersusKappale(ohjelmanGeneroimakappale: Buffer[Buffer[String]], tiedostostaLuettuKapple: Source) = {

    var kappaleenKaikkiRivitPerakkain = Buffer[String]()
    for {
      viivasto <- ohjelmanGeneroimakappale
      rivi <- viivasto
    } kappaleenKaikkiRivitPerakkain += rivi

    var i = 0
    try {
      for (rivi <- tiedostostaLuettuKapple.getLines) {
        assert(kappaleenKaikkiRivitPerakkain(i) == rivi, "first error in line: " + (i + 1))
        i += 1
      }
    } finally {
      tiedostostaLuettuKapple.close()
    }
  }

  def assertLyricsDifferentSizeThanNoteData(sanat: Buffer[String]) = {

    var nuottiData = Buffer(new NeljasosaNuotti("c2").asInstanceOf[ViivastolleLaitettava], new NeljasosaNuotti("d2").asInstanceOf[ViivastolleLaitettava], new NeljasosaNuotti("e2").asInstanceOf[ViivastolleLaitettava])
    val v = new Viivasto(nuottiData, sanat, "4") //vika parametri = tahtilaji
    v.piirraNuotit() // käskyttää metodia kasitteleLyriikat()

    // v.kappale.kappale should include third syllable, as nuottiData has 3 notes; 4.syllable "ta" should not be included
    if (sanat.size == 4) {
      assert(v.kappale.kappale(0).last.contains("kul-"), "***last syllable not found") // lyriikat ovat vikassa indeksissä
      assert(!v.kappale.kappale(0).last.contains("ta,"), "***too much lyrics in output")
    } else if (sanat.size == 2) assert(v.kappale.kappale(0).last.contains("ko"), "***lessLyrics ei toimi")

    // unitTestLiitosCounter counts how many times a note was added to Staff, should be 3 also when lyrics.size was 2 => testing 
    // that less lyrics didn't prevent adding more notes to the song 
    assert(v.unitTestLiitosCounter == 3, "***nuottiolioita liitettiin 3 kpl viivastoon")

    // actually, also a test here is that the program did not throw exception. Could have with bad code.
  }

  //////  S T U B    C L A S S E S:  ///////////////////////////////////////////////////////////////////////

  class TiedostonLukeminenStub(tiedostojenNimet: Buffer[String]) {

    var inputFromFile = Buffer[String]() // kaikki input, paitsi tyhjät rivit
    var nuottiDataRiveina = Buffer[String]()
    var nuottiAlkiot = Array[String]() // splitattuna yllä oleva data
    var nuottiDatanRivinumerot = Buffer[Int]() // syötetiedoston nuottidatarivit muistiin, ei tyhjiä rivejä
    val lyriikkadata = Buffer[String]() // biisin sanat

    var tahtilaji = "4"
    var kappaleenNimi = ""

    val inputhakemistonNimi = "./input_virheita/"
    val inputhakemisto = new File(inputhakemistonNimi)

    var ekaKerta = true
    var tahtilajiOnJoLuettu = false

    var stubMessage = "stubMessagen alustusteksti"
    var stubMessages = Buffer[String]()
    var stubMoneskokerta = 0
    var tiedostonNimi = tiedostojenNimet(0)

    lueTiedosto(tiedostonNimi)

    /////  M E T O D I T ///////////////////////////////////////////////////////////////////////// 

    def lueTiedosto(tiedostonNimi: String): Unit = {

      //  stubMoneskokerta += 1

      this.tiedostonNimi = tiedostonNimi
      this.inputFromFile = Buffer[String]() // pitää nollata, jos tänne tullaan virheidentarkistuksesta
      this.nuottiDataRiveina = Buffer[String]()
      this.nuottiDatanRivinumerot = Buffer[Int]()
      this.nuottiAlkiot = Array[String]()
      val kayttajanValitsemaTiedosto = Source.fromFile(inputhakemistonNimi + tiedostonNimi)
      tahtilajiOnJoLuettu = false

      try {
        for (rivi <- kayttajanValitsemaTiedosto.getLines) {
          this.inputFromFile += rivi.trim
        }
      } finally {
        kayttajanValitsemaTiedosto.close()
      }

      if (this.inputFromFile.size != 0) {
        kasitteleTunnisteet(this.inputFromFile)
        if (nuottiDataRiveina.size == 0) { println("\n\nei nuottidataa, ei tehdä mitään."); System.exit(1) }
        else if (ekaKerta) tarkistaVirheet() // loput virheidentarkistukset do while-loopissa, kutsu rivillä 94
      } else {
        println("\n\ntyhjästä tiedostosta ei voi tehdä nuotteja")
        System.exit(1)
      }
    }

    def tarkistaVirheet() = {

      ekaKerta = false
      var virheitaNolla = true

      do {
        virheitaNolla = true
        tarkistaVirheetForLoop()
      } while (!virheitaNolla)

      def tarkistaVirheetForLoop(): Unit = {
        for (i <- 0 until nuottiDataRiveina.size) {

          val ylimaaraisetValilyonnitPois = nuottiDataRiveina(i).trim().replaceAll(" +", " ");
          val splitattuRivi = ylimaaraisetValilyonnitPois.replaceAll(", ", ",").replaceAll(" ,", ",").replaceAll("< ", "<").replaceAll(" >", ">").replaceAll("<>", "").replaceAll("><", "> <").replaceAll(" -", "-").split(" ")

          for (alkio <- splitattuRivi) {
            if (virheitaNolla) {
              if (alkio == "") {} // ylimääräisiä välilyöntejä ei nuottiAlkiot:hin 

              else if (alkio.head == '<')
                tarkistaSoinnunVirheet(alkio.trim(), i)

              else if (oikeellisuusTesti(alkio) == "") // ei virhettä alkiossa, tarpeeksi infoa nuotin tekemiseen
                nuottiAlkiot = nuottiAlkiot :+ erikoistapauksetNuotinNimessa(alkio) // alkio sellaisenaan tai fixattuna

              else { // virheellinen alkio:
                virheitaNolla = false
                readLine("\n\n syöte '" + alkio + "' on virheellinen: " + oikeellisuusTesti(alkio) +
                  "\n Virhe on rivillä " + (nuottiDatanRivinumerot(i) + 1) +
                  "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")

                lueTiedosto(this.tiedostonNimi); return
              }
            }
          }
        } // end for nuottiDataRiveina
      }

      //nested function:  
      def tarkistaSoinnunVirheet(alkio: String, ind: Int): Unit = {

        //  println(alkio) 

        if (alkio.last != '>') {
          virheitaNolla = false
          readLine("\n\n syöte '" + alkio + "' on virheellinen: soinnun sävelten väliin tulee kirjoittaa pilkku. " +
            "\n Tai jos tarkoitit että sointu loppuu, niin muista laittaa >" +
            "\n\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind) + 1) +
            "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")
          lueTiedosto(this.tiedostonNimi); return
        } else {
          var sointu = alkio.tail.substring(0, alkio.size - 2).split(",")
          for (i <- 0 until sointu.size) {
            if (alkio != "" && oikeellisuusTesti(sointu(i)) == "") {
              sointu(i) = erikoistapauksetNuotinNimessa(sointu(i)) // korvataan soinnun alkiot mahdollisilla fixauksilla
            } else {
              virheitaNolla = false
              //                        readLine("\n\n syöte '" + sointu(i) +"' on virheellinen: " + oikeellisuusTesti(sointu(i)) + 
              //                        "\n Virhe on rivillä " + (nuottiDatanRivinumerot(ind)+1)  +
              //                        "\n Korjaa äsken valitsemaasi tiedostoon ja paina ENTER, kun tiedosto on tallennettu. ")

              // Stub START: alla olevat  4 riviä korvaavat yllä olevat kommentoidut 3                                                            ////    S T U B   H E R E !!!
              this.stubMessage = "löydettiin virhe rivillä " + (nuottiDatanRivinumerot(ind) + 1)
              stubMessages += this.stubMessage
              stubMoneskokerta += 1
              lueTiedosto(tiedostojenNimet(stubMoneskokerta)); return
            }
          }

          if (virheitaNolla) {
            var korjattuAlkio = "<"
            for (aani <- sointu) korjattuAlkio += aani + ","
            korjattuAlkio = korjattuAlkio.substring(0, korjattuAlkio.size - 1) + ">" // ei vikaa pilkkua
            nuottiAlkiot = nuottiAlkiot :+ korjattuAlkio // alkio = <g1,h1>
          }
        } // end else: ei ole kyse koko alkiosta <....>
      } // end tarkistaSoinnunVirheet
    } // end tarkistaVirheet

    def erikoistapauksetNuotinNimessa(alkio: String): String = {
      val alkioPituustietoPois = alkio.filter(_ != '-').filter(_ != '.')
      // case  c2# --> c#2
      if (alkioPituustietoPois.size == 3 && (alkio.tail.contains("#") || alkio.tail.contains("b")) && !alkioPituustietoPois(2).isDigit)
        return "" + alkio(0) + alkio(2) + alkio(1) + alkio.substring(3)
      else if (alkioPituustietoPois == "b#1") // popmuusikot kutsuvat h:ta b:ksi
        return "h#1"
      else if (alkioPituustietoPois == "b#2")
        return "h#2"
      alkio // palautetaan alkio muuttumattomana jos ei tehdä mitään ylläolevista toimenpiteistä
    }

    def kasitteleTunnisteet(inputFromFile: Buffer[String]) = { // tekstisyöterivejä

      var seuraavatrivitLyriikkaan = false
      for (i <- 0 until inputFromFile.size) {
        if (inputFromFile(i).trim.size != 0) {
          var kelvollinenSyoteRivi = inputFromFile(i).replaceAll("\t", "")

          if (kelvollinenSyoteRivi.head == '#') { //  T U N N I S T E E T
            if (kelvollinenSyoteRivi.tail.toLowerCase().trim.contains("sanat")) {
              seuraavatrivitLyriikkaan = true
              // varaudutaan siihen että joku kirjoittaa sanoja jo samalle riville kuin missä tunniste:
              if (kelvollinenSyoteRivi.tail.trim.substring(5).length > 0) {
                lyriikkadata += kelvollinenSyoteRivi.tail.trim.substring(5)
              }
            } else if (seuraavatrivitLyriikkaan == false) kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi, i) // end lyriikat false

          } else if (seuraavatrivitLyriikkaan) { // L Y R I I K K A 
            lyriikkadata += kelvollinenSyoteRivi
          } else { // L O P U T   ELI   N U O T I T      
            nuottiDatanRivinumerot += i
            nuottiDataRiveina += kelvollinenSyoteRivi.toLowerCase()
          }
        } // if   .size != 0 
      }
      //  println("kappaleenNimi: " + kappaleenNimi + ", tahtilaji" + tahtilaji)
    }

    def kasitteleKappaleenNimiJaTahtilaji(kelvollinenSyoteRivi: String, ind: Int) = {

      if (kelvollinenSyoteRivi.tail.toLowerCase().contains("nimi")) {
        kappaleenNimi = kelvollinenSyoteRivi.tail.substring(5, kelvollinenSyoteRivi.tail.size)

      } else if (!tahtilajiOnJoLuettu && "2345678".contains(kelvollinenSyoteRivi(1))) {
        tahtilaji = kelvollinenSyoteRivi(1).toString
        // varaudutaan siihen että joku kirjoittaa nuotteja jo samalle riville kuin missä tahtilaji-tunniste:
        if (kelvollinenSyoteRivi.tail.trim.substring(1) != 0) {
          nuottiDataRiveina += kelvollinenSyoteRivi.tail.trim.substring(1)
          nuottiDatanRivinumerot += ind
        }
        tahtilajiOnJoLuettu = true
      }
    }

    def oikeellisuusTesti(syote: String): String = { // esim. g#1---

      // ALUKSI TUTKITAAN  N U O T T I E N   S Y N T A K SI ,  _EI_ PITUUDET
      val filtteredNote = syote.filter(_ != '-').filter(_ != '.')

      if (filtteredNote == "z") {} // taukojen syntaksi helppo, tehdään pituustesti myöhemmin
      else {
        if (filtteredNote.count(_ == 'z') > 1)
          return "taukojen pituudet merkitään viivoilla, esim puolitauko z--"
        if (!"cdefgahb".contains(filtteredNote.toLowerCase().head.toString()))
          return "nuotin pitää alkaa kirjaimilla c,C, d,D e,E f,F g,G a,A h,H, b tai B" // väärä teksti jos "zz"
        else if (filtteredNote.size == 1)
          return "tarkoititko " + syote + "1 vai " + syote + "2?"
        else if (filtteredNote.size == 2 && (filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")) && !(filtteredNote.tail.contains("1") || filtteredNote.tail.contains("2")))
          return "tarkoititko " + syote + "1 vai " + syote + "2?"
        else if (filtteredNote.tail.contains("#b") || filtteredNote.tail.contains("b#"))
          return "nuotissa on ylennys- ja alennusmerkki"
        else if (!(filtteredNote.tail.contains("1") || filtteredNote.tail.contains("2")))
          return "sallitut oktaavialat ovat 1 ja 2"
        else if (filtteredNote.size == 3 && !(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))
          return "väärä formaatti. Muistathan syntaksin: esim. alennettu e on Eb, ei es"
        else if (filtteredNote.size > 3)
          return "liian pitkä nuotin nimi"
      } // iso else

      //  P I T U U D E T  
      val lkm = syote.count(_ == '-')
      if (lkm > 4)
        return "maksimipituus nuotille on 4, eli viivoja korkeintaan ----"
      else if (lkm == 3 && syote.contains(".")) // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
        return "väärä pituus"
      else if (lkm == 4 && syote.contains(".")) // max pituus 4
        return "pisteellistä kokonuottia ei ole määritelty, tee kokonuotti ja 2 taukoa"
      else if (lkm == 0 && syote.contains(".")) // ei pisteellistä kahdeksasosaa
        return "tämä ohjelma ei osaa käsitellä pisteellistä kahdeksasosaa"

      else ""

    } // end oikeellisuusTesti

  }
}