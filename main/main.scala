  import scala.io.Source

  
object test extends App{
  
  
 // val input= "c1---- d1---- e1---- f1-- g1-- d1#- e1#- c1- c1- c1--- c2-"
  val kokonuotteja = "d1---- e1---- d1-- e1-- f1--- f- h1--- h1-"
  val pisteellisia= "c1--- d1#- c1#--- d1#- e1#- c1- c1- d1b-- e1- c1- e1- f1- g1b--- e1- f1- g1----"
  val input= "c1-- d1#- e1#- c1- c1- d1b-- e1- c1- e1- f1- g1b--- e1- f1- g1----"
  val kahdeksasosia = "g1 a1 g1 f1 e1- c1 c1 g1# a1# g1 f1 e1 c1- e1"
  val taukoja= "z---- z-----"
  val taukoja2= "g1- g1- z d2- e2- e2- d2-- c2- c2- h1- h1- a1- a1- z z c1- d1- e1- c1- c1- d1- e1- c1- e1- f1- g1-- e1- f1- g1-- g2- g2- f2-- e2-- d2-- c2-- c2-- g2--- f1- f1---- e1--- g1- c1- g1--- g1- g1- d2- d2- g1- g1- d2- d2- g1- g1- d2- d2- "
  val tahtilaji = 4     //(yleisin 4/4)
  val lyrics = "Uk-ko Noo-a uk-ko Noo-a"
  
  
  val sointuTest = "g1 a2 <g1,a2> h2 <c2,e2,c2>"
  val splitattu = sointuTest.split(" ") 
 
      
  
//  val piirturi = new nuottiPiirturi(inputFromFile, tahtilaji)
  val piirturi = new NuottiPiirturi(kokonuotteja, tahtilaji)
 //  val piirturi = new nuottiPiirturi(taukoja, tahtilaji)
//  val piirturi2 = new nuottiPiirturi(kahdeksasosia)
//  val valssi = new nuottiPiirturi(kahdeksasosia, 3)     // tahtiviivat huonosti
//    val piirturiTiedostosta = new nuottiPiirturi(inputFromFile, tahtilaji, lyricsNew)

  
  }  