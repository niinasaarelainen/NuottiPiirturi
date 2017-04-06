import scala.collection.mutable.Buffer

class Kappale {
  
  var kappale =  Buffer[Buffer[String]]()
  
  
  def lisaaKappaleenNimi(kappaleenNimi: String){
      var apuPuskuri = Buffer[String]()
      apuPuskuri += ""
      apuPuskuri += ""
      apuPuskuri += "\t\t\t\t" + kappaleenNimi
      this.kappale += apuPuskuri    
  }
  
  
  def lisaaViivasto(viivasto: Buffer[String]){
      this.kappale += viivasto
  }
  
  
  def printtaaRuudulleIlmanAjastusta() = {
      println(); println()
      for {
            viivasto <- kappale     // kappaleen nimikin on "viivasto"
            rivi <- viivasto
      } println(rivi)
  }
}