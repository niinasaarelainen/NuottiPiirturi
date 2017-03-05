import scala.collection.mutable.Buffer

class Kappale {
  
//  var kappale =  Buffer[Viivasto]()
  var kappale =  Buffer[Buffer[String]]()
  
  
  def lisaaKappaleenNimi(kappaleenNimi: String){
    var apuPuskuri = Buffer[String]()
    apuPuskuri += "\n                                        " + kappaleenNimi
    this.kappale += apuPuskuri    
  }
  
  
  def lisaaViivasto(viivasto: Buffer[String]){
    this.kappale += viivasto
  }
  
  
  def printtaaRuudulleIlmanAjastusta() = {
    for {
          viivasto <- kappale     // kappaleen nimikin on "viivasto"
          rivi <- viivasto
      } println(rivi)
  }
}