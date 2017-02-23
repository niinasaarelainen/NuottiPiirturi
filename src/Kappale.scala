import scala.collection.mutable.Buffer

class Kappale {
  
//  var kappale =  Buffer[Viivasto]()
  var kappale =  Buffer[Buffer[String]]()
  
  def lisaaKappaleenNimi(kappaleenNimi: String){
    var apuPuskuri = Buffer[String]()
    apuPuskuri += "\n                 " + kappaleenNimi +"\n"
    this.kappale += apuPuskuri
    
    println("@Kappale: ")
    for(viivasto <- this.kappale)
     for(rivi <- viivasto)
      println(rivi)
  }
  
  def lisaaViivasto(viivasto: Buffer[String]){
    this.kappale += viivasto
  }
}