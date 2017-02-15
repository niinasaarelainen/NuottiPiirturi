import scala.collection.mutable.Buffer

class Kappale {
  
//  var kappale =  Buffer[Viivasto]()
  var kappale =  Buffer[Buffer[String]]()
  
  def lisaaViivasto(viivasto: Buffer[String]){
    this.kappale += viivasto
  }
}