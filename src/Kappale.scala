import scala.collection.mutable.Buffer

class Kappale {
  
  var kappale =  Buffer[Viivasto]()
  
  def lisaaViivastoRivi(v: Viivasto){
    this.kappale += v
  }
}