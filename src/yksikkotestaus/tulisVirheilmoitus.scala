package yksikkotestaus

//import studio2.train._   
// (used to be: (default package)

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Assertions._



@RunWith(classOf[JUnitRunner])
class TrainTest extends FlatSpec with Matchers {            // nyt yritetään etsiä Train-bugi #2 & SittingCar-bugi #1     
    
  
  // #1
  "NuottiPiirturi" should "find non-valid notes" in {
    val nuottejaOikein = Buffer("c1-", "d1-", "f#2")
    val nuottejaVaarin = Buffer("t1-", "d3-", "f")
    

    var virheita = 0 
    for (nuotti <- nuottejaOikein) {
        if("acdefgh".contains(nuotti.head.toString())
            virheita += 1
      assert(virheita == nuottejaOikein.size, "Train failed to add a car")
    }
  }
  
  
}