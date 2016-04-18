import scala.collection.mutable.Map

object EcoSim {
  
  class Species {
    var name: String = null
    var population: Int = 0
    var growthrate: Double = 0.0
    var starttime: Int = 0
    
    def called(n: String) = {
      name = n
      GlobalVars.addSpecie(this)
      this
    }
    
    def of(i: Int) = {
      population = i
      this
    }
    
    def growat(x: Double) = {
      growthrate = x
      this
    }
    
    def startingat(t: Int) = {
      starttime = t
      this
    }
    
    def show() {
      println("name: " + name)
      println("population: " + population)
      println("growthrate: " + growthrate)
      println("starttime: " + starttime)
    }
  }
  

  implicit def speciesString(name: String): Species = {
    GlobalVars.doesExist(name)
  }
  
  
  object GlobalVars {
    var species = Map[String, Species]()
    
    def addSpecie(s: Species) {
      species += (s.name -> s)
    }
    
    def doesExist(name: String): Species = {
      if (species.contains(name)) species(name)
      else null
    }
  }
  
  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value
  
  def main(args: Array[String]) = {
    new Species called "Frog" of 1000 growat .4 startingat 0
    "Frog" show
    
    //"Frog" population is 10
  }
  
}
