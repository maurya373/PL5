import scala.collection.mutable.Map

object EcoSim {
  
  class Species {
    var _name: String = null
    var _population: Int = 0
    var _growthrate: Double = 0.0
    var _starttime: Int = 0
    
    def called(n: String) = {
      _name = n
      GlobalVars.addSpecie(this)
      this
    }
    
    def of(i: Int) = {
      _population = i
      this
    }
    
    def growat(x: Double) = {
      _growthrate = x
      this
    }
    
    def startingat(t: Int) = {
      _starttime = t
      this
    }
    
    def show() {
      println("name: " + _name)
      println("population: " + _population)
      println("growthrate: " + _growthrate)
      println("starttime: " + _starttime)
    }
    
    
    def population(x: Int) {
      _population = x
    }
   
    def growthrate(x: Double) {
      _growthrate = x
    }
     
    def starttime(x: Int) {
      _starttime = x
    }
  }
  

  implicit def speciesString(name: String): Species = {
    GlobalVars.doesExist(name)
  }
  
  
  object GlobalVars {
    var species = Map[String, Species]()
    
    def addSpecie(s: Species) {
      species += (s._name -> s)
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
    "Frog" population 10
    "Frog" growthrate .3
    "Frog" starttime 2
    
    "Frog" show
  }
  
}
