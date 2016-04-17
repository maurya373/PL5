object EcoSim {
  
  class Species {
    var name: String = null
    var population: Int = 0
    var growthrate: Double = 0.0
    var starttime: Int = 0
    
    def called(n: String) = {
      name = n
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
  
  class SpeciesString(name: String) {
    
  }
  implicit def speciesString(name: String) = new SpeciesString(name)
  
  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value
  
  def main(args: Array[String]) = {
    var Frog = new Species called "Frog" of 1000 growat .4 startingat 0
    Frog.show
  }
  
}
