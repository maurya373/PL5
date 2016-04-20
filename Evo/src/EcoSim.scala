import scala.collection.mutable.Map

object EcoSim {
  
  // probably switch to a double?
  def simulate(time: Int) = {
    
    for(a <- 1 to time) {
      
        // all simulation code
        println("Year " + a + " Populations");
        println(" ---------- ");
        println();
        
        GlobalVars.species.keys.foreach((sp) => 
          if (GlobalVars.species.contains(sp)) {
            var pop = sp._population
            var gr = sp._growthrate
            var newpop = pop + (pop*gr)
            sp.population(newpop)
            println(sp._name + " : " + sp._population) })
            
        println();
        
    }
  }
  
  //lets have a way for global events to impact everything
  //like bad weather kills an entire species 
  //so events?
  
  class Species {
    var _name: String = null
    var _population: Double = 0.0
    var _growthrate: Double = 0.0
    var _starttime: Int = 0
    
    def called(n: String) = {
      _name = n
      GlobalVars.addSpecie(this)
      this
    }
    
    def of(i: Double) = {
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
    
    
    def population(x: Double) {
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
//    "Frog" population 10
    "Frog" growthrate .3
    "Frog" starttime 2
    "Frog" show
    
    new Species called "Fly" of 5000000 growat .1 startingat 0
    "Fly" show
    
    simulate(3)
    
  }
  
}
