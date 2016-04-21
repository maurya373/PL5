import scala.collection.mutable.Map

object EcoSim {

  // probably switch to a double?
  def simulate(time: Int) = {

    for (a <- 1 to time) {

      // all simulation code
      println("Year " + a + " Populations");
      println(" ---------- ");
      println();

      GlobalVars.species.keys.foreach((sp) =>
        if (GlobalVars.species.contains(sp)) {
          sp.update(1) //update the population by one time unit
          println(sp.showShort())
        })

      println();

    }
  }

  //lets have a way for global events to impact everything
  //like bad weather kills an entire species 
  //so events?

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

    def showAll() {
      println("Name: " + _name)
      println("Population: " + _population)
      println("Growth rate: " + _growthrate)
      println("Start time: " + _starttime)
    }

    def showShort() {
      println(_name + ": " + _population)
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

    //updates the population based on the growth
    def update(t: Int) = population(grow(t))
    
    //grows the population by growth rate for duration time t  
    private def grow(t: Int): Int = 
      if (t > 0) (_population + (_population * _growthrate)).toInt
      else grow(t - 1)
    
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
