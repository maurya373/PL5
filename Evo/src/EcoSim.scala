import scala.collection.mutable.Map

object EcoSim {

  var rand = scala.util.Random

  
  // ******************** List of Defs Users can call ********************
  
  // probably want to switch to a double eventually
  def simulate(time: Int) = {
    //do time-1 because loop is inclusive  
    for (a <- 0 to time - 1) {
      
      // all simulation code
      println("Time Step " + (GlobalVars.simulation_Time + 1) + " Data");
      println("-----------");
      
      GlobalVars.deterministicEvents.keys.foreach((ev) =>
        if (GlobalVars.deterministicEvents.contains(ev)) {
          GlobalVars.deterministicEvents(ev).runAll()
        })
        
      GlobalVars.randomEvents.keys.foreach((ev) => {
        if (GlobalVars.randomEvents.contains(ev)) {
          val r = rand.nextDouble()
          if (r < GlobalVars.randomEvents(ev).getProbability()) {
            GlobalVars.randomEvents(ev).runAll()
          }
        }
      })

      GlobalVars.species.keys.foreach((sp) =>
        if (GlobalVars.species.contains(sp)) {
          if (sp._starttime <= GlobalVars.simulation_Time){
            sp.update(1) //update the population by one time unit
            sp.showNumbers()
          }
        })

        
      GlobalVars.simulation_Time += 1
      println();

    }
  }
  
  //print state of the entire ecosystem, i.e. all species
  def showEcosystem() = {
    println("State at beginning of time step: " + GlobalVars.simulation_Time)
    println("-------------------------------------")
    GlobalVars.species.keys.foreach((sp) =>
        if (GlobalVars.species.contains(sp)) {
          sp.showAll()
          println("--------------------------")
        })
    println()
  }
  
  //print out all random events and deterministic events
  def showAllEvents()
  {
    showRandomEvents()
    showDeterministicEvents()
  }
  
  //returns true if a species exists
  def speciesExists(name: String) = {
  {
    GlobalVars.species.contains(name)
  }}
  
  //prints out names and probs of all random events
  def showRandomEvents() = {
    println("The random events are as follows:")
    println("----------------------------------")
    GlobalVars.randomEvents.keys.foreach((re) => 
      if (GlobalVars.randomEvents.contains(re)) {
        println(GlobalVars.randomEvents(re)._name + ": Probability of Occurrence is " + GlobalVars.randomEvents(re)._probability)
      }
      )
    println("----------------------------------")
    println()
  }
  
    //prints out names of deterministic events
    def showDeterministicEvents() = {
    println("The deterministic events are as follows:")
    println("----------------------------------")
    GlobalVars.deterministicEvents.keys.foreach((re) => 
      if (GlobalVars.deterministicEvents.contains(re)) {
        println(GlobalVars.deterministicEvents(re)._name)
      }
      )
    println("----------------------------------")
    println()
  }
  
  def updatePopulations(i: Int) {
      GlobalVars.species.keys.foreach((sp) =>
        GlobalVars.species(sp).update(i))
  }
  
  def multiplyPopulationByRate(d: Double) {
      GlobalVars.species.keys.foreach((sp) =>
        GlobalVars.species(sp).update(d))
  }
  
  
  
  // *************************** Internal Code ******************************

  //lets have a way for global events to impact everything
  //like bad weather kills an entire species 
  //so events?

  class Species {

    // private vars for Species
    var _name: String = null
    var _population: Long = 0
    var _birthrate: Double = 0.0
    var _deathrate: Double = 0.0
    var _carryingcapacity: Long = Long.MaxValue
    var _starttime: Int = 0
    
    var prey = Map[String, Int]()

    // Setter for species name
    def called(n: String) = {
      _name = n
      GlobalVars.addSpecies(this)
      this
    }

    // Setter for species population
    def of(i: Int) = {
      if (i > _carryingcapacity) {
        _population = _carryingcapacity
      }
      else {
       _population = i
      }
      this
    }
    
    // Setter for species growth rate
    def deathrate(x: Double) = {
      _deathrate = x
      this
    }
    
    //Setter for species carrying capacity
    def carryingcapacity(x: Long) = {
      _carryingcapacity = x
      this
    }

    // Setter for species growth rate
    def birthrate(x: Double) = {
      _birthrate = x
      this
    }

    // Setter for species start time
    def startingat(t: Int) = {
      _starttime = t
      this
    }

    // Show all data for the a particular species
    def showAll() {
      println("Name: " + _name)
      println("Population: " + _population)
      println("Birth rate: " + _birthrate)
      println("Death rate: " + _deathrate)
      println("Start time: " + _starttime)
      println("Carrying Capacity: " + _carryingcapacity)
      if (!prey.isEmpty)
      {
        print("One "+ _name + " consumes ")
        prey.keys.foreach((p) => print(p + ": " + prey(p) + ", "))
        println()
      }
    }

    // print name and population
    def showNumbers() {
      println(_name + " Population: " + _population)
    }

    // Setter for species population
    def population(x: Long) {
      //println("setting " + _name + " population to " + x)
      if (x > _carryingcapacity)
      {
        _population = _carryingcapacity
      }
      else
      {
       _population = x 
      }
    }
    
    def population() : Long = {
      this._population
    }

    // Setter for species start time
    def starttime(x: Int) {
      _starttime = x
    }

    // Updates the population based on the growth
    def update(t: Int) = population(grow(t))
    
    def update(t: Double) = population(grow(t))

    // Grows the population by growth rate for duration time t  
    private def grow(t: Int): Long =
      if (t > 0) (_population + ((_population * _birthrate).toLong) - ((_population * _deathrate).toLong))
      else grow(t - 1)
      
    private def grow(t: Double): Long =
      if (t > 0) ((_population * t).toLong)
      else grow(t - 1)

    
    def setAsPrey(s: String, consumption: Int) {
        if (!speciesExists(s)) {
           println(s + " is extinct *****")
        }
        else {
           prey += (s -> consumption)
        }
    }
    
    def setAsPredator(s: String, consumption: Int) {
        if (!speciesExists(s)) {
           println(s + " is extinct *****")
        }
        else {
           GlobalVars.species(s).prey += (_name -> consumption);
        }
    }
      
      
  }

  implicit def speciesString(name: String): Species = {
    GlobalVars.getSpecies(name)
  }

  implicit def eventString(name: String):Event = {
    var e = GlobalVars.getEvent(name)
    e match { 
      case e: DeterministicEvent => GlobalVars.getDeterministicEvent(name)
      case e: RandomEvent => GlobalVars.getRandomEvent(name)
    }
  }
  

  abstract class Event {
    
    var _name: String = null
    var _time: Int = 0
    
    // list of commands for this event
    var _statements: () => Unit = _
    
    def called(n: String): Event
    
    def show() {
      println(_name + " occurs at " + _time)
    }
    

    def execute() {
      _statements.apply()
    }
    
    def define(statements: Function0[Unit]) = {
      _statements = statements
    }
    
  }
  
  class DeterministicEvent extends Event {

    // Setter for event name
    def called(n: String) = {
      _name = n
      GlobalVars.addDeterministicEvent(this)
      this
    }

    def occursAtTime(t: Int) = {
      _time = t
      this
    }
    
    def runAll() {
      if (_time == GlobalVars.simulation_Time) {
        println("************** "+ _name + " occurred **************")
        execute()
      }
    }
  }
  
  class RandomEvent extends Event {
    var _probability: Double = 0.0
    
    def getProbability(): Double = {
      _probability
    }
    
    def withProbability(p: Double): RandomEvent = {
      _probability = p
      this
    }
    
    def runAll() {
      println("************** "+ _name + " occurred **************")
      execute()
    }
    
    // Setter for event name
    def called(n: String) = {
      _name = n
      GlobalVars.addRandomEvent(this)
      this
    }    
    
  }

  // Object of Global Variables for program users to interact with
  object GlobalVars {

    var simulation_Time: Int = 0
    var end_of_world: Int = 0
    
    var species = Map[String, Species]()
    var events = Map[String, Event]()
    
    var deterministicEvents = Map[String, DeterministicEvent]()
    var randomEvents = Map[String, RandomEvent]()
    
    def addSpecies(s: Species) {
      species += (s._name -> s)
    }

    def addDeterministicEvent(e: DeterministicEvent) {
      deterministicEvents += (e._name -> e)
      events += (e._name -> e)
    }
    
    def addRandomEvent(e: RandomEvent) {
      randomEvents += (e._name -> e)
      events += (e._name -> e)
    }

    def getSpecies(name: String): Species = {
      if (species.contains(name)) species(name)
      else null
    }

    def getDeterministicEvent(name: String): DeterministicEvent = {
      if (deterministicEvents.contains(name)) deterministicEvents(name)
      else null
    }
    
    def getRandomEvent(name: String): RandomEvent = {
      if (randomEvents.contains(name)) randomEvents(name)
      else null
    }
    
    def getEvent(n: String): Event = {
      if (events.contains(n)) events(n)
      else null
    }

  }


  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value
  
  def main(args: Array[String]) = {
    
    new Species called "Frog" of 100 birthrate 1 deathrate 0.5 startingat 0 carryingcapacity 5000
    new Species called "Fly" of 1000 birthrate 1 deathrate 0.5 startingat 0
    new Species called "Cricket" of 1000 birthrate 1 deathrate 0.5 startingat 0
    
    "Frog" setAsPrey("Fly", 10)
    "Frog" setAsPrey("Cricket", 10)
    
    new DeterministicEvent called "Tornado" occursAtTime 1
    "Tornado" define (() => {
      "Frog" population 10
      "Fly" population 10
      "Frog" birthrate 1
      new Species called "Jans" of 1000 birthrate 2 deathrate 0.5 startingat 2
    })
    
    new RandomEvent called "Earthquake" withProbability .1 define (() => {
      multiplyPopulationByRate(0.2)
      if (speciesExists("Jans")) {
        "Jans" population 1000000
      }
      "Fly" population 5000000
      new Species called "Tyler" of 1 birthrate 1 deathrate 0 startingat 10
    })
    

    showEcosystem()
    simulate(7)
    
    if(("Jans" population) <  ("Fly" population)){
      "Frog" population 5000
    }
    else{
      "Frog" population 6000
    }
    
    while(("Jans" population) > 0){
      var newPop = ("Jans" population)-1
      "Jans" population newPop
    }
    
    showEcosystem()
  }

}