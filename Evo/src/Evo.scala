import collection.mutable.Buffer
import scala.collection.mutable.Map

object Evo {

  /********* Global States ***********/
  // Object of Global Variables for program users to interact with
  object EcoSystem {
    //current time, gets incremented each iteration in simulate
    var worldTime: Int = 0

    //all the species in the ecosystem
    var species = Map[Symbol, Species]()
    //all functions of events
    var functions = Map[Symbol, functionWrapper[_]]()
    //all events
    var events = Map[Symbol, EventClass]()
    //all deterministic events
    var deterministicEvents = Map[Symbol, DeterministicEvent]()

    //add a new species to the ecosystem
    def addSpecies(s: Species) {
      species += (s._name -> s)
    }
    
    //add a new deterministic event
    def addDeterministicEvent(e: DeterministicEvent) {
      deterministicEvents += (e._name -> e)
      events += (e._name -> e)
    }

    //get an event
    def getEvent(n: Symbol): EventClass = {
      if (events.contains(n)) events(n)
      else null
    }

    //get a deterministic event
    def getDeterministicEvent(name: Symbol): DeterministicEvent = {
      if (deterministicEvents.contains(name)) deterministicEvents(name)
      else null
    }
    
    //get a species from a name
    def getSpecies(name: Symbol): Species = {
      if (species.contains(name)) species(name)
      else null
    }
    
    //show the status of the ecosystem
    def showEcosystem() = {
      println("State at beginning of time step: " + worldTime)
      println()
      species.keys.foreach((sp) =>
        if (species.contains(sp)) {
          sp.showAll()
          println("-------------------------------------")
        })
      println()
    }
  }

  /********* Classes ***********/
  class Species {
    //properties of species
    var _name: Symbol = null
    var _time: Int = 0
    var _population: Long = 0
    var _birthrate: Double = 0.0
    var _deathrate: Double = 0.0

    //SETTERS
    //setter for the _name property
    def called(n: Symbol) = {
      _name = n
      EcoSystem.addSpecies(this)
      this
    }

    //setter for the _population property
    def of(p: Long) = {
      _population = p
      this
    }

    //setter for the _deathrate property
    def deathrate(dr: Double) = {
      _deathrate = dr
      this
    }

    //setter for the _birthrate property
    def birthrate(br: Double) = {
      _birthrate = br
      this
    }

    //setter for the _time property
    def at(t: Int) = {
      _time = t
      this
    }

    //another setter for the _population property
    def population(x: Long) {
      _population = x
    }

    //another setter for the _time property
    def time(x: Int) {
      _time = x
    }
    
    //GETTERS
    def getName() = _name
    def getPopulation() = _population
    def getBirthrate() = _birthrate
    def getDeathrate() = _deathrate
    def getTime() = _time
      
    //METHODS
    //show all the data for the a particular species
    def showAll() {
      println("Name: " + _name)
      println("Population: " + _population)
      println("Birth rate: " + _birthrate)
      println("Death rate: " + _deathrate)
      println("Start time: " + _time)
    }

    //print name and population
    def showNumbers() {
      println(_name + " Population: " + _population)
    }

    //updates the population based on the growth
    def update(t: Int) = population(grow(t))

    // Grows the population by growth rate for duration time t  
    private def grow(t: Int): Long =
      if (t > 0) (_population + ((_population * _birthrate).toLong) - ((_population * _deathrate).toLong))
      else grow(t - 1)

  }
  
  //super class of both deterministic and random events
  abstract class EventClass {
    //name of the event
    var _name: Symbol = null
    //time at which the event occurs
    var _time: Int = 0

    //setter of _name property
    def called(n: Symbol): EventClass
    //setter of _time property
    def at(t: Int): EventClass
    //called every iteration of time, function is only called at _time
    def execute()
    //print method
    def show() {
      println(_name + " occurs at " + _time)
    }
  }

  //all statements in an event inherit are labeled 'Expression' by this trait
  trait Expression

  class DeterministicEvent extends EventClass {
    //add the event to the global list when creating it
    def called(n: Symbol) = {
      _name = n
      EcoSystem.addDeterministicEvent(this)
      this
    }

    //setter for the _time property
    def at(t: Int) = {
      _time = t
      this
    }

    //add the function to the global map of functions (using call-by-name)
    def definedAs(function: => FUNCTION) {
      EcoSystem.functions += (_name -> new functionWrapper(function))
    }

    //run the event's code if the time is right
    def execute() {
      if (_time == EcoSystem.worldTime) {
        var function = EcoSystem.functions(_name)
        function.get
      }
    }
  }
  
  /********* Implicits ***********/
  //try to convert any Symbol to an event type
  implicit def eventString(name: Symbol): EventClass = {
    var e = EcoSystem.getEvent(name)
    e match {
      case e: DeterministicEvent => EcoSystem.getDeterministicEvent(name)
      //case e: RandomEvent        => EcoSystem.getRandomEvent(name)
    }
  }
  
  implicit def speciesString(name: Symbol): Species = {
    EcoSystem.getSpecies(name)
  }

  /********* Simulate ***********/
  def simulate(time: Int) = {
    //do time-1 because loop is inclusive  
    for (a <- 0 to time - 1) {
      println("Time Step " + (EcoSystem.worldTime + 1));
      println(" _______________________ ");

      //run every event
      EcoSystem.deterministicEvents.keys.foreach((ev) =>
        if (EcoSystem.deterministicEvents.contains(ev)) {
          ev.execute()
        })
      
      //show and update every species in the ecosystem
      EcoSystem.species.keys.foreach((sp) =>
        if (EcoSystem.species.contains(sp)) {
          if (sp.getTime() <= EcoSystem.worldTime) {
            sp.update(1) //update the population by one time unit
          }
          sp.showNumbers()
        })

      //increment the time
      EcoSystem.worldTime += 1
      println()
      println()
    }
  }

  /********* EVO Language ***********/
  //Wrapper class to hold function of a method (uses call-by-name)
  class functionWrapper[Expression](body: => Expression) {
    //use get to run the function code
    def get = body
  }

  //class to create a  new function
  class FUNCTION {
    //for every expression written, add it to the buffer
    val expressions = Buffer[Expression]()
    trait Expression {
      expressions += this
    }
  }

  //Evo - Utility class that does nothing
  class EMPTY extends Expression {}
  def empty = new EMPTY()

  //Evo - takes a condition and the expressions to be run if the condition is true
  class IF(condition: Boolean, expressions: => Expression) extends Expression {
    if (condition) expressions
  }

  //Evo - executes the expressions i number of times, for loop
  class STEP(i: Int, expressions: => Expression) extends Expression {
    var counter = 0
    while (counter < i) {
      expressions
      counter += 1
    }
    def iteration = counter
  }

  //Evo - prints the string str
  class PRINT(str: String) extends Expression {
    println(str)
  }
  
  //Evo - update species population
  class UPDATE_POPULATION(species: Symbol, p: Long) extends Expression {
    species population p
  }

  /********* tests ***********/
  def main(args: Array[String]) = {
    new Species called 'Pig of 1000 birthrate .4 deathrate .3
    
    new DeterministicEvent called 'Tornado at 4 definedAs (new FUNCTION {
      new PRINT("12")
      new IF(('Pig getPopulation) < 2, (
        new PRINT("FRAIJ")))
      new IF(1 < 2, (
        new UPDATE_POPULATION('Pig, 696969)  
      ))
      new STEP(3, (
        new PRINT("FARES")))
    })

    simulate(5)

    println("---")
  }

}