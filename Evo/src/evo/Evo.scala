package evo {

  import scala.collection.mutable.{ Buffer, ArrayBuffer, Map }

  class Evo {

    /********* Global States ***********/
    // Object of Global Variables for program users to interact with
    object EcoSystem {
      //current time, gets incremented each iteration in simulate
      var worldTime: Int = 0

      //all the species in the ecosystem
      var species = Map[Symbol, Species]()
      //all functions of events
      var functions = Map[Symbol, Function]()
      //all events
      var events = Map[Symbol, EventClass]()
      //all deterministic events
      var deterministicEvents = Map[Symbol, DeterministicEvent]()
      //all random events
      var randomEvents = Map[Symbol, RandomEvent]()
      //all generic events
      var genericEvents = Map[Symbol, GenericEvent]()

      //ADD METHODS
      //add a new species to the ecosystem
      def addSpecies(s: Species) {
        species += (s._name -> s)
      }

      //add a new deterministic event
      def addDeterministicEvent(e: DeterministicEvent) {
        deterministicEvents += (e._name -> e)
        events += (e._name -> e)
      }

      //add a new random event
      def addRandomEvent(e: RandomEvent) {
        randomEvents += (e._name -> e)
        events += (e._name -> e)
      }

      //add a new generic event
      def addGenericEvent(e: GenericEvent) {
        genericEvents += (e._name -> e)
        events += (e._name -> e)
      }

      //GET METHODS
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

      //get a random event
      def getRandomEvent(name: Symbol): RandomEvent = {
        if (randomEvents.contains(name)) randomEvents(name)
        else null
      }

      //get a generic event
      def getGenericEvent(name: Symbol): GenericEvent = {
        if (genericEvents.contains(name)) genericEvents(name)
        else null
      }

      //get a species from a name
      def getSpecies(name: Symbol): Species = {
        if (species.contains(name)) species(name)
        else null
      }

      //util function that returns random number 0.0 to 1.0 inclusive
      def getRandomValue() = {
        var rand = scala.util.Random
        rand.nextDouble()
      }

      //UTIL METHODS
      //returns true if a species exists
      def speciesExists(name: Symbol) = {
        species.contains(name)
      }

      def updatePopulations(i: Int) {
        species.keys.foreach((sp) =>
          species(sp).population(i))
      }

      def multiplyAllPopulationsByRate(d: Double) {
        species.keys.foreach((sp) =>
          species(sp).update(d))
      }

      def multiplyPopulationByRate(s: Symbol, d: Double) {
        s.update(d)
      }

      def killSpecies(s: Symbol) {
        s.population(0)
      }
      
      val DoNothing: Symbol = 'DoNothing

      //SHOW METHODS
      //print out all random events and deterministic events
      def showAllEvents() {
        showRandomEvents()
        showDeterministicEvents()
      }

      //prints out names and probs of all random events
      def showRandomEvents() = {
        println("The random events are as follows:")
        println("----------------------------------")
        randomEvents.keys.foreach((re) =>
          if (randomEvents.contains(re)) {
            println(randomEvents(re)._name + ": Probability of Occurrence is " + randomEvents(re)._probability)
          })
        println("----------------------------------")
        println()
      }

      //prints out names of deterministic events
      def showDeterministicEvents() = {
        println("The deterministic events are as follows:")
        println("----------------------------------")
        deterministicEvents.keys.foreach((re) =>
          if (deterministicEvents.contains(re)) {
            println(deterministicEvents(re)._name)
          })
        println("----------------------------------")
        println()
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

    //
    def predation() {
      var updatedSpecies = Map[Symbol, Long]()
      // loop through all species
      EcoSystem.species.keys.foreach((sp) =>

        // loop through prey of each species
        sp.prey.keys.foreach((pr) => {

          if (EcoSystem.species(pr)._population == 0) {
            println(pr + " is extinct ******")
          } else {
            println(pr + " is not extinct")
            var deathNum = sp.prey(pr) * EcoSystem.species(sp)._population

            //println(sp._population + " " + sp + " ate " + deathNum + " " + pr)

            if (updatedSpecies.contains(pr)) {
              var v = Math.max(updatedSpecies(pr) - deathNum, 0)
              updatedSpecies.put(pr, v)
            } else {
              var v = Math.max(EcoSystem.species(pr)._population - deathNum, 0)
              updatedSpecies.put(pr, v)
            }
          }
        }))

      EcoSystem.species.keys.foreach((sp) =>

        if (updatedSpecies.contains(sp)) {
          sp._population = updatedSpecies(sp)
        })

    }

    /********* Classes ***********/
    //SPECIES
    class Species extends Expression {
      //properties of species
      var _name: Symbol = null
      var _time: Int = 0
      var _population: Long = 0
      var _birthrate: Double = 0.0
      var _deathrate: Double = 0.0
      var _carryingcapacity: Long = Long.MaxValue

      //
      var _traits: ArrayBuffer[Map[String, Double]] = null
      var _traitReference: Map[String, Int] = null
      var _currentTrait: String = null

      //prey of the species
      var prey = Map[Symbol, Long]()
      var preyEvent = Map[Symbol, (Long, String)]()

      //SETTERS
      //setter for the _name property
      def called(n: Symbol) = {
        _name = n
        EcoSystem.addSpecies(this)
        this
      }

      //setter for the _population property
      def of(p: Long) = {
        if (p > _carryingcapacity) { _population = _carryingcapacity }
        else { _population = p }
        this
      }

      //setter for the _carryingcapacity property
      def withCapacity(cc: Long) = {
        _carryingcapacity = cc
        this
      }

      //setter for the _deathrate property
      def withDeathrate(dr: Double) = {
        _deathrate = dr
        this
      }

      //setter for the _birthrate property
      def withBirthrate(br: Double) = {
        _birthrate = br
        this
      }

      //setter for the _time property
      def enterAt(t: Int) = {
        _time = t
        this
      }

      //another setter for the _population property
      def population(p: Long) = {
        of(p)
      }

      //another setter for deathrate property
      def deathrate(dr: Double) = {
        withDeathrate(dr)
      }

      //another setter for birthrate property
      def birthrate(br: Double) = {
        withBirthrate(br)
      }

      //another setter for the _carryingcapacity property
      def capacity(t: Int) = {
        withCapacity(t)
      }

      //another setter for the _time property
      def time(t: Int) = {
        enterAt(t)
      }

      //GETTERS
      def getName() = _name
      def getPopulation() = _population
      def getBirthrate() = _birthrate
      def getDeathrate() = _deathrate
      def getTime() = _time
      def getCarryingCapacity() = _carryingcapacity

      //METHODS
      //show all the data for the a particular species
      def showAll() {
        println("Name: " + _name)
        println("Population: " + _population)
        println("Birth rate: " + _birthrate)
        println("Death rate: " + _deathrate)
        println("Start time: " + _time)
        println("Carrying Capacity: " + _carryingcapacity)
        if (!prey.isEmpty) {
          print("One " + _name + " consumes ")
          var count: Int = 0
          prey.keys.foreach((p) =>
            if (count == prey.size - 1) {
              print(p + ": " + prey(p))
              count = count + 1
            } else {
              print(p + ": " + prey(p) + ", ")
              count = count + 1
            })
          println()
        }
        if (this._traitReference != null) {
          var currentIndex = 0
          this._traitReference.keys.foreach { i =>
            currentIndex = this._traitReference.apply(i)
            println("Trait: " + i)
            var currentMap = this._traits.apply(currentIndex)
            currentMap.keys.foreach { i =>
              print("Phenotype = " + i)
              println(" Occurence = " + currentMap(i))
            }
          }
        }
      }

      //print name and population
      def showNumbers() {
        println(_name + " Population: " + _population)
      }

      //updates the population based on the growth
      def update(t: Int) = population(grow(t))
      //updates population based on percentage
      def update(t: Double) = population(grow(t))

      //grows the population by growth rate for duration time t  
      private def grow(t: Int): Long =
        if (t > 0) (_population + ((_population * _birthrate).toLong) - ((_population * _deathrate).toLong))
        else grow(t - 1)

      //grows the population based on a percentage
      private def grow(t: Double): Long =
        if (t > 0) ((_population * t).toLong)
        else grow(t - 1)

      def setAsPrey(s: Symbol, consumption: Long) {

        if (!EcoSystem.genericEvents.contains(EcoSystem.DoNothing)) {

          new GenericEvent called EcoSystem.DoNothing definedAs new Function(
            Print("Nothing should happen") ::
              End)
        } //        if (!speciesExists(s)) {
        //           println(s + " is extinct *****")
        //        }
        else {
          prey += (s -> consumption)
        }
      }

      def setAsPrey(s: Symbol, consumption: Long, ev: String) {
        if (!EcoSystem.species.contains(s)) {
          println(s + " is extinct *****")
        } else {
          prey += (s -> consumption)
        }
      }

      def setAsPredator(s: Symbol, consumption: Long) {
        if (!EcoSystem.species.contains(s)) {
          println(s + " is extinct *****")
        } else {
          EcoSystem.species(s).prey += (_name -> consumption);
        }
      }

      //
      def addTrait(traitName: String) = {
        if (this._traits == null) {
          //Init traits list and add this map
          this._currentTrait = traitName
          this._traitReference = Map[String, Int](traitName -> 0)
          this._traits = new ArrayBuffer[Map[String, Double]]
          this
          //phenotype calls will now add actual phenotypes to the appropriate Map in _traits
        } else {
          this._currentTrait = traitName
          var newIndex = this._traits.size
          this._traitReference += (traitName -> newIndex)
          this
          //phenotype calls will now add actual phenotypes to the appropriate Map in _traits
        }
      }

      //
      def phenotype(pheno: String, occurence: Double) = {
        //Get index of current trait
        var currentIndex = this._traitReference.apply(this._currentTrait)
        //If there is a map at this index, add to it
        if (this._traits.isDefinedAt(currentIndex)) {
          //May need to create map?
          var tempMap = this._traits.apply(currentIndex)
          tempMap.+=(pheno -> occurence)
          this._traits.update(currentIndex, tempMap)
          this
          //Okay, added phenotype.
        } else {
          //CurrentIndex does not exist so need to create map.
          var newMap = Map[String, Double](pheno -> occurence)
          this._traits.insert(currentIndex, newMap)
          this
          //Added phenotype.
        }
      }

      //
      def showTrait(traitName: String) {
        var currentIndex = this._traitReference.apply(traitName)
        println("Trait: " + traitName)
        var currentMap = this._traits.apply(currentIndex)
        currentMap.keys.foreach { i =>
          print("Phenotype = " + i)
          println(" Occurence = " + currentMap(i))
        }
      }
    }

    //EVENTS:
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
      def definedAs(function: => Function) {
        EcoSystem.functions += (_name -> function)
      }

      //run the event's code if the time is right
      def execute() {
        if (_time == EcoSystem.worldTime) {
          var function = EcoSystem.functions(_name)
          function.get
        }
      }
    }

    class RandomEvent extends EventClass {
      //probability of event happening at every time tick
      var _probability: Double = 0.0

      //get the _probability property
      def getProbability() = _probability

      //set the _probability property
      def withProbability(p: Double): RandomEvent = {
        _probability = p
        this
      }

      //set the _name property
      def called(n: Symbol) = {
        _name = n
        EcoSystem.addRandomEvent(this)
        this
      }

      //implement execute function from EventClass
      def execute() {
        var function = EcoSystem.functions(_name)
        function.get
      }

      //implement at function from EventClass that has no effect
      def at(t: Int) = {
        this
      }

      //add the function to the global map of functions (using call-by-name)
      def definedAs(function: => Function) {
        EcoSystem.functions += (_name -> function)
      }
    }

    class GenericEvent extends EventClass {

      // Setter for event name
      def called(n: Symbol) = {
        _name = n
        EcoSystem.addGenericEvent(this)
        this
      }

      def execute() {
        var function = EcoSystem.functions(_name)
        function.get
      }

      //add the function to the global map of functions (using call-by-name)
      def definedAs(function: => Function) {
        EcoSystem.functions += (_name -> function)
      }

      def at(t: Int) = {
        this
      }
    }

    /********* Implicits ***********/
    //try to convert any Symbol to an event type
    implicit def eventString(name: Symbol): EventClass = {
      var e = EcoSystem.getEvent(name)
      e match {
        case e: DeterministicEvent => EcoSystem.getDeterministicEvent(name)
        case e: RandomEvent        => EcoSystem.getRandomEvent(name)
      }
    }

    //try to convert any Symbol into a Species
    implicit def speciesString(name: Symbol): Species = {
      EcoSystem.getSpecies(name)
    }

    /********* Simulate ***********/
    def simulate(time: Int) = {
      //do time-1 because loop is inclusive  
      
      
      
      for (a <- 0 to time - 1) {
        println("Time Step " + (EcoSystem.worldTime + 1));
        println(" _______________________ ");

        //run every deterministic event
        EcoSystem.deterministicEvents.keys.foreach((ev) =>
          if (EcoSystem.deterministicEvents.contains(ev)) {
            ev.execute()
          })

        //run every random event
        EcoSystem.randomEvents.keys.foreach((ev) => {
          if (EcoSystem.randomEvents.contains(ev)) {
            val p = EcoSystem.randomEvents(ev).getProbability()
            val r = EcoSystem.getRandomValue()
            if (r <= p) {
              ev.execute()
            }
          }
        })

        //show and update every species in the ecosystem
        EcoSystem.species.keys.foreach((sp) =>
          if (EcoSystem.species.contains(sp)) {
            if (sp.getTime() <= EcoSystem.worldTime) {
              sp.update(1) //update the population by one time unit
              sp.showNumbers() //show the updated population of the species
            }
          })

        predation()

        //increment the time
        EcoSystem.worldTime += 1
        println(); println()
      }
    }

    /********* EVO Language ***********/
    //Wrapper class to hold function of a method (uses call-by-name)
    class Function(e: => List[Expression]) {
      def get = e
    }

    //Evo - takes a condition and the expressions to be run if the condition is true
    class IfClass(condition: Boolean)(e: => List[Expression]) extends Expression {
      if (condition) e
    }
    //used to call If without new keyword
    object If {
      def apply(condition: Boolean)(e: => List[Expression]) = new IfClass(condition)(e)
    }

    //Evo - executes the expressions i number of times, for loop
    class StepClass(i: Int)(e: => List[Expression]) extends Expression {
      var counter = 0
      while (counter < i) {
        e
        counter += 1
      }
    }
    //used to call Step without new keyword
    object Step {
      def apply(i: Int)(e: => List[Expression]) = new StepClass(i)(e)
    }

    //Evo - prints the string str
    class PrintClass(str: String) extends Expression {
      println(str)
    }
    //used to print without the new keyword
    object Print {
      def apply(str: String) = new PrintClass(str)
    }

    //Evo - update species population
    class UpdatePopulationClass(species: Symbol, p: Long) extends Expression {
      species population p
    }
    //used to call the method without using new keyword
    object UpdatePopulation {
      def apply(species: Symbol, p: Long) = new UpdatePopulationClass(species, p)
    }

    //Evo - updates all populations to a specific number
    class UpdateAllPopulationsToClass(i: Int) extends Expression {
      EcoSystem.updatePopulations(i)
    }
    object UpdateAllPopulationsTo {
      def apply(i: Int) = new UpdateAllPopulationsToClass(i)
    }

    //Evo - updates all populations by a specific growth percentage
    class UpdateAllPopulationsByClass(d: Double) extends Expression {
      EcoSystem.multiplyAllPopulationsByRate(d)
    }
    object UpdateAllPopulationsBy {
      def apply(d: Double) = new UpdateAllPopulationsByClass(d)
    }

    //Evo - updates a population by a specific growth percentage
    class UpdatePopulationByClass(s: Symbol, d: Double) extends Expression {
      EcoSystem.multiplyPopulationByRate(s, d)
    }
    object UpdatePopulationBy {
      def apply(s: Symbol, d: Double) = new UpdatePopulationByClass(s, d)
    }
    
     //Evo - kills a species
    class KillSpeciesClass(s: Symbol) extends Expression {
      EcoSystem.killSpecies(s)
    }
    object KillSpecies {
      def apply(s: Symbol) = new KillSpeciesClass(s)
    }

    //Evo - cleaner way to show the ecosystem
    def showEcosystem = EcoSystem.showEcosystem()

    def End = Nil

  }

}
  
  /********* tests ***********/
  /*def main(args: Array[String]) = {
    //example of creating Species
    new Species called 'Pig of 1000 birthrate .4 deathrate .3
    new Species called 'Frog of 100 birthrate 0 deathrate 0 at 0 carryingcapacity 5000
    new Species called 'Fly of 1000 birthrate 0 deathrate 0 at 0
    new Species called 'Cricket of 500 birthrate 0 deathrate 0 at 0

    //set food web relations between species 
    'Frog setAsPrey ('Fly, 5)
    'Frog setAsPrey ('Cricket, 2)

    //example of defining deterministic event
    new DeterministicEvent called 'Tornado at 4 definedAs new Function (
      Print("Starting it") ::
      (new Species called 'Pog of 1000 birthrate .4 deathrate .3) ::
      Print("goo") ::
      If(('Pig getPopulation) < 2000) (
           UpdateAllPopulationsTo(5000) ::
           Print("updated all populations to 5000") ::
           End
      ) ::
      If(1 < 2) (
           UpdatePopulation('Pig, 696969) ::
           Print("updated all pigs to 696969") ::
           End
      ) ::
      If(('Pig getPopulation) > ('Frog getPopulation)) (
           UpdateAllPopulationsBy(.2) ::
           Print("updated all growth by .2") ::
           End
      ) ::
      Step(4)(
        Print("ok") ::
        If(('Pig getPopulation) < 2) (
           Print("watermelon") ::
           End
        ) ::
        End
      ) ::
      End
    )

    //example of defining random event
    new RandomEvent called 'Wipeout withProbability .5 definedAs new Function (
      Print("WIPEOUT!!") ::
      End
    )

    //run the ecosystem - print the states before and after
    showEcosystem
    simulate(5)
    showEcosystem

    println("---")
  }

}*/