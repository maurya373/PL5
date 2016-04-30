import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

object EcoSim {

  var rand = scala.util.Random
  
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
        
      GlobalVars.genericEvents.keys.foreach((ev) =>
        if (GlobalVars.genericEvents.contains(ev)) {
          GlobalVars.genericEvents(ev).runAll()
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

      predation()
        
      GlobalVars.simulation_Time += 1
      println();

    }  
  }
  

  def predation() {
    
    var updatedSpecies = Map[String, Long]()
    // loop through all species
    GlobalVars.species.keys.foreach((sp) => 
    
      // loop through prey of each species
       sp.prey.keys.foreach((pr) => {
         
         if (GlobalVars.species(pr)._population == 0)
         {
           println(pr + " is extinct ******")
         }
         else
         {
           println(pr + " is not extinct")
           var deathNum = sp.prey(pr) * GlobalVars.species(sp)._population
         
           //println(sp._population + " " + sp + " ate " + deathNum + " " + pr)
           
           if (updatedSpecies.contains(pr)) {             
             var v = Math.max(updatedSpecies(pr) - deathNum, 0)
             updatedSpecies.put(pr, v)
           }
           else {
             var v = Math.max(GlobalVars.species(pr)._population-deathNum, 0)
             updatedSpecies.put(pr, v)
           }   
         }
       }
       )
    ) 
    
    GlobalVars.species.keys.foreach((sp) => 
       
       if (updatedSpecies.contains(sp)) {  
          sp._population = updatedSpecies(sp)   
       }
    )
    
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
    GlobalVars.species.contains(name)
  }
  
  
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
  
  
  def testTraits(currentSpecies : Species){
      var currentTrait : String = null
      var accumulator : Double = 0.0
      var phenoMap : Map[String, Double] = null
      if(currentSpecies._traitReference != null){
        currentSpecies._traitReference.keys.foreach{ i =>
          currentTrait = i
          accumulator = 0.0
          var phenoMap = currentSpecies._traits(currentSpecies._traitReference(i))
          phenoMap.keys.foreach{ j =>
            accumulator += phenoMap(j)
          }
          if(accumulator != 1.0){
            println("\nWARNING\n"+"Occurences of trait \""+currentTrait+"\" do not sum to 1.") 
          }
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
      case e: GenericEvent => GlobalVars.getGenericEvent(name)
    }
  }
  
  object GlobalVars {

    var simulation_Time: Int = 0
    var end_of_world: Int = 0
    
    var DoNothing: String = "DoNothing"
    
    var species = Map[String, Species]()
    var events = Map[String, Event]()
    
    var deterministicEvents = Map[String, DeterministicEvent]()
    var randomEvents = Map[String, RandomEvent]()
    var genericEvents = Map[String, GenericEvent]()
    
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
    
    def addGenericEvent(e: GenericEvent) {
      genericEvents += (e._name -> e)
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
    
    def getGenericEvent(name: String): GenericEvent = {
      if (genericEvents.contains(name)) genericEvents(name)
      else null
    }
    
    def getEvent(n: String): Event = {
      if (events.contains(n)) events(n)
      else null
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
  
class GenericEvent extends Event {
    
    // Setter for event name
    def called(n: String) = {
      _name = n
      GlobalVars.addGenericEvent(this)
      this
    } 
    
    def runAll() {
      println("************** "+ _name + " occurred **************")
      execute()
    }   
}

class Species {

    // private vars for Species
    var _name: String = null
    var _population: Long = 0
    var _birthrate: Double = 0.0
    var _deathrate: Double = 0.0
    var _carryingcapacity: Long = Long.MaxValue
    var _starttime: Int = 0
    var _traits : ArrayBuffer[Map[String, Double]] = null
    var _traitReference : Map[String, Int] = null 
    var _currentTrait : String = null
    
    var prey = Map[String, Long]()
    var preyEvent = Map[String, (Long, String)]()

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
        var count: Int = 0
        prey.keys.foreach((p) =>
            if (count == prey.size-1) {
              print(p + ": " + prey(p))
              count = count + 1
            }
            else {
              print(p + ": " + prey(p) + ", ")
              count = count + 1
            })
        println()
      }
      if(this._traitReference != null){
        var currentIndex = 0
        this._traitReference.keys.foreach{ i =>
          currentIndex = this._traitReference.apply(i)
          println("Trait: "+ i)
          var currentMap = this._traits.apply(currentIndex)
          currentMap.keys.foreach{ i =>  
            print( "Phenotype = " + i )
            println(" Occurence = " + currentMap(i) )
          }
        }
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

    
    def setAsPrey(s: String, consumption: Long) {
        
        if (!GlobalVars.genericEvents.contains(GlobalVars.DoNothing)) {
          
          new GenericEvent called GlobalVars.DoNothing define (() => {
            println("Nothing should happen")
          })
        }
        
//        if (!speciesExists(s)) {
//           println(s + " is extinct *****")
//        }
        else {
           prey += (s -> consumption)
        }
    }
    
    def setAsPrey(s: String, consumption: Long, ev: String) {
        if (!EcoSim.speciesExists(s)) {
           println(s + " is extinct *****")
        }
        else {
           prey += (s -> consumption)
        }
    }
    
    def setAsPredator(s: String, consumption: Long) {
        if (!EcoSim.speciesExists(s)) {
           println(s + " is extinct *****")
        }
        else {
           GlobalVars.species(s).prey += (_name -> consumption);
        }
    }
    
    
      
    def addTrait(traitName : String)={
      if(this._traits == null){
        //Init traits list and add this map
        this._currentTrait = traitName
        this._traitReference = Map[String,Int](traitName -> 0)
        this._traits = new ArrayBuffer[Map[String, Double]]
        this
        //phenotype calls will now add actual phenotypes to the appropriate Map in _traits
      }
      else{
        this._currentTrait = traitName
        var newIndex = this._traits.size
        this._traitReference += (traitName -> newIndex)
        this
        //phenotype calls will now add actual phenotypes to the appropriate Map in _traits
      }
    }
    
    def phenotype(pheno : String, occurence : Double)={
      //Get index of current trait
      var currentIndex = this._traitReference.apply(this._currentTrait)
      //If there is a map at this index, add to it
      if(this._traits.isDefinedAt(currentIndex)){
        //May need to create map?
        var tempMap = this._traits.apply(currentIndex)
        tempMap.+=(pheno -> occurence)
        this._traits.update(currentIndex, tempMap)
        this
        //Okay, added phenotype.
      }
      else{
        //CurrentIndex does not exist so need to create map.
        var newMap = Map[String, Double](pheno -> occurence)
        this._traits.insert(currentIndex, newMap)
        this
        //Added phenotype.
      }
    }
    
    def showTrait(traitName: String){
      var currentIndex = this._traitReference.apply(traitName)
      println("Trait: "+ traitName)
      var currentMap = this._traits.apply(currentIndex)
      currentMap.keys.foreach{ i =>  
        print( "Phenotype = " + i )
        println(" Occurence = " + currentMap(i) )}
    }
}



  

  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value
  
  def main(args: Array[String]) = {
    
    new Species called "Frog" of 100 birthrate 0 deathrate 0 startingat 0 carryingcapacity 5000
    new Species called "Fly" of 1000 birthrate 0 deathrate 0 startingat 0
    new Species called "Cricket" of 500 birthrate 0 deathrate 0 startingat 0
    new Species called "Jans" of 750 birthrate 0.1 deathrate 0.1 startingat 0
    
    showEcosystem()
    simulate(7)
    
    println("\n\n\n\n-----------------\nTESTING TRAITS\n------------\n\n\n")
    
    "Jans" addTrait "Eye Color" phenotype("Blue",0.5) phenotype("Brown", 0.5)
    "Jans" addTrait "Height" phenotype("Short", 0.01) phenotype("Tall", 0.99)
    "Jans" addTrait "Hair" phenotype("Ponytail", 0.5)
    
    "Jans" showTrait "Eye Color"
    "Jans" showTrait "Height"
    
    GlobalVars.species.keys.foreach{ i =>
      testTraits(GlobalVars.species(i))
    }
    
    println("\n\n\n\n-----------------\nTESTING TRAITS\n------------\n\n\n")
    println("\n\nSimulation over\n\n")    
    
    if(("Jans" population) <  ("Fly" population)){
      "Frog" population 5000
      println("Jans less than Fly")
    }
    else{
      "Frog" population 6000
      println("Jans NOT less than Fly")
    }

    "Frog" setAsPrey("Fly", 5)
    "Frog" setAsPrey("Cricket", 2)
    
    showEcosystem()
    simulate(5)
    showEcosystem()
    
  }
}



