package evo {
import scala.collection.mutable.{ Buffer, ArrayBuffer, Map }
import scala.math.abs
import scala.util.{Try, Success, Failure}
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

    //species eat each other
    def predation() {
      var updatedSpecies = Map[Symbol, Double]()
      // loop through all species
      EcoSystem.species.keys.foreach((sp) =>

        // loop through prey of each species

        sp.preyEvent.keys.foreach((pr) => {

          // if species is extinct run generic event
          if (EcoSystem.species(pr)._population == 0) {

            EcoSystem.species(sp).preyEvent(pr)._2.execute();
          } else {

            var deathNum = (sp.preyEvent(pr)._1 * EcoSystem.species(sp)._population)

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

    def reproduction() {

      // iterate through species
      EcoSystem.species.keys.foreach { (sp) =>
        if (sp.getTime() <= EcoSystem.worldTime) {

          //Get population of current species
          var currentPop: Double = sp.getPopulation()
          var newPop: Double = currentPop
          var phenoData: (Double, Double, Double) = null
          var sum: Double = 0.0
          var newTypeProportion: Double = 0.0
          //If this species has traits
          if (sp._traitReference != null) {
            //For each trait
            sp._traitReference.keys.foreach { (spTrait) =>
              sum = 0
              //For each phenotype
              sp._traits(sp._traitReference(spTrait)).keys.foreach { phenotype =>
                //Modify population to population of this trait
                phenoData = sp._traits(sp._traitReference(spTrait)).apply(phenotype)
                //phenoData tuple is of the form (Percentage of population that has it, Birthrate, Deathrate)
                newPop += ((currentPop * phenoData._1) * (phenoData._2 - phenoData._3))
                sum += phenoData._1 * (phenoData._2 - phenoData._3)
              }

              sp._traits(sp._traitReference(spTrait)).keys.foreach { phenotype =>
                phenoData = sp._traits(sp._traitReference(spTrait)).apply(phenotype)
                
                newTypeProportion = (phenoData._1 * (1 + (phenoData._2 - phenoData._3))) / (1 + sum)
                sp._traits(sp._traitReference(spTrait))(phenotype) = (newTypeProportion, phenoData._2, phenoData._3)
              }
            }
          }
          sp.population(newPop)
          sp.showNumbers()
        }
      }

    }
    
    def addMutations(): ExpressionResult = {
      EcoSystem.species.keys.foreach((sp) =>
        if (sp.getTime() <= EcoSystem.worldTime)
        {
          sp.addMutation()
        }
      )
      new ExpressionResult()
    }
    
    

    def testTraits(currentSpecies: Species) {
      var currentTrait: Symbol = null
      var accumulator: Double = 0.0
      var phenoMap: Map[Symbol, (Double, Double, Double)] = null
      if (currentSpecies._traitReference != null) {
        currentSpecies._traitReference.keys.foreach { i =>
          currentTrait = i
          accumulator = 0.0
          var phenoMap = currentSpecies._traits(currentSpecies._traitReference(i))
          phenoMap.keys.foreach { j =>
            accumulator += phenoMap(j)._1
          }
          var epsilon: Double = 1.0
          if (!(~=(accumulator, 1.0, 0.0001))) {
            println("\n\n-------------------------------------\n"+"WARNING")
            println("Proportions of trait " + currentTrait + " in species " + currentSpecies._name + " do not sum to 1.")
            println("Accumulator "+abs(accumulator-1))
            println("-------------------------------------\n\n")
          }
        }
      }
    }
    
    def ~=(x: Double, y: Double, precision: Double) = {
    	if ((x - y).abs < precision) true else false
    }

    /********* Classes ***********/
    //SPECIES
    class Species extends Expression {

      //properties of species
      var _name: Symbol = null
      var _time: Int = 0
      var _population: Double = 0

      var _carryingcapacity: Long = Long.MaxValue
      
      //probability od a mutation occurring at any time step
      var _probMutation : Double = 0.0
      //proportion of population affected during mutations
      var _mutationProportion : Double = 0.0
      var _mutationNum : Long = 1

      //Trait name and reference index in _traits, for example:
      //"Eye Color" -> 0
      var _traitReference: Map[Symbol, Int] = null
      //Each map corresponds to a certain trait and has it's phenotypes and birthrate deathrate
      //blue -> 0.5 , red -> 0.5
      //tall -> 0.4 , short -> -.5
      //Map[Phenotype, (Percentage of population that has it, Birthrate, Deathrate)]
      var _traits: ArrayBuffer[Map[Symbol, (Double, Double, Double)]] = null
      //Stores the current trait. Used for the "constructor".
      var _currentTrait: Symbol = null

      //prey of the species
      var preyEvent = Map[Symbol, (Double, Symbol)]()

      

      //SETTERS
      //setter for the _name property
      def called(n: Symbol) = {
        _name = n
        EcoSystem.addSpecies(this)
        this
      }

      //setter for the _population property
      def of(p: Double) = {
        if (p > _carryingcapacity) { _population = _carryingcapacity }
        else { _population = p }
        this
      }

      //setter for the _carryingcapacity property

      def withCapacity(cc: Long) = {
        _carryingcapacity = cc
        this
      }



      //setter for the _time property
      def enterAt(t: Int) = {
        _time = t
        this
      }
      
      def mutationRate(m : Double) = {
        _probMutation = m
        this
      }
      
      def mutationProportion(m : Double) = {
        _mutationProportion = m
        this
      }

      //another setter for the _population property
      def population(p: Double) {
        of(p)
      }

      //another setter for the _carryingcapacity property
      def capacity(t: Long) = {
        withCapacity(t)
      }

      //another setter for the _time property
      def time(t: Int) = {
        enterAt(t)
      }

      //GETTERS
      def getName() = _name
      def getPopulation() = _population
      def getTime() = _time
      def getCarryingCapacity() = _carryingcapacity

      //METHODS
      //show all the data for the a particular species
      def showAll() {
        println("Name: " + _name)
        println("Population: " + _population.toLong)
        println("Start time: " + _time)
        println("Carrying Capacity: " + _carryingcapacity)
        if (!preyEvent.isEmpty) {
          print("One " + _name + " consumes ")
          var count: Int = 0
          preyEvent.keys.foreach((p) =>
            if (count == preyEvent.size - 1) {
              print(p + ": " + preyEvent(p)._1)
              count = count + 1
            } else {
              print(p + ": " + preyEvent(p)._1 + ", ")
              count = count + 1
              }
            )
          println()
        }
        if (this._traitReference != null) {
          var currentIndex = 0
          var currentPropor : Double = 0.0
          this._traitReference.keys.foreach { i =>
            currentIndex = this._traitReference.apply(i)
            println("Trait: " + i)
            var currentMap = this._traits.apply(currentIndex)
            currentMap.keys.foreach { i =>
              currentPropor = currentMap(i)._1*100
              print("Phenotype: " + i+" is ")
              print(f"$currentPropor%2.2f")
              print("% of population ")
              print("Birth rate: " + currentMap(i)._2+" ")
              print("Death rate: " + currentMap(i)._3+"\n")
            }
          }
        }
      }
      
      def update(updateBy : Double){
        this._population = (this._population * updateBy)
      }

      //print name and population
      def showNumbers() {
        println(_name + " Population: " + _population.toLong)
      }


      def remove(deaths: Long, spTrait: Symbol, givenType: Symbol): ExpressionResult = {
        var population = (_population*_traits(_traitReference(spTrait)).apply(givenType)._1)
        println("LRpop"+population)
        var amountToRemove : Double = 0
        if(deaths <= population){
          amountToRemove = deaths
        }
        else{
          amountToRemove = population
        }
        var netDiff = 0.0
        if (_population == 0.0)
        {
          netDiff = 0.0
        }
        else
        {
          netDiff = (amountToRemove.toDouble/_population).toDouble
        }
        var newProportion : Double = 0.0
        var currentTuple : (Double, Double, Double) = (0.0, 0.0, 0.0)
        _traits(_traitReference(spTrait)).keys.foreach{ phenotype =>
          currentTuple = _traits(_traitReference(spTrait)).apply(phenotype)
          if(phenotype.equals(givenType)){
            newProportion =  (currentTuple._1 - netDiff)/(1-netDiff)
          }
          else{
            newProportion =  (currentTuple._1)/(1-netDiff)
          }
          _traits(_traitReference(spTrait))(phenotype) = (newProportion, currentTuple._2, currentTuple._3)
        }
        _population -= amountToRemove
        new ExpressionResult()
      }
      
      def add(births: Long, spTrait: Symbol, givenType: Symbol): ExpressionResult = {
        var population = (_population*_traits(_traitReference(spTrait)).apply(givenType)._1)
        
        var netDiff = (births.toDouble/_population).toDouble
        var newProportion : Double = 0.0
        var currentTuple : (Double, Double, Double) = (0.0, 0.0, 0.0)
        _traits(_traitReference(spTrait)).keys.foreach{ phenotype =>
          currentTuple = _traits(_traitReference(spTrait)).apply(phenotype)
          if(phenotype.equals(givenType)){
            newProportion =  (currentTuple._1 + netDiff)/(1+netDiff)
          }
          else{
            newProportion =  (currentTuple._1)/(1+netDiff)
          }
          _traits(_traitReference(spTrait))(phenotype) = (newProportion, currentTuple._2, currentTuple._3)
        }
        _population += births
        new ExpressionResult()
      }
      
      def add(births: Double, spTrait: Symbol, givenType: Symbol): ExpressionResult = {
        var population = (_population*_traits(_traitReference(spTrait)).apply(givenType)._1).toLong
        population = (population * (births)).toLong
        add(population, spTrait, givenType)
        new ExpressionResult()
      }
      
      def remove(deaths: Double, spTrait: Symbol, givenType: Symbol): ExpressionResult = {
        var population = (_population*_traits(_traitReference(spTrait)).apply(givenType)._1).toLong
        
        population = (population * (deaths)).toLong
        remove(population, spTrait, givenType)
        new ExpressionResult()
      }
      
      
      def updateMutation(probMutation: Double, propOccurence: Double): ExpressionResult =  {
        _probMutation = probMutation
        _mutationProportion = propOccurence
        new ExpressionResult()
      }

      
       def addMutation(): ExpressionResult = {
        var death = EcoSystem.getRandomValue()
        var growth = EcoSystem.getRandomValue() * EcoSystem.getRandomValue()
        if (EcoSystem.getRandomValue() <= _probMutation)
        {
          _name addTrait Symbol("Mutation" + _mutationNum.toString()) 
          phenotype(Symbol("With Mutation" + _mutationNum.toString()), (_mutationProportion, growth, death)) phenotype (Symbol("Without Mutation" + _mutationNum.toString()), (1.0 - _mutationProportion, 0.0, 0.0))
          _mutationNum = _mutationNum + 1
        }
        new ExpressionResult
      }

      def setAsPrey(s: Symbol, consumption: Double): Expression = {
        
        if (!EcoSystem.genericEvents.contains(EcoSystem.DoNothing)) {
          
          new GenericEvent called EcoSystem.DoNothing definedAs new Function (
            //Print("Nothing should happen") ::
            End
          )
        }
        
        setAsPrey(s, consumption, EcoSystem.DoNothing)
    }
    
    def setAsPrey(s: Symbol, consumption: Double, ev: Symbol): Expression = {
        if (!EcoSystem.species.contains(s)) {
           //println(s + " does not exist *****")
        }
        else {
           preyEvent += (s -> (consumption, ev))
        }
        new ExpressionResult
    }
    
    def setAsPredator(s: Symbol, consumption: Double): Expression = {
        if (!EcoSystem.species.contains(s)) {
           //println(s + " does not exist *****")
        }
        else {
           EcoSystem.species(s).setAsPrey(_name, consumption)
        }
        new ExpressionResult
    }
    
    def setAsPredator(s: Symbol, consumption: Double, ev: Symbol): Expression = {
        if (!EcoSystem.species.contains(s)) {
           //println(s + " does not exist *****")
        }
        else {
           EcoSystem.species(s).setAsPrey(_name, consumption, ev)
        }
        new ExpressionResult
    }

      //

    def addTrait(traitName: Symbol) = {
      if (this._traits == null) {
        //Init traits list and add this map
        this._currentTrait = traitName
        this._traitReference = Map[Symbol, Int](traitName -> 0)
        this._traits = new ArrayBuffer[Map[Symbol, (Double, Double, Double)]]
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
    def phenotype(pheno: Symbol, phenoData: (Double, Double, Double)) = {
      //Get index of current trait
      var currentIndex = this._traitReference.apply(this._currentTrait)
      //If there is a map at this index, add to it
      if (this._traits.isDefinedAt(currentIndex)) {
        //May need to create map?
        var tempMap = this._traits.apply(currentIndex)

        tempMap.+=(pheno -> phenoData)

        this._traits.update(currentIndex, tempMap)
        this
        //Okay, added phenotype.
      } else {
        //CurrentIndex does not exist so need to create map.

        var newMap = Map[Symbol, (Double, Double, Double)](pheno -> phenoData)

        this._traits.insert(currentIndex, newMap)
        this
        //Added phenotype.
      }
    }

    //

    def showTrait(traitName: Symbol) {

      var currentIndex = this._traitReference.apply(traitName)
      println("Trait: " + traitName)
      var currentMap = this._traits.apply(currentIndex)
      currentMap.keys.foreach { i =>
        print("Phenotype = " + i)

        println(" Data = " + currentMap(i).toString())

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
           println("************** " + _name + " occurred **************")
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
         println("************** " + _name + " occurred **************")
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
        case e: GenericEvent       => EcoSystem.getGenericEvent(name)

      }
    }

    //try to convert any Symbol into a Species
    implicit def speciesString(name: Symbol): Species = {
      EcoSystem.getSpecies(name)
    }
    

    /********* Simulate ***********/
    def simulate(time: Int) = {
      //Test if trait proportions are valid  
      EcoSystem.species.keys.foreach { (sp) =>
          testTraits(sp)
        }
      
      //do time-1 because loop is inclusive
      for (a <- 0 to time - 1) {
        println("Time Step " + (EcoSystem.worldTime + 1));
        println(" -------------------- ");

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


        predation()
        reproduction()
        addMutations()

        //increment the time
        EcoSystem.worldTime += 1
        println(); println()
      }
    }
    
    class ExpressionResult extends Expression

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
    class RepeatClass(i: Int)(e: => List[Expression]) extends Expression {
      var counter = 0
      while (counter < i) {
        e
        counter += 1
      }
    }
    //used to call Step without new keyword
    object Repeat {
      def apply(i: Int)(e: => List[Expression]) = new RepeatClass(i)(e)
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
    
    def randomNumber = EcoSystem.getRandomValue()

  }


}
