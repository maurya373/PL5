//import scala.collection.mutable.Map
//import collection.mutable.Buffer

object EcoSim {
  def main(args: Array[String]) = { 
    println("ok")
  }
}
  /*var rand = scala.util.Random

  // probably want to switch to a double eventually
  def simulate(time: Int) = {
    //do time-1 because loop is inclusive  
    for (a <- 0 to time - 1) {

      // all simulation code
      println("Time Step " + (GlobalVars.simulation_Time + 1) + " Data");
      println(" _______________________ ");

      GlobalVars.deterministicEvents.keys.foreach((ev) =>
        if (GlobalVars.deterministicEvents.contains(ev)) {
          ev.runAll()
        })

      GlobalVars.randomEvents.keys.foreach((ev) => {
        if (GlobalVars.randomEvents.contains(ev)) {
          val r = rand.nextDouble()
          if (GlobalVars.randomEvents(ev).getProbability() < r) {
            ev.runAll()
          }
        }
      })

      GlobalVars.species.keys.foreach((sp) =>
        if (GlobalVars.species.contains(sp)) {
          if (sp._starttime <= GlobalVars.simulation_Time) {
            sp.update(1) //update the population by one time unit

          }
          sp.showNumbers()
        })

      GlobalVars.simulation_Time += 1
      println();

    }
  }

  //print state of the entire ecosystem, i.e. all species
  def showEcosystem() = {
    println("State at beginning of time step: " + GlobalVars.simulation_Time)
    println()
    GlobalVars.species.keys.foreach((sp) =>
      if (GlobalVars.species.contains(sp)) {
        sp.showAll()
        println("-------------------------------------")
      })
    println()
  }

  //lets have a way for global events to impact everything
  //like bad weather kills an entire species 
  //so events?

  class Species {

    // private vars for Species
    var _name: String = null
    var _population: Long = 0
    var _birthrate: Double = 0.0
    var _deathrate: Double = 0.0
    var _starttime: Int = 0

    // Setter for species name
    def called(n: String) = {
      _name = n
      GlobalVars.addSpecies(this)
      this
    }

    // Setter for species population
    def of(i: Int) = {
      _population = i
      this
    }

    // Setter for species growth rate
    def deathrate(x: Double) = {
      _deathrate = x
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
    }

    // print name and population
    def showNumbers() {
      println(_name + " Population: " + _population)
    }

    // Setter for species population
    def population(x: Long) {
      //      println("setting " + _name + " population to " + x)
      _population = x
    }

    def population(): Long = {
      this._population
    }

    // Setter for species start time
    def starttime(x: Int) {
      _starttime = x
    }

    // Updates the population based on the growth
    def update(t: Int) = population(grow(t))

    // Grows the population by growth rate for duration time t  
    private def grow(t: Int): Long =
      if (t > 0) (_population + ((_population * _birthrate).toLong) - ((_population * _deathrate).toLong))
      else grow(t - 1)

  }

  implicit def speciesString(name: String): Species = {
    GlobalVars.getSpecies(name)
  }

  implicit def eventString(name: String): Event = {
    var e = GlobalVars.getEvent(name)
    e match {
      case e: DeterministicEvent => GlobalVars.getDeterministicEvent(name)
      case e: RandomEvent        => GlobalVars.getRandomEvent(name)
    }
  }

  // Need to think about how to structure these event defs within the code properly
  // I Just took a shortcut for now, but we want these to be
  // linked to a particular event
  //  def populationUpdate(name: String, pop: Int) {
  //    GlobalVars.getSpecies(name).population(pop)
  //  }
  //    
  //  def growthRateUpdate(name: String, gr: Double) {
  //    GlobalVars.getSpecies(name).growat(gr)
  //  }

  abstract class Event {

    var _name: String = null
    var _time: Int = 0

    // list of commands for this event
    var _statements: () => Unit = _

    def called(n: String): Event

    def show() {
      println(_name + " occurs at " + _time)
    }

    def runAll() {
      //execute event if it's time
      if (_time == GlobalVars.simulation_Time) {
        println("************** " + _name + " occurred **************")
        execute()
      }
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

    def at(t: Int) = {
      _time = t
      this
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
      println(randomEvents)
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

  /*IF ("Jans" population < "Fly" population) THEN
      statements
      ....
    DONE*/

  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value

  /*def DO (statements: => List[Any]) {
     println("fraij")
     println("list0: " + statements(0))
     def exec() {
       
     }
   }*/

  /*trait HouseElement
   case class house( elements: HouseElement* )
   trait FloorElement
   case class floor( elements: FloorElement * ) extends HouseElement
   case class bedroom( name: String ) extends FloorElement
   case class fraij(b: Boolean) extends FloorElement
   case object kitchen extends FloorElement
   
   trait Statements
   case class PROGRAM( statements: Statements) extends Statements {
     
   }
   
   case class DO(statements: Statements) extends Statements {
     def get = statements
   }
   
   case class PRINT(str: String) extends Statements{
     println(str)
   }
   
   class Foo[A](body: => A){
     def get = body
   }*/


  /*
   * 
   * 
   */
  
  trait Expression
  class DEVENT extends Event {
    def called(n: String) = {
      _name = n
      //GlobalVars.addDeterministicEvent(this)
      this
    }

    def at(t: Int) = {
      _time = t
      this
    }
    
    /*def definedAs(function: => FUNCTION[Expression]) {
       println(function)
    }*/
  }
  
  /*class FUNCTION[A](expressions: => A) {
    val exprs = Buffer[Expression]()
    trait Expression {
      exprs += this 
    } 
  }*/
  
  class PRINT[A](str: => String) extends Expression {
    println(str)
  }

  class IF[A](condition: Boolean, expressions: => A) extends Expression {
    if (condition) expressions
  }
  class LOOP[A](i: Int, expressions: => A) extends Expression {    
    var counter = 0
    while (counter < i) {
      expressions
      counter += 1
    }
  }

  def main(args: Array[String]) = {      
    println("oee")
    //var e = new DEVENT called "TORNADO" at 4
    //println(e)
    /*e.definedAs(new FUNCTION {
      new PRINT("ayy")
    })*/
    
    
    
    /*new EVENTT('TORNADO, 4, new FUNCTION {
      new IF(1 < 2, (
        new PRINT("FRAIJ")
      ))

      new LOOP(5, (
        new PRINT("FARES")
     ))
    })*/
    
    
    
    
    
    
    
    /*var a = new Foo (
      println("FRAIIJ")    
    )
    
    var i = 0;
    for(i <- 1 to 10){
      println( "Value of a: " + i);
      if (i == 5) {
        a.get
      }
    }*/

    /*PROGRAM (
      DO (
         PRINT("fraijjjjj")  
      )
    )*/

    /*DO (
       null    
     )*/

    /*house (
      floor (
        bedroom("kids"),
        bedroom("master"),
        fraij(("Jans" population) < ("Fly" population))
      ),
      floor (
        kitchen
      )
    )*/

    /*new Species called "Frog" of 100 birthrate 1 deathrate 0.5 startingat 0
    new Species called "Fly" of 1000 birthrate 1 deathrate 0.5 startingat 0*/

    /*new Event called "e" occursAtTime 2
    "e" define {
      () => "Frog" population 5000
      () => "Fly" population 3340
    }*/

    // population update is reflected in year 3 (after year 2)
    //new Event called "Earthquake" occursAtTime 2 populationUpdate ("Frog", 373)

    //"anotherone".define { () => ??? }
    //new DeterministicEvent called "Tornado" occursAtTime 1
    /*DO {
      "Frog" population 0,
      "Fly" population 0,
      "Frog" birthrate 1,
      println("sweg")
    }*/

    /*"Tornado" define 
    
    DO statements DO
    define(() => )
    define(() => {}_
    "Tornado" define (() => {
      "Frog" population 0
      "Fly" population 0
      "Frog" birthrate 1
      new Species called "Jans" of 1000 birthrate 2 deathrate 0.5 startingat 2
      
    })*/

    /*new RandomEvent called "Tornadoes" withProbability .5 define (() => { 
       println("ahhh")
    })*/

    /*"anotherone" define new Gilad({
      "Frog" population 5000
      "Fly" population 3340
    })*/

    /*(() => {
      "Frog" population 5000
      "Fly" population 3340
    })*/

    //        for i 1 20 loop
    //          print "Frog" showAll
    //        endloop
    //        if "Frog" population < "Fly" population startif
    //          populationUpdate("Frog", 0)
    //        endif
    //        populationUpdate ("Frog", 373)

    /*showEcosystem()
    simulate(3)
    showEcosystem()*/

    /*IF ("Jans" population < "Fly" population) THEN
      statements
      ....
    DONE*/

    /*if(("Jans" population) <  ("Fly" population)){
      "Frog" population 5000
    }
    else{
      "Frog" population 6000
    }
    
    while(("Jans" population) > 0){
      //Kill one Jan
      var newPop = ("Jans" population)-1
      "Jans" population newPop
    }*/

    //showEcosystem()

    println("---")
  }

}*/