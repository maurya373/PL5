import scala.collection.mutable.Map

object EcoSim {

  var Simulation_Time: Int = 0;

  // probably want to switch to a double eventually
  def simulate(time: Int) = {

    for (a <- 0 to time) {

      // all simulation code
      println("Year " + Simulation_Time + " Populations");
      println(" ---------- ");

      GlobalVars.species.keys.foreach((sp) =>
        if (GlobalVars.species.contains(sp)) {
          sp.update(1) //update the population by one time unit
          sp.showNumbers()
          
        })

      GlobalVars.events.keys.foreach((ev) =>
        if (GlobalVars.events.contains(ev)) {
          ev.runAll()
        })

      Simulation_Time += 1
      println();

    }
  }
  
  def showEnvironment(time: Int) = {
    
  }

  //lets have a way for global events to impact everything
  //like bad weather kills an entire species 
  //so events?

  class Species {

    // private vars for Species
    var _name: String = null
    var _population: Long = 0
    var _growthrate: Double = 0.0
    var _starttime: Int = 0

    // Setter for species name
    def called(n: String) = {
      _name = n
      GlobalVars.addSpecies(this)
      this
    }

    // Setter for species population
    def of(i: Long) = {
      _population = i
      this
    }

    // Setter for species growth rate
    def growat(x: Double) = {
      _growthrate = x
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
      print("Population: ")
      println(prettyPrintNum(_population))
      println("Growth rate: " + _growthrate)
      println("Start time: " + _starttime)
    }

    // print name and population
    def showNumbers() {
      print(_name + ": ")
      println(prettyPrintNum(_population))
    }

    // Setter for species population

    def population(x: Long) {
      println("setting population to " + x)
      _population = x
    }

    // Setter for species growth rate
    def growthrate(x: Double) {
      _growthrate = x
    }

    // Setter for species start time
    def starttime(x: Int) {
      _starttime = x
    }

    // Updates the population based on the growth
    def update(t: Int) = population(grow(t))

    // Grows the population by growth rate for duration time t  

    private def grow(t: Int): Long = { 
      if (t > 0) (_population + (_population * _growthrate)).toLong
      else grow(t - 1)
    }
      
    

}
  
  def prettyPrintNum(number: Long): String={
      if(number > 1000000000000L){
        var numString = (number/1000000000000f).toString()
        return numString + " trillion"
      }
      else if(number > 1000000000L){
        var numString = (number/1000000000f).toString()
        return numString + " billion"
      }
      else if(number > 1000000L){
        var numString = (number/1000000f).toString()
        return numString+ " million"
      }
      else if(number > 1000L){
        var numString = (number/1000f).toString()
        return numString + " thousand"
      }
      else{
        return number.toString()
      }
    }

  implicit def speciesString(name: String): Species = {
    GlobalVars.getSpecies(name)
  }

  implicit def eventString(name: String): Event = {
    GlobalVars.getEvent(name)
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

  class Event {

    var _name: String = null
    var _time: Int = 0

    // list of commands for this event
    var commands = Map[String, List[Any]]()

    var _statements: List[Any] = null

    // Setter for event name
    def called(n: String) = {
      _name = n
      GlobalVars.addEvent(this)
      this
    }

    def occursAtTime(t: Int) = {
      _time = t
      this
    }

    def show() {
      println(_name + " occurs at " + _time)
    }

    def runAll() {
      // Add the rest later
      commands.keys.foreach((cm) =>
        if (cm.equals("PopUpdate")) {
          internalPopUpdate(commands(cm))
        }
        else if (cm.equals("GrowthRateUpdate")){
          internalGrowthRateUpdate(commands(cm));
        })
        
        //TODO Implement execution. _statements is not assigned.
//        if (_time == Simulation_Time) {
//          println("time is " + _time)
//          execute()
//        };
    }

    def execute() {
      println(Simulation_Time)
      var s: Any = null
      for (s <- _statements) {
//        s
        println(s)
      }
    }

    def define(statements: => List[Any]) = {
      println("define")
      _statements = statements
      println("set it")
    }

    // Here temporarily until we realize better structure
    // Add to commands list
    def populationUpdate(name: String, pop: Long) {
      commands += ("PopUpdate" -> List(name, pop))
      
    }

    // Actual execution method
    def internalPopUpdate(l: List[Any]) {
      if (Simulation_Time == _time) {
        var tempName = l(0).toString()
        var tempPop = l(1).toString().toLong
//        println(tempName.toString())
//        println(tempPop.toString())
        GlobalVars.getSpecies(tempName).population(tempPop)
        println("*"+this._name+" event changes "+tempName+" population to "+prettyPrintNum(tempPop))
      }
    }
    
    def internalGrowthRateUpdate(l: List[Any]){
      if(Simulation_Time == _time){
        var tempName = l(0).toString()
        var tempRate = l(1).toString().toDouble
        GlobalVars.getSpecies(tempName).growat(tempRate)
        println("*"+this._name+" event changes "+tempName+" growth rate to "+tempRate.toString())
      }
    }

    def growthRateUpdate(name: String, gr: Double) {
      commands += ("GrowthRateUpdate" -> List(name, gr))  
    }

  }

  // Object of Global Variables for program users to interact with
  object GlobalVars {

    var species = Map[String, Species]()
    var events = Map[String, Event]()

    def addSpecies(s: Species) {
      species += (s._name -> s)
    }

    def addEvent(e: Event) {
      events += (e._name -> e)
    }

    def getSpecies(name: String): Species = {
      if (species.contains(name)) species(name)
      else null
    }

    def getEvent(name: String): Event = {
      if (events.contains(name)) events(name)
      else null
    }

  }

  def Expression() {

  }

  //Species _name of _population growat .4 startingat 0
  //_name parameterType is value

  def main(args: Array[String]) = {
    new Species called "Frog" of 5010000000L growat .2 startingat 0
    "Frog" growthrate 0
    "Frog" starttime 2
    "Frog" showAll

    new Species called "Mosquito" of 5000000 growat .1 startingat 0
//    "Fly" showAll
    
    new Species called "Frog" of 1000 growat .2 startingat 0
    //"Frog" growthrate 0
    //"Frog".growthrate(0)
    //"Frog" starttime 2 
    //"Frog" show
    new Species called "Fly" of 5000000 growat .1 startingat 0
    //    "Fly" showAll

    // population update is reflected in year 3 (after year 2)
    new Event called "Earthquake" occursAtTime 2 populationUpdate ("Frog", 373)
    new Event called "Zika" occursAtTime 2 growthRateUpdate ("Mosquito", .01) 

    new Event called "anotherone" occursAtTime 2
    "anotherone" define List(
      "Frog" population 5000,
      "Fly" population 3340)
    
    println("done")

    //        for i 1 20 loop
    //          print "Frog" showAll
    //        endloop
    //        if "Frog" population < "Fly" population startif
    //          populationUpdate("Frog", 0)
    //        endif
    //        populationUpdate ("Frog", 373)

    simulate(3)
    
    
    
    

  }

}
