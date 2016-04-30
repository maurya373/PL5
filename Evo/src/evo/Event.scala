package evo
  
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