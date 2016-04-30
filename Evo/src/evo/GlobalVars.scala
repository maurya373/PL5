package evo

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