package evo

import scala.collection.mutable.Map

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
    "Jans" addTrait "Hair" phenotype("Ponytail", 0.5) phenotype("Bald", 0.2)
    
    "Jans" showTrait "Eye Color"
    "Jans" showTrait "Height"
    
    // "Jans" getTrait "Eye Color" remove 0.2 of "Blue"
        
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



