package evo

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

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
        if (!GlobalVars.species.contains(s)) {
           println(s + " is extinct *****")
        }
        else {
           prey += (s -> consumption)
        }
    }
    
    def setAsPredator(s: String, consumption: Long) {
        if (!GlobalVars.species.contains(s)) {
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
