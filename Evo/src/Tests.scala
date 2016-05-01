
 
import evo.Evo

object Tests extends Evo {
  
  def main(args: Array[String]) = {
  
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000
    'Dinosaurs enterAt 1
       
    
    //example of defining deterministic event
    new DeterministicEvent called 'Earthquake at 3 definedAs new Function (
      
      If(('Dinosaurs getPopulation) < 500) (
           KillSpecies('Dinosaurs) ::
           End
      ) ::      
      
      End
    )
    
    //example of defining a random event
    new RandomEvent called 'Meteor withProbability .1 definedAs new Function (
      KillSpecies('Dinosaurs) ::
      Print("BOOM!") ::
      End
    )
    
    
    showEcosystem
    
    simulate(5)
    
    showEcosystem
  }

}