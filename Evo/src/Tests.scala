import evo.Evo

object Tests extends Evo {
  
  def main(args: Array[String]) = {
  
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    new Species called 'Jans of 100 withCapacity 20000 enterAt 0
    
    'Jans addTrait 'EyeColor phenotype('Blue, (0.5, 0.0, 0.0)) phenotype ('Red, (0.5, 0.0, 0.0))
    
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000
    'Dinosaurs enterAt 1
       
    //remove(organisms, trait, phenotype)
    //'Jans remove(0.2, 'EyeColor, 'Blue)
    
    'Jans updateMutation(0.5, 0.3)
    'Jans addMutation
    
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
      'Jans.capacity(80000) ::
      KillSpecies('Dinosaurs) ::
      Print("BOOM!") ::
      End
    )
    
    
    
    showEcosystem
    
    simulate(5)
    
    showEcosystem
    
    
    
    
  }

}