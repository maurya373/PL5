import evo.Evo

object Tests extends Evo {
  
  def main(args: Array[String]) = {
  
    
    // ************** Large Program
    
    new Species called 'Human of 2000 withCapacity 10000000
    new Species called 'Hawk of 2000 withCapacity 10000000
    new Species called 'Jaguar of 500 withCapacity 10000000
    new Species called 'Python of 7500 withCapacity 10000000
    new Species called 'Spider of 100000 withCapacity 10000000
    new Species called 'Frog of 10000 withCapacity 10000000
    new Species called 'Rabbit of 10000 withCapacity 10000000
    new Species called 'Panda of 50 withCapacity 100000
    new Species called 'Plants of 5000000 withCapacity 50000000
    new Species called 'Trees of 1000000 withCapacity 20000000
    
    new Species called 'Orcs of 1000000 withCapacity 20000000 
    new Species called 'Ents of 1000000 withCapacity 20000000
    new Species called 'Elves of 1000000 withCapacity 20000000
    
    'Human.setAsPrey('Jaguar, 1)
    'Human.setAsPrey('Rabbit, 1)
    'Human.setAsPrey('Plants, 2)
    
    // Finish Food Web Consumption
    // Finish Traits
    
    
    new RandomEvent called 'Earthquake withProbability .005 definedAs new Function (
      (UpdatePopulation('Panda, 10)  ::
      ('Jans updateMutation(0.5, 0.3))::
      If(('Dinosaurs getPopulation) < 500) (
           KillSpecies('Dinosaurs) ::
           End
      ) ::      
      
      End
    )
    
    
    /*
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    new Species called 'Crocdiles of 100 withCapacity 20000 enterAt 0
    
    'Dinosaurs enterAt 1
    'Dinosaurs addTrait 'EyeAnatomy phenotype('DualLid, (0.5, 0.15, 0.1)) phenotype ('SingleLid, (0.5, 0.14, 0.1))
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000

    'Dinosaurs enterAt 1
       
    remove(organisms, trait, phenotype)
    'Jans remove(0.2, 'EyeColor, 'Blue)
    
    'Jans addMutation
    
    //example of defining deterministic event
    new DeterministicEvent called 'Earthquake at 3 definedAs new Function (
      ('Jans remove (10, 'EyeColor, 'Blue)) ::
      ('Jans updateMutation(0.5, 0.3))::
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

    simulate(10)
    showEcosystem

    */
    
    
    

  }

}