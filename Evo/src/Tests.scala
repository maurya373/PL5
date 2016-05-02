import evo.Evo

object Tests extends Evo {
  
  def main(args: Array[String]) = {
  
    // Create All Species
    
    new Species called 'Human of 2000 withCapacity 10000000 enterAt 0
    new Species called 'Hawk of 2000 withCapacity 10000000 enterAt 0
    new Species called 'Jaguar of 500 withCapacity 10000000 enterAt 0
    new Species called 'Snake of 7500 withCapacity 10000000 enterAt 0
    new Species called 'Spider of 100000 withCapacity 10000000 enterAt 0
    new Species called 'Insect of 100000000 withCapacity 5000000000L enterAt 0
    new Species called 'Frog of 10000 withCapacity 10000000 enterAt 0
    new Species called 'Rabbit of 10000 withCapacity 10000000 enterAt 0
    new Species called 'Panda of 100 withCapacity 100000 enterAt 0
    new Species called 'Deer of 100000 withCapacity 10000000 enterAt 0
    new Species called 'Plant of 5000000 withCapacity 50000000 enterAt 0
    new Species called 'Tree of 1000000 withCapacity 20000000 enterAt 0
    
    // Add all taits, phenotypes, and growth rates
    
    'Human addTrait 'Height phenotype('Tall, (0.6, randomNumber, randomNumber)) phenotype ('Short, (0.4, 0.2, 0.15))
    'Human addTrait 'EyeColor phenotype('Blue, (0.2, 0.6, 0.4)) phenotype ('Brown, (0.8, 0.1, 0.05))
    
    'Hawk addTrait 'Color phenotype('White, (0.78, 0.5, 0.2)) phenotype ('Brown, (0.22, 0.8, 0.3))
    
    'Jaguar addTrait 'TailSize phenotype('Long, (0.5, 0.3, 0.1)) phenotype ('Short, (0.5, 0.2, 0.15))
    
    'Snake addTrait 'Appearance phenotype('Striped, (0.2, 0.3, 0.1)) phenotype ('Solid, (0.8, 0.2, 0.15))
    
    'Spider addTrait 'Venomous phenotype('Lethal, (0.9, 0.3, 0.1)) phenotype ('Nonlethal, (0.1, 0.2, 0.15))
    
    'Insect addTrait 'Exoskeleton phenotype('Yes, (0.75, 0.3, 0.1)) phenotype ('No, (0.25, 0.2, 0.15))
    
    'Frog addTrait 'Habitat phenotype('Water, (0.68, 0.3, 0.1)) phenotype ('Land, (0.32, 0.2, 0.15))
    
    'Rabbit addTrait 'Ears phenotype('Long, (0.6, 0.5, 0.45)) phenotype ('Short, (0.4, 0.3, 0.45))
    
    'Panda addTrait 'Weight phenotype('Heavy, (0.95, 0.7, 0.2)) phenotype ('Light, (0.05, 0.2, 0.15))
    
    'Deer addTrait 'Color phenotype('DarkBrown, (0.15, 0.3, 0.1)) phenotype ('LightBrown, (0.85, 0.2, 0.15))
    
    'Plant addTrait 'Poisonous phenotype('Yes, (0.11, 0.3, 0.1)) phenotype ('No, (0.89, 0.2, 0.15))
    
    'Tree addTrait 'Height phenotype('Tall, (0.65, 0.3, 0.1)) phenotype ('Short, (0.35, 0.2, 0.15))
    
    
    // All predator/prey relationships
    
    'Human.setAsPrey('Jaguar, 0.02)
    'Human.setAsPrey('Rabbit, 0.1)
    'Human.setAsPrey('Plant, 2)
    
    'Hawk.setAsPrey('Snake, 1)
    'Hawk.setAsPrey('Frog, 1)
    'Hawk.setAsPrey('Rabbit, 1)
    
    'Jaguar.setAsPrey('Human, 1)
    'Jaguar.setAsPrey('Rabbit, 3)
    'Jaguar.setAsPrey('Panda, 1)
    'Jaguar.setAsPrey('Deer, 1)
    
    'Snake.setAsPrey('Spider, 10)
    'Snake.setAsPrey('Frog, 3)
    'Snake.setAsPrey('Rabbit, 1)
    
    'Spider.setAsPrey('Insect, 10)
    'Spider.setAsPrey('Plant, 1)
    
    'Insect.setAsPrey('Plant, 1)
    'Insect.setAsPrey('Tree, 1)
    
    'Frog.setAsPrey('Snake, 1)
    'Frog.setAsPrey('Frog, 1)
    'Frog.setAsPrey('Rabbit, 1)
    
    'Plant.setAsPredator('Insect, 0.0001)
    'Plant.setAsPredator('Panda, 0.01)
    'Plant.setAsPredator('Deer, 0.01)
    'Plant.setAsPredator('Rabbit, 0.001)
    
    'Tree.setAsPredator('Insect, 0.00001)
    'Tree.setAsPredator('Panda, 0.005)
    'Tree.setAsPredator('Deer, 0.005)
    
    
    // Show initial species pool
    showEcosystem
    
    // Define some random events to occur during the simulation
    
    showEcosystem
    simulate(500)
    showEcosystem
    
    new RandomEvent called 'Earthquake withProbability .03 definedAs new Function (
      (UpdateAllPopulationsBy(.6))  ::  // 60% of each species population remain
      If(('Panda getPopulation) < 10) (
           KillSpecies('Panda) ::
           End
      ) ::      
      End
    )
    
    new RandomEvent called 'Deforestation withProbability .1 definedAs new Function (
      (UpdatePopulationBy('Tree, .8))  :: // 80% of Tree remain 
      End
    )
    
    new RandomEvent called 'Flood withProbability .05 definedAs new Function (
      (UpdateAllPopulationsBy(.9))  ::  // 90% of the population remains
      End
    )
    
    
    // Define some deterministic events
    
    new DeterministicEvent called 'EarthDay at 1000 definedAs new Function (
      ('Tree add(10, 'EyeColor, 'Blue)) ::
      ('Jans updateMutation(0.5, 0.3))::
      If(('Dinosaurs getPopulation) < 500) (
           KillSpecies('Dinosaurs) ::
           End
      ) ::      
      
      End
    )
    
    
    
    
/*
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

    simulate(10)
    showEcosystem

*/
    
    
    

  }

}