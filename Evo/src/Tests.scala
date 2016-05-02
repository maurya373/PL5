import evo.Evo

object Tests extends Evo {
  
  def main(args: Array[String]) = {
  
    // Create All Species
    
    new Species called 'Human of 20000 withCapacity 10000000 enterAt 0
    new Species called 'Hawk of 500 withCapacity 10000000 enterAt 0
    new Species called 'Jaguar of 800 withCapacity 10000000 enterAt 0
    new Species called 'Snake of 5000 withCapacity 10000000 enterAt 0
    new Species called 'Spider of 10000000 withCapacity 10000000 enterAt 0
    new Species called 'Insect of 100000000 withCapacity 5000000000L enterAt 0
    new Species called 'Frog of 20000 withCapacity 10000000 enterAt 0
    new Species called 'Rabbit of 10000 withCapacity 10000000 enterAt 0
    new Species called 'Panda of 200 withCapacity 100000 enterAt 0
    new Species called 'Deer of 10000 withCapacity 10000000 enterAt 0
    new Species called 'Plant of 50000000 enterAt 0
    new Species called 'Tree of 10000000 enterAt 0
    
    // Add all traits, phenotypes, and growth rates
    
    'Human addTrait 'Height phenotype('Tall, (0.6, 0.54, 0.22)) phenotype ('Short, (0.4, 0.538, 0.2))
    'Human addTrait 'EyeColor phenotype('Blue, (0.2, 0.6, 0.4)) phenotype ('Brown, (0.8, 0.1, 0.05))
    
    'Hawk addTrait 'Color phenotype('White, (0.68, 0.5, 0.2)) phenotype ('Brown, (0.22, 0.8, 0.3)) phenotype ('Black, (0.1, 0.8, 0.3))
    
    'Jaguar addTrait 'TailSize phenotype('Long, (0.5, 0.3, 0.1)) phenotype ('Short, (0.5, 0.2, 0.15))
    
    'Snake addTrait 'Appearance phenotype('Striped, (0.2, 0.3, 0.1)) phenotype ('Solid, (0.6, 0.2, 0.15)) phenotype('Pattern, (0.2, 0.3, 0.1))
    
    'Spider addTrait 'Venomous phenotype('Lethal, (0.9, 0.3, 0.1)) phenotype ('Nonlethal, (0.1, 0.2, 0.15))
    
    'Insect addTrait 'Exoskeleton phenotype('Yes, (0.75, 0.3, 0.1)) phenotype ('No, (0.25, 0.2, 0.15))
    
    'Frog addTrait 'Habitat phenotype('Water, (0.68, 0.3, 0.1)) phenotype ('Land, (0.32, 0.2, 0.15))
    
    'Rabbit addTrait 'Ears phenotype('Long, (0.6, 0.5, 0.45)) phenotype ('Short, (0.4, 0.3, 0.45))
    
    'Panda addTrait 'Weight phenotype('Heavy, (0.95, 0.7, 0.2)) phenotype ('Light, (0.05, 0.2, 0.15))
    
    'Deer addTrait 'Color phenotype('DarkBrown, (0.15, 0.3, 0.1)) phenotype ('LightBrown, (0.85, 0.2, 0.15))
    
    'Plant addTrait 'Poisonous phenotype('Yes, (0.11, 0.3, 0.1)) phenotype ('No, (0.89, 0.2, 0.15))
    
    'Tree addTrait 'Height phenotype('Tall, (0.3, 0.3, 0.1)) phenotype('Medium, (0.5, 0.3, 0.1)) phenotype ('Short, (0.2, 0.2, 0.15))
    
    
    // All predator/prey relationships
    
    'Human.setAsPrey('Jaguar, 0.001)
    'Human.setAsPrey('Rabbit, 0.005)
    'Human.setAsPrey('Plant, 2)
    
    'Hawk.setAsPrey('Snake, 1)
    'Hawk.setAsPrey('Frog, 1)
    'Hawk.setAsPrey('Rabbit, 1)
    
    'Jaguar.setAsPrey('Human, 0.25)
    'Jaguar.setAsPrey('Rabbit, 1)
    'Jaguar.setAsPrey('Panda, 0.25)
    'Jaguar.setAsPrey('Deer, 0.5)
    
    'Snake.setAsPrey('Spider, 5)
    'Snake.setAsPrey('Frog, 2)
    'Snake.setAsPrey('Rabbit, 1)
    
    'Spider.setAsPrey('Insect, 3)
    'Spider.setAsPrey('Plant, 0.001)
    
    'Insect.setAsPrey('Plant, 0.0001)
    'Insect.setAsPrey('Tree, 0.00001)
    
    'Frog.setAsPrey('Insect, 3)
    'Frog.setAsPrey('Spider, 1)
    
    'Plant.setAsPredator('Panda, 0.01)
    'Plant.setAsPredator('Deer, 0.01)
    'Plant.setAsPredator('Rabbit, 0.001)
    
    'Tree.setAsPredator('Panda, 0.005)
    'Tree.setAsPredator('Deer, 0.005)
    
    
    // Add some mutations
    
    'Hawk updateMutation(0.0001, .01)
    'Snake updateMutation(0.0002, .03)
    'Plant updateMutation(0.0002, .02)
    
    
    // Show initial species pool
    showEcosystem
    
    

    // Define some generic event

    
    new GenericEvent called 'RabbitExtinction definedAs new Function (
      'Human.setAsPrey('Deer, 0.5) ::
      End
    )
    
    'Human.setAsPrey('Rabbit, 0.1, 'RabbitExtinction)
    
    
    // Define some random events to occur during the simulation
    
    new RandomEvent called 'Earthquake withProbability .01 definedAs new Function (
      (UpdateAllPopulationsBy(.75))  ::  // 75% of each species population remain
      If(('Panda getPopulation) < 10) (
           Print("PANDAS ARE EXTINCT") ::
           KillSpecies('Panda) ::
           End
      ) ::      
      End
    )
    

    new RandomEvent called 'Deforestation withProbability .05 definedAs new Function (
      (UpdatePopulationBy('Tree, .8))  :: // 80% of trees remain 

      End
    )
    
    new RandomEvent called 'Flood withProbability .05 definedAs new Function (
      (UpdateAllPopulationsBy(.9))  ::  // 90% of the population remains
      'Human.remove(.2, 'Height, 'Short) ::  // 20% of the short humans die during the flood
      End
    )
    
    
    // Define some deterministic events
    

    new DeterministicEvent called 'EarthDay at 500 definedAs new Function (
      Repeat (3) (
           UpdatePopulationBy('Tree, 1.1) ::
           UpdatePopulationBy('Plant, 1.05) ::

           End
      ) ::      
      End
    )
    
    //observe effects of predators of frogs and prey of frogs
    new DeterministicEvent called 'FrogDeathStar at 100 definedAs new Function (
      KillSpecies('Frog) :: 
      End
    )
    
    new GenericEvent called 'FrogExtinction definedAs new Function (
      'Snake.setAsPrey('Snake, 0.01) ::
      End
    )
    
    simulate(10000)
    showEcosystem
    
    
  }
}