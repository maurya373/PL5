import evo.Evo

object SmallTest2 extends Evo{
  def main(args: Array[String]) = {
    
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    new Species called 'Crocodiles of 100 withCapacity 20000 enterAt 0
    
    'Dinosaurs enterAt 1
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000

    'Dinosaurs addTrait 'EyeAnatomy phenotype('DualLid, (0.5, 0.15, 0.1)) phenotype ('SingleLid, (0.5, 0.14, 0.1))
    
    new RandomEvent called 'AsteroidImpact withProbability .05 definedAs new Function (
      Print("\n\nBOOM!\n\n") ::
      (UpdateAllPopulationsBy(.1))  ::  // 90% of the population dies
      KillSpecies('Dinosaurs) :: //All dinosaurs die
      End
    )
    
    showEcosystem    
    simulate(5)
    showEcosystem
    
    'Crocodiles addTrait 'Habitat phenotype('Saltwater, (0.5, 0.06, 0.05)) phenotype ('Freshwater, (0.5, 0.055, 0.05))
    
    showEcosystem
    simulate(1000)
    showEcosystem
    
  }
}