import evo.Evo

object SmallTest3 extends Evo{
  def main(args: Array[String]) = {
    
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    new Species called 'Insects of 1000000 withCapacity Long.MaxValue enterAt 0
    
    'Dinosaurs enterAt 1
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000

    'Dinosaurs addTrait 'EyeAnatomy phenotype('DualLid, (0.5, 0.15, 0.1)) phenotype ('SingleLid, (0.5, 0.14, 0.1))
    'Insects addTrait 'Wings phenotype('Yes, (0.2, 0.01, 0.009)) phenotype('No, (0.8, 0.009, 0.009))
    
    new DeterministicEvent called 'IceAge at 500 definedAs new Function (
      Repeat (3) (
           UpdatePopulationBy('Dinosaurs, 0.9) :: //0.9^3 = 0.729
           UpdatePopulationBy('Insects, 1.05) :: //1.05^3 = 1.157625

           End
      ) ::
      // 300000*0.739 = 218700
      (If(('Dinosaurs getPopulation) < 220000) (
        (new Species called 'Mammoths of 5000 withCapacity 100000) ::
        ('Mammoths addTrait 'TuskSize phenotype('Large, (0.7, 0.02, 0.0195)) phenotype('Small, (0.3, 0.02, 0.019999)) ) ::
      End
      )) ::
      End
    )
    
    
    
    
    showEcosystem    
    simulate(5)
    showEcosystem
    
    
    
    showEcosystem
    simulate(1000)
    showEcosystem
    
  }
}