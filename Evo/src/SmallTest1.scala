import evo.Evo

object SmallTest1 extends Evo{
  def main(args: Array[String]) = {
    
    new Species called 'Dinosaurs of 10000 withCapacity 200000 enterAt 0
    new Species called 'Crocodiles of 100 withCapacity 20000 enterAt 0
    
    'Dinosaurs enterAt 1
    'Dinosaurs population 50000
    'Dinosaurs capacity 300000

    'Dinosaurs addTrait 'EyeAnatomy phenotype('DualLid, (0.5, 0.15, 0.1)) phenotype ('SingleLid, (0.5, 0.14, 0.1))
    
    
    showEcosystem    
    simulate(5)
    showEcosystem
    
    'Dinosaurs addTrait 'Feathers phenotype('Colorful, (0.8, 0.09, 0.1)) phenotype ('Dull, (0.2, 0.14, 0.1))
    
    showEcosystem
    simulate(10)
    showEcosystem
    
  }
}