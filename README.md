 
Developed by Maurya Avirneni, Kshitij Delvadiya, Prabhat Nagarajan, and Gilad Oved

Note
-----

Please view the section at the bottom for future work, which was omitted from our presentation due to time constraints.

About
-----

The goal of our programming language Evo is to provide a very general, simple, and powerful tool for biologists to model natural selection, evolution, and ecosystems. The narrowness in the scope of this language allows biologists to remain ignorant of types, objects and other programming language paradigms. They can just focus on writing the central components of the language. It is easy to simulate for the time periods you want and easy to obtain data after any time step.


Usage
-----
Download the file from this repo called `Evo.scala`

To use our dsl, make sure to import that file into the scala file you are working with

Also you need to make sure to ***extend Evo***

All set!


EVO Documentation
-----------------

Species
-------
```scala
 new Species called 'Fly of 1000 entersAt 1 withCapacity 10000000
```
This creates a species named Fly, with a population of 1000, which enters the ecosystem at time 1. withCapacity defines the Carrying Capacity, which is the maximum number of individuals that can live for that species.


Traits
------
```scala
 'Jaguar addTrait 'TailSize phenotype('Long, (0.6, 0.3, 0.1)) phenotype ('Short, (0.4, 0.2, 0.15))
```
This adds a trait called TailSize to the Jaguar species. There are two phenotypes for this trait, which are Long and Short. The first number is the proportion of the species with that particular phenotype. The second and third numbers are the birth rate and death rate respectively. 


Mutations
---------
```scala
 'Snake updateMutation(0.0002, .03)
```
Snakes would have a .02% probability of developing a mutation, and that mutation would initally affect 3% of the population of snakes. Mutations are technically traits, and (arbitrarily defined by an internal function) some mutations will have a larger birth rate than a death rate, and will survive. Most will die off fairly quickly.


Predation
---------
```scala
 'Human.setAsPrey('Jaguar, 0.001)
 'Plant.setAsPredator('Deer, 2)
```

A species can be set as a prey or a predator. The rate of consumption means how many of the prey the predator eats at a time step. For example, 1 human eats .001 of a Jaguar, and a Deer eats 2 Plants.


Events
------

Within the function, subsequent statements are followed by double colons ::, and the end of statements must end with the keyword 'End'. The examples below will solidify these rules.


Random Event
------------
```scala
 new RandomEvent called 'Earthquake withProbability .01 definedAs new Function (
       (UpdateAllPopulationsBy(.75))  ::  // 75% of each species population remain
       If(('Panda getPopulation) < 10) (
            Print("PANDAS ARE EXTINCT") ::
            KillSpecies('Panda) ::
            End
       ) ::      
       End
 )
```

A random event occurs with at the defined probabilty at any given time step. Here, there is an If statement that checks if the population of Pandas are less then 10, and if so, run a print statement, and then call the method KillSpecies()


Deterministic Event
-------------------
```scala
 new DeterministicEvent called 'EarthDay at 500 definedAs new Function (
      Repeat (3) (
           UpdatePopulationBy('Tree, 1.1) ::  // Update Tree population to 110% of current Tree population
           UpdatePopulationBy('Plant, 1.05) ::

           End
      ) ::      
      End
 )
```

A deterministic event occurs exactly at the time step the user defines. Here, there is a Repeat statement, which runs the following statements to UpdatePopuluations by a rate a total of 3 times. 


Generic Event
-------------
```scala
 new GenericEvent called 'RabbitExtinction definedAs new Function (
      'Human.setAsPrey('Deer, 0.5) ::
      End
 )
    
 'Human.setAsPrey('Rabbit, 0.1, 'RabbitExtinction)
```

A generic event only only be applied for when a prey goes extinct. In this case, we apply the generic event called RabbitExtinction to Humans, who eat Rabbits. When the Rabbit population is in fact 0, Deer will be set as prey for Humans.


Displaying Data
---------------
```
 simulate(100)
 showEcosystem
```

Call simulate with the number of time steps desired. Call showEcosystem to provide comprehensive data for each species.


API
---

TODO: List of Methods User can call
