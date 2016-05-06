 
Developed by Maurya Avirneni, Kshitij Delvadiya, Prabhat Nagarajan, and Gilad Oved

Note
-----

Please view the section at the bottom for future work, which was omitted from our presentation due to time constraints.

About
-----

Evo is an internal Scala DSL. The goal of Evo is to provide a very general, simple, and powerful tool for biologists to model natural selection, evolution, and ecosystems. The narrowness in the scope of this language allows biologists to remain ignorant of types, objects and other programming language paradigms. They can just focus on writing the central components of the language. It is easy to simulate for the time periods you want and easy to obtain data after any time step.


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
```scala
 simulate(100)
 showEcosystem
```

Call simulate with the number of time steps desired. Call showEcosystem to provide comprehensive data for each species.


API
---
```scala
simulate(5)
simulate(x:Int)
```
Run the simulation for 5 timesteps.


```scala
showEcosystem
```
Show the current state of the ecosystem. If called after running simulate(5), then you will see a detailed report of the status of each species in the ecosystem after 5 time steps.


```scala
new Species called 'Human of 20000 withCapacity 10000000 enterAt 0
new Species called 'Human of x:[Int|Double|Long] withCapacity y:[Long] enterAt z:Int
```
Create a species called Human with the following properties:
 a population of 20,000
 a carrying capacity of 10,000,000
 entering the simulation at time step 0


```scala
'Human addTrait 'Height phenotype('Tall, (0.6, 0.54, 0.22)) phenotype ('Short, (0.4, 0.538, 0.2))
'Human addTrait i:Symbol phenotype(j:Symbol, (x:Double, y:Double, z:Long)) phenotype ('Short, (a:Double, b:Double, c:Long))
```
Add a trait to the Human species with the following properties:
 the trait's name: Height
 phenotypes:
  Tall (60% of the population with a growth rate of 0.54 and a death rate of 0.22)
  Short (40% of the population with a growth rate of 0.538 and a death rate of 0.2)
Any number of phenotypes can be added to a trait. The percentages of the population that has each trait must sum to 1 (100%). If the don't, a warning will be displayed but the error will not be corrected. Any growth calculation will not be accurate.


```scala
'Human.setAsPrey('Plant, 2)
'Human.setAsPrey(x:Symbol, y:Double)
```
Plants are preyed upon by Humans. Each human eats 2 plants per time step. 


```scala
'Plant.setAsPredator('Panda, 0.01)
'Plant.setAsPredator(x:Symbol, y:Double)
```
The predator of Plants is a Panda. Each panda eats 0.01 plants per time step.


```scala
'Hawk updateMutation(0.0001, .01)
'Hawk updateMutation(x:Double, y:Double)
```
Give Hawks the ability to mutate. 1% of the population (y) will mutate with a probability of 0.0001 (x)


```scala
randomNumber
'Hawk updateMutation(randomNumber, randomNumber)
```
Replaced with a random number between 0 and 1 [0.0, 1.0]. Can be used anywhere a Double is required.


Future Work
-----
- **Including Natural Resources instead of only Species:** Our current model is based on the assumption that the organisms and living entities in an ecosystem play the primary rle in ecosystem dynamics. However, ecosystems is defined as its living AND nonliving components. Therefore, we can extend our language to include nonliving components such as space, natural resources, etc.

- **Data Output:** If a biologist is going to study simulations on an ecosystem, he/she would like the data from the simulation to be readily available. While Evo only stores the state of the ecosystem at the "current" time step, we could very easily modify it to store the entire history of the ecosystem. If we integrate this history with software like R, we can allow the biologist to easily generate graphs of the population over time, the trait distribution over time, etc.

- **Correlated Traits:** One major implementation addition we should allow if correlated traits. For instance, our languages allows for there to be 50 blue-eyed crocodiles and 50 red-eyed crocodiles. We can also have 40 short ones and 60 tall ones. However our language does not allow one to specify how many short, blue-eyed crocodiles exist.

- **Preferences of Diet etc.** Our current predation structure involves one creature eating somenumber of another creature as a time step. For example, one human eats 3 chickens at each time step. We then allow the user to create an alternative event if chickens go extinct. However, we would like a more complex dietary structure, where species have dietary preferences, or certain traits in a species have dietary preferences, along with alternative preferences.

- **Error Handling:** Presently, our language has default settings if the programmer does not specify things. Additionally, it may issue warnings, but it will continue to operate. We plan to add proper error handling to this language.

- **Multiple Ecosystems:** If we allow for multiple ecosystems, we can allow for interaction betweene ecosystems. Additionally, since our language is structured so that simulations of an ecosystem run serially, we could allow for parallelization in a multiple ecosystem setting.

- **Have standard probability distributions for things like mutations** Many biological and evolutionary processes follow distributions. A mutation is essentially a trait that is either has a positive or negative effect on the survival of a species. We hope to allow the biologist(programmer) to specify a probability distribution from which we draw to determine if a mutation is positive or negative.
