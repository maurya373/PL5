import collection.mutable.Buffer
import scala.collection.mutable.Map

object Evo {
  
    // Object of Global Variables for program users to interact with
  object EcoSystem {
    var worldTime: Int = 0
    var endTime: Int = 0

    var events = Map[Symbol, Any]()
  }
  
  abstract class EventClass {
    var _name: Symbol = null
    var _time: Int = 0
    
    def called(n: Symbol): EventClass
    def at(t: Int): EventClass
    def execute()
    def show() {
      println(_name + " occurs at " + _time)
    }
  }
  
  
  trait Expression
  
  class DeterministicEvent extends EventClass {
    var fun: FUNCTION = null
    
    def called(n: Symbol) = {
      _name = n
      //EcoSystem.addDeterministicEvent(this)
      this
    }

    def at(t: Int) = {
      _time = t
      this
    }
    
    def execute() {
        
    }
    
    def definedAs(function: => FUNCTION) {
       EcoSystem.events += (_name -> new EVNT(function))
    }
  }
  
  class EVNT[Expression](body: => Expression){
    def get = body
  }
  
  class FUNCTION {
    val expressions = Buffer[Expression]()
    trait Expression {
      expressions += this 
    } 
    
    def run = expressions
  }
  
  class EMPTY extends Expression{}
  def empty = new EMPTY()
  
  class IF(condition: Boolean, expressions: => Expression) extends Expression {
    if (condition) expressions
  }
  
  class STEP(i: Int, expressions: => Expression) extends Expression {    
    var counter = 0
    while (counter < i) {
      expressions
      counter += 1
    }
  }
  
  class PRINT(str: String) extends Expression {
    println(str)
  }
  
  
   def main(args: Array[String]) = {
     new DeterministicEvent called 'Tornado at 4 definedAs (new FUNCTION {
         new PRINT("12")
         new IF (1 < 2, (
           new PRINT("true")   
         ))
         new STEP(3, (
           new PRINT("FARES")  
         ))
     })
     
     var a = 0;
     for(a <- 1 to 10){
        println( "Value of a: " + a );
        if (a == 4) {
          var c = EcoSystem.events('Tornado)
          var dog:EVNT[_] = c.asInstanceOf[EVNT[_]]          
          println(dog.get)     
        }
     }
      
     println("---")
   }
  
}