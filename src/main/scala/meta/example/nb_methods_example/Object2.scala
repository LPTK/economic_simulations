package meta.example.nb_methods_example

import meta.classLifting.SpecialInstructions.waitTurns
import meta.deep.runtime.Actor
import squid.quasi.lift

@lift
class Object2 extends Actor{

  val deposit: Int = 500

  def get(msgId: Int, randArg: Double, randArg2: Double): Int = {
    println("Async msg" + msgId + ": remote method with wait 1 " +
      " arg: " + randArg +  " arg: " + randArg2)
    waitTurns(1)
    deposit
  }

  def main(): Unit = {
    while (true) {
      waitTurns(1)
    }
  }
}
