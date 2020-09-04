package meta.example.monitor_example.agent_monitor
import meta.deep.runtime.{Actor}
import squid.quasi.lift

@lift
class MainInit extends Actor {
  def main(): List[Actor] = {
    val monitor: monitorSim = new monitorSim
    val foo: object1 = new object1(monitor)

    List(monitor, foo)
  }
}
