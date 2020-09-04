package meta.example.meeting_example

import meta.classLifting.Lifter
import meta.deep.IR
import meta.deep.codegen.{CreateActorGraphs, CreateCode, EdgeMerge, Pipeline}
import meta.deep.runtime.Actor
import IR.TopLevel._

object meetingExample extends App {

  val cls1: ClassWithObject[Person] = Person.reflect(IR)
  val mainClass: ClassWithObject[MainInit] = MainInit.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1)
  val lifter = new Lifter()
  val simulationData = lifter(startClasses, mainClass)

  val pipeline = Pipeline(
    new CreateActorGraphs(simulationData._1),
    List(
      new EdgeMerge(),
      new CreateCode(simulationData._2, "generated/main/scala"),
    ))

  pipeline.run()
}