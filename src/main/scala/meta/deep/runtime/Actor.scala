package meta.deep.runtime

import java.util.UUID

import meta.deep.runtime.Actor.AgentId
import org.slf4j.LoggerFactory

import scala.collection.mutable.{ListBuffer, Map}

/**
  * This object handles the unique id generation of an actor
  * as long as all ids are generated on a single instance
  */
object Actor {
  type AgentId = Long
  var lastAgentId: AgentId = 0

  /**
    * Generates a new id for an agent and returns it
    *
    * @return id for an agent
    */
  def getNextAgentId: AgentId = {
    lastAgentId = lastAgentId + 1
    lastAgentId
  }

  val newActors: ListBuffer[Actor] = ListBuffer[Actor]()

  val waitTurnList: ListBuffer[Int] = ListBuffer[Int]()

  // track the number of Sims waiting for each label at each iteration. Set once
  val waitLabels: Map[String, Int] = Map[String, Int]()

  // track the min value that each label group should advance by
  val labelVals: Map[String, ListBuffer[Double]] = Map[String, ListBuffer[Double]]()
  var proceedLabel: Map[String, Double] = Map[String, Double]()

  // global interrupt table
//  val interrupts: Map[Double, ListBuffer[Message]] = Map[Double, ListBuffer[Message]]()

  def initLabelVals(): Unit = {
    waitLabels("time") = 0
    waitLabels.keys.foreach(k => {
      labelVals(k) = ListBuffer[Double]()
      proceedLabel(k) = 0
    })
  }

  private def minWaits(l: ListBuffer[Double], total: Int): Double = {
    if (l.length == total) {
      l.min
    } else {
      0
    }
  }

  def proceedGroups(): Unit = {
//    println("labels: ", waitLabels)
//    println("label vals: ", labelVals)
//    println("proceed label: ", proceedLabel)

    waitLabels.keys.foreach(k => {
      proceedLabel(k) = minWaits(labelVals(k), waitLabels(k))
      labelVals(k).clear()
    })
  }

  def minTurn(): Int = {
    if (waitTurnList.length > 0){
      waitTurnList.min
    } else {
      1
    }
  }
}

/**
  * This class is the supertype of the messages
  */
abstract class Message extends Serializable {

  /**
    * The sender of the message
    */
  val senderId: Actor.AgentId

  /**
    * The receiver of the message
    */
  val receiverId: Actor.AgentId

  /**
    * A unique id for the message-communication (request/response)
    */
  var sessionId: String = UUID.randomUUID().toString

  override def toString: String = {
    "Message: " + senderId + " -> " + receiverId + "(" + sessionId + ")"
  }
}

/**
  * This represents a message, which is used for sending something to another actor
  * @param senderId the id of the sender
  * @param receiverId the id of the receiver
  * @param methodId the id of the method which should be called
  * @param argss the arguments of the method
  */
case class RequestMessage(override val senderId: Actor.AgentId,
                          override val receiverId: Actor.AgentId,
                          methodId: Int,
                          argss: List[List[Any]])
    extends Message {

  var future: Future[Any] = Future[Any]()
  /**
    * this functions simplified the replying to a method
    * @param owner the sender of the reply message
    * @param returnValue the return value/answer for the request message
    */
  def reply(owner: Actor, returnValue: Any): Unit = {
    val msg = ResponseMessage(receiverId, senderId, returnValue)
    msg.sessionId = this.sessionId
    owner.sendMessage(msg)
  }
}

/**
  * This class is used to answer to a received message.
  * @param senderId the id of the sender
  * @param receiverId the id of the receiver
  * @param arg the return value of the method/answer of the request message
  */
case class ResponseMessage(override val senderId: Actor.AgentId,
                           override val receiverId: Actor.AgentId,
                           arg: Any)
    extends Message

case class Future[+T](var isCompleted: Boolean = false,
                      val value: Option[T] = None,
                      val id: String = UUID.randomUUID().toString){
  def setValue[U >: T](y: U): Future[U] ={
    Future(true, Some(y), id)
  }
}

/**
  * This class represents the main class of the generated classes
  * It contains the logic for message handling and defines the
  * functions for a step-wise simulation
  */
class Actor() {

  var id: AgentId = Actor.getNextAgentId
  var currentTurn: Int = 0
  var currentTime: Double = 0
  var current_pos: Int = 0
  var monitor = Monitor

  var async_messages: Map[String, Future[Any]] = Map[String, Future[Any]]()

//  val logger = LoggerFactory.getLogger("Sims")
  val logger = LoggerFactory.getLogger(this.getClass.getName)

  final def isCompleted(future_obj: Future[Any]): Boolean = {
    async_messages.get(future_obj.id).isDefined
  }

  final def getFutureValue[T](future_obj: Future[T]): T = {
    async_messages.get(future_obj.id).get.value.get.asInstanceOf[T]
  }

  final def clearFutureObj(future_obj: Future[Any]): None.type ={
    async_messages = async_messages.-(future_obj.id)
    None
  }

  /**
    * Contains the received messages from the previous step
    */
//  protected var receivedMessages: List[Message] = List()
  var receivedMessages: List[Message] = List()

  /**
    * Contains the messages, which should be sent to other actors in the next step
    */
  var sendMessages: List[Message] = List()

  /**
    * A map of listeners, which is required to register a listener for a response of a request message
    */
  var responseListeners
    : Map[String, Message => Unit] = Map()

  var interrupts: Map[Double, ListBuffer[Message]] = Map()

  /**
    * Adds one message to the sendActions list, which will be collected and distributed at the end of the step
    *
    * @param message Action, which should be sent to a different Agent
    */
  final def sendMessage(message: Message): Unit = {
    if (message.receiverId == this.id) {
      addReceiveMessages(List(message))
    } else {
      sendMessages = message :: sendMessages
    }
  }

  // localized interrupts
  final def registerInterrupt(time: Double, message: Message): Unit = {
    if (interrupts.get(time).isDefined){
      interrupts(time).append(message)
    } else {
      interrupts(time) = ListBuffer(message)
    }
  }

  final def checkInterrupts(time: Double): Actor = {
    val registeredInterrupts: Option[ListBuffer[Message]] = interrupts.remove(time)
    if (registeredInterrupts.isDefined){
      receivedMessages = receivedMessages ::: registeredInterrupts.get.toList
    }
    this
  }

  // global interrupts
//  final def registerInterrupts(time: Double, message: Message): Unit = {
//    if (Actor.interrupts.get(time).isDefined){
//        Actor.interrupts(time).append(message)
//      } else {
//        Actor.interrupts(time) = ListBuffer(message)
//      }
//  }
//
//  final def callInterrupt(time: Double): Actor = {
//    val registeredInterrupts: Option[ListBuffer[Message]] = Actor.interrupts.remove(time)
//    if (registeredInterrupts.isDefined){
//      receivedMessages = receivedMessages ::: registeredInterrupts.get.toList
//    }
//    this
//  }

  /**
    * Adds a list of messages to the agent
    *
    * @param messages Actions with receiver matching the agent from the previous step
    */
  final def addReceiveMessages(messages: List[Message]): Actor = {
    this.receivedMessages = this.receivedMessages ::: messages.filter(
      x =>
        x.isInstanceOf[RequestMessage] || responseListeners
          .get(x.sessionId)
          .isEmpty)
    messages
      .filter(
        x =>
          responseListeners.get(x.sessionId).isDefined && x
            .isInstanceOf[ResponseMessage])
      .foreach(x => {
        val handler = responseListeners(x.sessionId)
        responseListeners.remove(x.sessionId)
        handler(x)
      })
    this
  }

  /**
    * This returns all messages, which are sent via sendMessage
    * @return the actor itself
    */
  final def getSendMessages: List[Message] = {
    sendMessages
  }

  /**
    * This resets sendMessages, so that getSendMessages is empty again
    * @return the actor itself
    */
  final def cleanSendMessage: Actor = {
    sendMessages = List()
    this
  }

  /**
    * Sets a message response handler for a specific session id
    *
    * @param sessionId session of message you want to listen for a response
    * @param handler   function, which handles the message
    */
  final def setMessageResponseHandler(sessionId: String,
                                      handler: Message => Unit): Unit = {
    responseListeners += (sessionId -> handler)
  }

  /**
    * This function removes all receivedMessages of type RequestMessage from the receivedMessages list
    * and returns them to the method caller
    * @return a list of receivedMessages of type RequestMessage
    */
  final def popRequestMessages: List[RequestMessage] = {
    val rM = this.receivedMessages
      .filter(_.isInstanceOf[RequestMessage])
      .map(_.asInstanceOf[RequestMessage])
    this.receivedMessages =
      this.receivedMessages.filterNot(_.isInstanceOf[RequestMessage])
    rM
  }

  /**
    * This function removes all receivedMessages of type ResponseMessage from the receivedMessages list
    * and returns them to the method caller
    * @return a list of receivedMessages of type ResponseMessage
    */
  final def popResponseMessages: List[ResponseMessage] = {
    val rM = this.receivedMessages
      .filter(_.isInstanceOf[ResponseMessage])
      .map(_.asInstanceOf[ResponseMessage])
    this.receivedMessages =
      this.receivedMessages.filterNot(_.isInstanceOf[ResponseMessage])
    rM
  }

  /**
    * This runs the stepFunction until the timer > until
    * @param until how long the code should be executed
    * @return the actor itself
    */
  def run_until(until: Int): Actor = {
    println("run_until from the actor!")
    while (currentTurn <= until) {
      println(this.getClass.getSimpleName, currentTurn, until, current_pos)
      val (a, b) = stepFunction
      current_pos = a
      currentTurn = b
    }
    this
  }

  /**
    * Executes one step in the simulation.
    * By default it does not change the pos and increases the timer at 1 (next step)
    * @return a function, which takes the position and timer and
    *         returns the next position and timer which should be passed again
    *         when calling this function the next time.
    */
  def stepFunction: (Int, Int) = (current_pos, currentTurn + Actor.minTurn())
}
