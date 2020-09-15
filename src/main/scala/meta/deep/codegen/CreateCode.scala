package meta.deep.codegen

import java.io.{BufferedWriter, File, FileWriter}

import meta.deep.IR
import meta.deep.IR.Predef._
import meta.deep.algo.AlgoInfo
import meta.deep.algo.AlgoInfo.EdgeInfo
import meta.deep.member.{ActorType}
import meta.deep.runtime.Actor
import meta.compile.GeneratedPackage._

import scala.collection.mutable.ArrayBuffer

class CreateCode(initCode: OpenCode[List[Actor]], storagePath: String, generatedPackage: GeneratedPackage)
    extends StateMachineElement() {

  var compiledActorGraphs: List[CompiledActorGraph] = Nil

  override def run(compiledActorGraphs: List[CompiledActorGraph])
    : List[CompiledActorGraph] = {

    //Create dir folders to save class
    val f = new File(storagePath)
    if (!f.exists()) {
      f.mkdirs()
    }

    this.compiledActorGraphs = compiledActorGraphs

    for (cAG <- this.compiledActorGraphs) {
      prepareClass(cAG)
    }

    createInit(IR.showScala(initCode.rep))

    null
  }

  /**
    * This code generated the class data and passes it for the class generation
    * @param compiledActorGraph the graph data required for generating the class
    */
  def prepareClass(compiledActorGraph: CompiledActorGraph): Unit = {
    var self_name = Map[String, String]()
    compiledActorGraph.actorTypes.map(actorType =>
      self_name += (actorType.self.toCode.toString().substring(5).dropRight(1) -> actorType.name))
//    val selfs = compiledActorGraph.actorTypes.map(actorType =>
//      actorType.self.toCode.toString().substring(5).dropRight(1))

    val commands = generateCode(compiledActorGraph)
    val code = this.createCommandOpenCode(commands)

    val codeWithInit = this.generateVarInit(
      compiledActorGraph.variables2,
      this.generateMutVarInit(
        compiledActorGraph.variables,
        code"""
              val ${AlgoInfo.timeVar} = squid.lib.MutVar(0)
              val ${AlgoInfo.positionVar} = squid.lib.MutVar(0)
              meta.deep.algo.Instructions.splitter
              val getCommands = () => $code
              val commands = getCommands()
              meta.deep.algo.Instructions.splitter
              (until: Int) => {
                while ((${AlgoInfo.timeVar}!) <= until && (${AlgoInfo.positionVar}!) < commands.length) {
                  val command = commands((${AlgoInfo.positionVar}!))
                  command()
                }
                ${compiledActorGraph.actorTypes.head.self}
              }
          """
      )
    )

    val scalaCode = IR.showScala(codeWithInit.rep)
    var replacedTypes = scalaCode
//    selfs.foreach(x => replacedTypes = replacedTypes.replace(x, "this"))
    val steps = changeTypes(replacedTypes)

    //Needed to split, so that a function can be extracted from the code, to write everything as class variables
    val parts =
      steps.split("""meta\.deep\.algo\.Instructions\.splitter\(\);""")

    val timerPattern = "timeVar_[0-9]*".r
    val timeVarGenerated: String = timerPattern.findFirstIn(parts(0)).get
    val timeVarReplaceWith: String = "currentTurn" // timer as defined by actor class

    var initVars: String = parts(0).substring(2)
      .replace(" var "," private var ")
      .replace(" val ", " private val ")
      .replace(s"private var ${timeVarGenerated}: scala.Int = 0;\n  ", s"") +
      parts(1).replace(timeVarGenerated, timeVarReplaceWith)

    //This ugly syntax is needed to replace the received code with a correct function definition
    var run_until = "  override def run_until" + parts(2)
      .trim()
      .substring(1)
      .replaceFirst("=>", ": meta.deep.runtime.Actor = ")
      .replaceAll(timeVarGenerated, timeVarReplaceWith)
      .dropRight(1)
      .trim
      .dropRight(1)

    // "classname_" is for merging optimization
    var initParams: String = compiledActorGraph.actorTypes.flatMap(actorType => {
      actorType.states.map(s =>{
        self_name.keys.foreach(self => {
          initVars = initVars.replace(s"${self}.${s.sym.name};", s"this.${s.sym.name};")
          initVars = initVars.replace(s"${self}.`${s.sym.name}_=`", s"this.`${s.sym.name}_=`")
//          initVars = initVars.replace(s"${self}.${s.sym.name};", s"this.${self_name(self)}_${s.sym.name};")
//          initVars = initVars.replace(s"${self}.`${s.sym.name}_=`", s"this.`${self_name(self)}_${s.sym.name}_=`")
        })
        s"  var ${s.sym.name}: ${changeTypes(s.tpe.rep.toString)} = ${changeTypes(IR.showScala(s.init.rep))}"
        //        s"  var ${actorType.name}_${s.sym.name}: ${changeTypes(s.tpe.rep.toString)} = ${changeTypes(IR.showScala(s.init.rep))}"
      })}).mkString("\n")

    val parameters: String = compiledActorGraph.actorTypes.flatMap(actorType => {
      actorType.parameterList.map(x => {
        if (compiledActorGraph.parameterList.indexOf(x) != -1) {
          self_name.keys.foreach(self => {
            initVars = initVars.replace(s"${self}.${x._1};", s"this.${x._1};")
            initVars = initVars.replace(s"${self}.`${x._1}_=`", s"this.`${x._1}_=`")
//            initVars = initVars.replace(s"${self}.${x._1};", s"this.${self_name(self)}_${x._1};")
//            initVars = initVars.replace(s"${self}.`${x._1}_=`", s"this.`${self_name(self)}_${x._1}_=`")
          })
          s"var ${x._1}: ${changeTypes(x._2, false)}"
//          s"var ${actorType.name}_${x._1}: ${changeTypes(x._2, false)}"
        } else {
          ""
        }})
    }).mkString(", ")

    self_name.keys.foreach(self => {
      initParams = initParams.replace(self, "this")
      initVars = initVars.replace(self, "this")
      run_until = run_until.replace(self, "this")
    })

    def parents: String = {
      s"${compiledActorGraph.parentNames.head}${compiledActorGraph.parentNames.tail.foldLeft("")((a,b) => a + " with " + b)}"
    }

    createClass(compiledActorGraph.name, parameters, initParams, initVars, run_until, parents);
  }

  /**
    * This generates the code of the state machine
    * @param compiledActorGraph the graph data for generating the code of an actor
    * @return a list of commands/code fragments, which can be called
    */
  def generateCode(
      compiledActorGraph: CompiledActorGraph): List[OpenCode[Unit]] = {
    val graph: ArrayBuffer[EdgeInfo] = compiledActorGraph.graph
    //Reassign positions
    var positionMap: Map[Int, Int] = Map()

    val groupedGraph = graph.groupBy(_.from.getNativeId)

    var code: ArrayBuffer[OpenCode[Unit]] = ArrayBuffer[OpenCode[Unit]]()

    var changeCodePos: List[(Int, EdgeInfo)] = List()
    var requiredSavings: List[Int] = List()
    var posEdgeSaving: Map[(Int, Int), Int] = Map()
    var edgeSaving: Map[(Int, Int), Int] = Map()

    def generateCodeInner(node: Int): Unit = {

      positionMap = positionMap + (node -> code.length)

      val start = groupedGraph.getOrElse(node, ArrayBuffer[EdgeInfo]())

      //If we have more than one unknown cond, we have to store the edges to the list, so that the position can be looked up
      var unknownCondNode: Int = 0
      start.foreach(edge => {
        if (edge.cond == null) {
          unknownCondNode = unknownCondNode + 1
        }
      })

      // Assume, a node has either only conditions or not any.
      // If in future something is different this line throws an error
      // to show that this thing has to be reimplemented to find a ways to know
      // whether a condition should be followed or a jump executed
      // At the moment conditions only apply in if statements, thus it has two
      // Outgoing edges and thus are not merged, thus this should not happen
      // Therefore we assume, if unknownCondNode == 0 then this is a conditional edge
      assert(start.length == unknownCondNode || unknownCondNode == 0)

      start.zipWithIndex.foreach(edgeIndex => {
        val edge = edgeIndex._1
        val target = edge.to.getNativeId

        //Go to next free pos, and if already next node code is defined go to first code fragment of next node
        val nextPos = positionMap.getOrElse(target, code.length + 1)

        //If there are more than one unknown cond, we have to get the position from stack
        var unknownCond: Int = 0
        groupedGraph
          .getOrElse(target, ArrayBuffer[EdgeInfo]())
          .foreach(edge2 => {
            if (edge2.cond == null) {
              unknownCond = unknownCond + 1
            }
          })

        var posChanger: OpenCode[Unit] =
          code"${AlgoInfo.positionVar} := ${Const(nextPos)}"
        if (unknownCond > 1) {
          requiredSavings = target :: requiredSavings
          posChanger =
            code"${AlgoInfo.positionVar} := ${edge.positionStack}.remove(0).find(x => x._1 == (${Const(
              edge.edgeState._1)},${Const(edge.edgeState._2)})).get._2; ()"
        }

        val currentCodePos = code.length

        if (edge.storePosRef.nonEmpty) {
          changeCodePos = (currentCodePos, edge) :: changeCodePos
        }
        if (unknownCondNode > 1 && edge.cond == null) {
          posEdgeSaving = posEdgeSaving + ((node, target) -> currentCodePos)
        }
        edgeSaving = edgeSaving + ((node, target) -> currentCodePos)

        if (edge.cond != null) {
          code.append(
            code"if(${edge.cond}) {${edge.code}; $posChanger} else {}")
        } else {
          code.append(code"${edge.code}; $posChanger")
        }

        if (nextPos == code.length) {
          generateCodeInner(target)
        }

        //Rewrite to jump to next code pos, if conditional statement wrong, which is only known after the sub graph is generated and the next
        //edge is added (else part)
        if (edge.cond != null && start.length > (edgeIndex._2 + 1)) {
          code(currentCodePos) =
            code"if(${edge.cond}) {${edge.code}; $posChanger} else {${AlgoInfo.positionVar} := ${Const(code.length)}}"
        }

        //Add wait at end, if there is a wait on that edge
        if (edge.waitEdge) {
          code(currentCodePos) =
            code"${code(currentCodePos)}; ${AlgoInfo.timeVar} := (${AlgoInfo.timeVar}!) + 1"
        }

//        code(currentCodePos) =
//          code"println(${Const(currentCodePos)}); ${code(currentCodePos)}"

      })
    }

    generateCodeInner(0)

    // Add position storing for the code elements, where required
    changeCodePos.foreach(x => {
      val c = code(x._1)
      x._2.storePosRef.foreach(edgeInfoGroup => {

        var amountM1: Int = 0

        val edgeInfos = edgeInfoGroup.map(edgeInfo => {
          val startPos = edgeInfo.from.getNativeId
          val endPos = edgeInfo.to.getNativeId
          if (!requiredSavings.contains(edgeInfo.from.getNativeId)) {
            amountM1 = amountM1 + 1
          }
          (edgeInfo.edgeState,
           edgeSaving((startPos, endPos)),
           edgeInfo.positionStack,
           (edgeInfo.from.getNativeId, edgeInfo.to.getNativeId))
        })

        //The basic idea was this, but it may not hold, therefore this assert to change the implementation
        //It's either all or non of them, so either it's a direct call or it's not, since the local graph
        //is not modified, so method inlining should be applied to all subgraphs. If there is a wait, then it should
        //be for all as well, so this makes no difference
        assert(amountM1 == 0 || amountM1 == edgeInfos.length)

        //There are some edges, which require a stack value
        //At the moment, we can assume, that there are either all edges not needing a jump position or not
        //if (amountM1 < edgeInfos.length) {
        if (amountM1 == 0) {
          assert(edgeInfos.count(_._3 == edgeInfos.head._3) == edgeInfos.length)
          val startCode = code"List[((Int, Int), Int)]()"
          val lookupT = edgeInfos.foldRight(startCode)((a, b) =>
            code"((${Const(a._1._1)},${Const(a._1._2)}),${Const(a._2)}) :: $b")
          code(x._1) = code"$c; ${edgeInfos.head._3}.prepend($lookupT)"
        }
        //Find edges, which have no popping added and append popping from stack to remove value if added before
        /*if (amountM1 != edgeInfos.length) {
          edgeInfoGroup.foreach(edgeInfo => {
            if (!requiredSavings.contains(edgeInfo.from.getNativeId)) {
              val startPos = edgeInfo.from.getNativeId
              val endPos = edgeInfo.to.getNativeId
              val codePos:Int = edgeSaving((startPos, endPos))
              val c1 = code"${edgeInfo.positionStack}.remove(0); ()"
              val c2 = code(codePos)
              code(codePos) = code"$c2; $c1"
            }
          })
        }*/

      })
    })

    //Rewrite variables to replaced ones
    code = code.map(x => {
      var y = x
      compiledActorGraph.variables.foreach({
        case v =>
          //Quick-fix for var types
          if (v.from != null) {
            y = y.subs(v.from).~>(code"(${v.to}!).asInstanceOf[${v.A}]")
          }
      })
      y
    })

    code.toList
  }

  /**
    * Creates the class file
    *
    * @param className types defined in the class file in the form of case class
    * @param initParams state variables
    * @param initVars   generated variables needed globally
    * @param run_until  function, which overrides the run until method
    * @param parents a string containing parent names
    */
  def createClass(className: String,
                  parameters: String,
                  initParams: String,
                  initVars: String,
                  run_until: String,
                  parents: String): Unit = {
    val classString =
      s"""package ${generatedPackage}

class ${className} (${parameters}) extends ${parents} {
$initParams
$initVars
$run_until
}
"""

    val file = new File(storagePath + "/" + className + ".scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

  /**
    * This changes the type of the variables to reference to the generated classes.
    * @param code which should be changed
    * @return code with replaced variable types
    */
  def changeTypes(code: String, init: Boolean = false): String = {

    var result = code

    for (cAG <- this.compiledActorGraphs) {
      //We only replace compiledActorGraphs with 1 actor type, otherwise it is a merged one
      if (cAG.actorTypes.length == 1) {
        // canonical name of the actor, including full path and the actor name 
        // actorTypes.head.name is needed for merging 
        val canonicalSimName: String = 
          (generatedPackage match {
            case vanillaPackage(pkgName) => pkgName  
            case mergedPackage(pkgName) => pkgName 
            case ssoPackage(pkgName) => pkgName 
          }) + "." + cAG.actorTypes.head.name

        // replace the package of canonical Sim name with the generated one. Respect word boundary
        result = result.replaceAll("\\b"+canonicalSimName+"\\b", generatedPackage + "." + cAG.name)
      }
    }

    // new Sims are added to newActors at runtime 
    init match {
      case true => result 
      case _ => {
        val newSimPattern = s"(\\s+)val (\\S*) = new generated.(\\S*);".r
        newSimPattern.replaceAllIn(result, 
          m=>{(m + s"${m.group(1)}meta.deep.runtime.Actor.newActors.append(${m.group(2)})")})
      }
    }
  }

  /**
    * Converts a list of opencode code fragments to a opencode of array of code fragments
    */
  def createCommandOpenCode(
      commands: List[OpenCode[Unit]]): OpenCode[Array[() => Unit]] = {
    var start: OpenCode[Array[() => Unit]] =
      code"new Array[() => Unit](${Const(commands.length)})"
    commands.zipWithIndex.foreach(x => {
      start = code"val data = $start; data(${Const(x._2)}) = () => ${x._1}; data"
    })
    start
  }

  /**
    * Generates init code for variables of a list of VarWrappers
    *
    * @param variables list of varWrapper
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateMutVarInit[R: CodeType](variables: List[AlgoInfo.VarWrapper[_]],
                                      after: OpenCode[R]): OpenCode[R] =
    variables match {
      case Nil => code"$after"
      case x :: xs =>
        initVar(x, generateMutVarInit(xs, after))
    }

  /**
    * generates init code for variables of list of type VarValue
    *
    * @param variables list of varvalue
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateVarInit[R: CodeType](variables: List[VarValue[_]],
                                   after: OpenCode[R]): OpenCode[R] =
    variables match {
      case Nil => code"$after"
      case x :: xs =>
        initVar2(x, generateVarInit(xs, after))
    }

  /**
    * This generates the init code of the simulation
    * @param code init code of the simulation
    */
  def createInit(code: String): Unit = {
    var modifiedCode: String = code

    // to support merge; break traits
//    if (hasDirectAccess(code)){
//      println("Direct attribute access from MainInit is supported for backward compatibility only. Please use parameter list instead")
//      val agentMap: Map[String, String] = buildAgentNameMap(modifiedCode)
//      modifiedCode = code.split("\n").map(statement =>
//        // keep the compiler happy about unreachable cases
//        statement.replace(".`", s".`${agentMap.getOrElse(getObjectName(statement), "ERROR")}_")
//      ).mkString("\n")
//    }

    val classString =
      s"""package ${generatedPackage}

object InitData  {
  def initActors: List[meta.deep.runtime.Actor] = {${changeTypes(modifiedCode, init = true)}}
}"""
    val file = new File(storagePath + "/InitData.scala")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(classString)
    bw.close()
  }

  def hasDirectAccess(code: String): Boolean = {
    code.contains("_=`")
  }

  def buildAgentNameMap(code: String): Map[String, String] = {
    var tokens: Array[String] = Array()
    var agentNameMap: Map[String, String] = Map[String, String]()
    code.split(";").filter(_.contains("new ")).foreach(
      line => {
        tokens = line.split("\\s+")
        agentNameMap += (tokens(tokens.indexOf("val") + 1) -> shortenPathName(tokens(tokens.indexOf("new") + 1)))
      })
    agentNameMap
  }

  def shortenPathName(fullPathName: String): String = {
    fullPathName.slice(
      ((a: Int, b: Int) => (if (a > 0) a else b)) (fullPathName.lastIndexOf(".")+1, 0),
      ((a: Int, b: Int) => (if (a > -1) a else b)) (fullPathName.indexOf("("), fullPathName.size)
    )
  }

  def getObjectName(statement: String): String = {
    (statement.split("\\s+").filterNot(_=="")).head.split("\\.").head
  }

  /**
    * Generates init code of one variable of type VarWrapper
    *
    * @param variable of type VarWrapper
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar[A, R: CodeType](variable: AlgoInfo.VarWrapper[A],
                                      rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"""val ${variable.to} = squid.lib.MutVar(${nullValue[A]}); $rest"""
  }

  /**
    * Generates init code of one variable of type VarValue
    *
    * @param variable variable of type VarValue
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar2[A, R: CodeType](variable: VarValue[A],
                                       rest: OpenCode[R]): OpenCode[R] = {
    code"val ${variable.variable} = ${variable.init}; $rest"
  }
}
