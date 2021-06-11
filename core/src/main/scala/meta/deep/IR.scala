package meta.deep

import squid.ir.SimpleAST
import squid.lang.Definitions

object IR extends SimpleAST with Definitions { self =>
  
  object MethodApplication {
    def unapply[T,C](q: Code[T,C]): Option[MethodApplication[T,C]] = unapplyMethodApplication[T,C](q)
  }
  def unapplyMethodApplication[T,C](q: Code[T,C], tp: Option[TypeRep] = None): Option[MethodApplication[T,C]] = {
    q.rep.dfn match {
      case app: MethodApp => Some(new MethodApplication(app,tp))
      case Ascribe(e,t) => unapplyMethodApplication(Code(e),tp orElse Some(t))
      case _ => None
    }
  }
  class MethodApplication[T,C](private val ma: MethodApp, private val asc: Option[TypeRep]) {
    val symbol = ma.sym
    val args: Seq[Seq[Code[Any,C]]] = List(Code(ma.self)) :: ma.argss.map(_.reps.map(Code.apply[Any,C] _))
    val targs: Seq[CodeType[_]] = ma.targs.map(CodeType.apply)
    def rebuild(argsTransfo: SelfTransformer): Code[T,C] = {
      val res = rep(MethodApp(
        ma.self |> argsTransfo.pipeline,
        ma.sym,
        ma.targs,
        ma.argss.map(_.map(self)(argsTransfo.pipeline)),
        ma.typ
      ))
      Code(asc map (Ascribe.mk(res, _).fold(res)(rep)) getOrElse res)
    }
    override def toString: String = s"${Rep(ma)}"
  }
  
}
