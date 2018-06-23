package ripl.reduce2.ast.typed

import ripl.ast.parse._
import ripl.ast.common._

import ripl.reduce2.ast.untyped._

case object ParseTreeToUntypedAst {

  def apply(exps: List[Exp]): Ast = {

    val ast = Ast(Definitions(), Scopes())

    def mapNamespaceMembers(scope: Scope, members: List[Exp]): Scope = {


      // lazy val contents = members.map(mapNamespaceMember(, e))
      // ???
    }

    def mapNamespaceMember(scope: Scope, e: Exp): (Name, Either[ID, Scope]) = e match {

      case SExp(Name("scope") :: rest) => rest match {
        case (n: Name) :: members => (n, Right(mapNamespaceMembers(scope, members)))
        case _ => ???
            // TODO: This is an error case, the language doesn't presently support
            // computing the names of definitions
      }

      case SExp(Name("define") :: rest) => rest match {
        case (n: Name) :: contents => ???
        case _ => ???
            // TODO: This is an error case, the language doesn't presently support
            // computing the names of definitions
      }

      // case SExp(Name("define") :: Name(n) :: definition) => ???

    }

    ???
  }
}
