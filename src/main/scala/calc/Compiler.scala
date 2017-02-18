package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  case class BranchTypeException(msg: String) extends Exception
  final val MainObjectFullName = "main.Main"

  private final val MainClassFullName = MainObjectFullName + "$"

  /** Compile an expression tree into a full `ClassDef`.
   *
   *  You do not need to modify this method.
   */
  def compileMainClass(tree: Tree): irt.ClassDef = {
    implicit val pos = tree.pos

    val className = encodeClassName(MainClassFullName)
    val classType = irtpe.ClassType(className)

    val ctorDef = irt.MethodDef(static = false,
        irt.Ident("init___", Some("<init>")), Nil, irtpe.NoType,
        irt.Block(List(
            irt.ApplyStatically(irt.This()(classType),
                irtpe.ClassType(ObjectClass),
                irt.Ident("init___", Some("<init>")),
                Nil)(
                irtpe.NoType),
            irt.StoreModule(classType, irt.This()(classType)))))(
        irt.OptimizerHints.empty, None)

    val body = compileExpr(tree)(Map[String, irtpe.Type]())
    val methodDef = irt.MethodDef(static = false,
        irt.Ident("main__D", Some("main")), Nil, irtpe.DoubleType, body)(
        irt.OptimizerHints.empty, None)

    val exportedMethodDef = irt.MethodDef(static = false,
        irt.StringLiteral("main"), Nil, irtpe.AnyType,
        irt.Apply(irt.This()(classType), irt.Ident("main__D", Some("main")),
            Nil)(irtpe.DoubleType))(
        irt.OptimizerHints.empty, None)

    val exportedModuleDef = irt.ModuleExportDef(MainObjectFullName)

    val allDefs = List(ctorDef, methodDef, exportedMethodDef, exportedModuleDef)

    val classDef = irt.ClassDef(
        irt.Ident(className),
        ir.ClassKind.ModuleClass,
        Some(irt.Ident(ObjectClass)),
        Nil,
        None,
        allDefs)(
        irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree)(implicit envType: Map[String, irtpe.Type]): irt.Tree = {
    implicit val pos = tree.pos

    tree match {
      case Literal(value) =>
        irt.DoubleLiteral(value)

      case BinaryOp(op, lhs, rhs) =>
        val bOp = op match {
          case "*" => irt.BinaryOp.Double_*
          case "/" => irt.BinaryOp.Double_/
          case "+" => irt.BinaryOp.Double_+
          case "-" => irt.BinaryOp.Double_-
        }
        irt.BinaryOp(bOp, compileExpr(lhs), compileExpr(rhs))

      case Let(name, value, body) =>
        val t = compileExpr(value)
        val q = irt.VarDef(irt.Ident(name.name), t.tpe, mutable = false, t)
        irt.Block(List(q, compileExpr(body)(envType + (name.name -> t.tpe))))

      case Ident(name) =>
        irt.VarRef(irt.Ident(name))(envType(name))

      case If(cond, thenp, elsep) =>
        val thenBranch = compileExpr(thenp)
        val elseBranch = compileExpr(elsep)
        val condTree = compileExpr(cond)

        if(thenBranch.tpe != elseBranch.tpe)
          throw new BranchTypeException(s"Branches do not have the same type ${thenBranch.tpe}, ${elseBranch.tpe}")

        val doubleCond = irt.BinaryOp(irt.BinaryOp.!==, condTree, irt.DoubleLiteral(0.0))
        irt.If(doubleCond, thenBranch, elseBranch)(thenBranch.tpe)

      case _ =>
        throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
