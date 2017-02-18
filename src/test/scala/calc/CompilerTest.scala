package calc

import org.junit.Assert._
import org.junit.Test
import org.scalajs.core.ir
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.{Trees => irt, Types => irtpe}

/** Tests focused on the Compiler.
  *
  * You can add more "whitebox" tests here. A whitebox test checks that the
  * compiler precisely emits the Trees we expect from some input.
  */
class CompilerTest {

  private implicit val DummyPos = ir.Position.NoPosition

  private val MainObjectFullName = Compiler.MainObjectFullName
  private val MainClassFullName = MainObjectFullName + "$"

  // Could be useful in tests, depending on the trees you generate
  private val classType = irtpe.ClassType(encodeClassName(MainClassFullName))

  private def assertCompile(expected: irt.Tree, sourceTree: Tree): Unit = {
    /* IR Trees do not have a meaningful equals() method, so we test equality
     * through hashes.
     */

    def hashOf(body: irt.Tree): irt.TreeHash = {
      // Can only hash entire methods
      val methodDef = irt.MethodDef(static = false, irt.Ident("main__D"),
        Nil, irtpe.DoubleType, body)(
        irt.OptimizerHints.empty, None)
      ir.Hashers.hashMethodDef(methodDef).hash.get
    }

    val expectedHash = hashOf(expected)
    val actual = Compiler.compileExpr(sourceTree)(Map[String, irtpe.Type]())
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
      ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = false))
  }

  private def parseExpr(expr: String) = Parser.parse(expr).get.value

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.DoubleLiteral(234), parseExpr("234"))
  }

  @Test def compileBinaryOpPlus(): Unit = {
    assertCompile(irt.BinaryOp(irt.BinaryOp.Double_+, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      parseExpr("4+5"))
  }

  @Test def compileBinaryOpMinus(): Unit = {
    assertCompile(irt.BinaryOp(irt.BinaryOp.Double_-, irt.DoubleLiteral(10), irt.DoubleLiteral(4)),
      parseExpr("10-4"))
  }

  @Test def compileBinaryOpMul(): Unit = {
    assertCompile(irt.BinaryOp(irt.BinaryOp.Double_*, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      parseExpr("4*5"))
  }

  @Test def compileBinaryOpDiv(): Unit = {
    assertCompile(irt.BinaryOp(irt.BinaryOp.Double_/, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      parseExpr("4/5"))
  }

  @Test def compileNestedBinaryOp(): Unit = {
    assertCompile(irt.BinaryOp(irt.BinaryOp.Double_/,
      irt.BinaryOp(irt.BinaryOp.Double_*, irt.DoubleLiteral(7), irt.DoubleLiteral(8)),
      irt.DoubleLiteral(5)),
      parseExpr("7*8/5"))
  }

  @Test def compileLetExpression(): Unit = {
    val letIdent = "x"
    assertCompile(
      irt.Block(
        irt.VarDef(
          irt.Ident("x"), irtpe.DoubleType, mutable = false, irt.DoubleLiteral(7.0)
        ),
        irt.BinaryOp(irt.BinaryOp.Double_+, irt.VarRef(irt.Ident("x"))(irtpe.DoubleType), irt.DoubleLiteral(6.0))),
      parseExpr(s"let $letIdent = 7 in $letIdent + 6"))
  }

  @Test def compileIf(): Unit = {
    assertCompile(
      irt.If(
        irt.BinaryOp(irt.BinaryOp.!==, irt.DoubleLiteral(7.0), irt.DoubleLiteral(0.0)),
        irt.DoubleLiteral(5), irt.DoubleLiteral(3))(irtpe.DoubleType), parseExpr("if(7) 5 else 3"))
  }

}
