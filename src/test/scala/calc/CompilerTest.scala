package calc

import fastparse.core.Parsed
import org.junit.Test
import org.junit.Assert._
import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._

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
      ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = true))
  }

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
  }

  @Test def compileBinaryOpPlus(): Unit = {
    assertCompile(irt.JSBinaryOp(irt.BinaryOp.Double_+, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      BinaryOp("+", Literal(4), Literal(5)))
  }

  @Test def compileBinaryOpMinus(): Unit = {
    assertCompile(irt.JSBinaryOp(irt.BinaryOp.Double_-, irt.DoubleLiteral(10), irt.DoubleLiteral(4)),
      BinaryOp("-", Literal(10), Literal(4)))
  }

  @Test def compileBinaryOpMul(): Unit = {
    assertCompile(irt.JSBinaryOp(irt.BinaryOp.Double_*, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      BinaryOp("*", Literal(4), Literal(5)))
  }

  @Test def compileBinaryOpDiv(): Unit = {
    assertCompile(irt.JSBinaryOp(irt.BinaryOp.Double_/, irt.DoubleLiteral(4), irt.DoubleLiteral(5)),
      BinaryOp("/", Literal(4), Literal(5)))
  }

  @Test def compileNestedBinaryOp(): Unit = {
    assertCompile(irt.JSBinaryOp(irt.BinaryOp.Double_/,
      irt.JSBinaryOp(irt.BinaryOp.Double_*, irt.DoubleLiteral(7), irt.DoubleLiteral(8)),
      irt.DoubleLiteral(5)),
      BinaryOp("/", BinaryOp("*", Literal(7), Literal(8)), Literal(5)))
  }

  @Test def compileLetExpression(): Unit = {
    val letIdent = "x"
    assertCompile(irt.Block(),
      Let(Ident(letIdent), Literal(7), BinaryOp("+", Ident(letIdent), Literal(6))))
  }


}
