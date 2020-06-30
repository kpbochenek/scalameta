package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ControlSyntaxSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

    test("indent-if-then") {
    val code = """|if a == true then
                  |  f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit())
    )
  }

  test("indent-if-then-multiline") {
    val code = """|if a == true
                  |  || b == true
                  |then f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(
        Term.ApplyInfix(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Name("||"), Nil, List(Term.ApplyInfix(Term.Name("b"), Term.Name("=="), Nil, List(Lit.Boolean(true))))),
        Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit()
      )
    )
  }

  test("braces-if-else") {
    val code = """|if (a == true)
                  |  f(x)
                  |else
                  |  g(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Term.Apply(Term.Name("g"), List(Term.Name("x"))))
    )
  }

  test("if-else-then-oneline") {
    val code = """if es.length != that.elems.length then return false"""
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Select(Term.Name("es"), Term.Name("length")), Term.Name("!="), Nil, List(Term.Select(Term.Select(Term.Name("that"), Term.Name("elems")), Term.Name("length")))), Term.Return(Lit.Boolean(false)), Lit.Unit())
    )
  }

  test("indent-if-then-else") {
    val code = """|if a == true then
                  |  f(x)
                  |else
                  |  g(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Term.Apply(Term.Name("g"), List(Term.Name("x"))))
    )
  }

  test("old-if-if") {
    val code = """|if (cond) a
                  |else {
                  |  if (cond2)
                  |    c
                  |  else
                  |    d
                  |} 
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.Name("cond"), Term.Name("a"), Term.Block(List(Term.If(Term.Name("cond2"), Term.Name("c"), Term.Name("d")))))
    )
  }

  test("indent-if-if") {
    val code = """|run {
                  |  if test1 then
                  |    a
                  |  if test2 then
                  |    b
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Apply(Term.Name("run"), List(Term.Block(List(Term.If(Term.Name("test1"), Term.Name("a"), Lit.Unit()), Term.If(Term.Name("test2"), Term.Name("b"), Lit.Unit())))))
    )
  }

  test("indent-if-else-multiexpr") {
    val code = """|if a == true then
                  |  f(x)
                  |  println("OK")
                  |else
                  |  g(x)
                  |  println("ERROR")
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Block(List(Term.Apply(Term.Name("f"), List(Term.Name("x"))), Term.Apply(Term.Name("println"), List(Lit.String("OK"))))), Term.Block(List(Term.Apply(Term.Name("g"), List(Term.Name("x"))), Term.Apply(Term.Name("println"), List(Lit.String("ERROR"))))))
    )
  }

  test("indent-if-partial-parens") {
    val code = """|if (a) || (b) then f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("||"), Nil, List(Term.Name("b"))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit())
    )
  }

  test("indent-if-partial-parens-multiline") {
    val code = """|if (a)
                  |  || (b)
                  |then f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("||"), Nil, List(Term.Name("b"))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit())
    )

  }
}
