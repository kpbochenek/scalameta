package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class SignificantIndentationSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val defx = Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int"))
  val defy = Defn.Def(Nil, Term.Name("y"), Nil, Nil, Some(Type.Name("String")), Term.Block(List(Term.Apply(Term.Name("fa"), Nil), Term.Apply(Term.Name("fb"), Nil))))
  val rgbcase = Defn.RepeatedEnumCase(Nil, List(Term.Name("Red"), Term.Name("Green"), Term.Name("Blue")))

  test("basic-example") {
    val code = """|trait A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("trait A { def f: Int }"))(
      Defn.Trait(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), List(
        defx)))
    )
  }

  test("multistat-example") {
    val code = """|trait A:
                  |  def f: Int
                  |  def y: String = { fa(); fb() }
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Trait(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), List(
        defx, defy
        )))
    )
  }

  test("indent-and-back") {
    val code = """|object O:
                  |  class C:
                  |    def f: Int = 3
                  |  trait T:
                  |    def f: Int = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
Defn.Object(Nil, Term.Name("O"), Template(Nil, Nil, Self(Name(""), None), List(
  Defn.Class(Nil, Type.Name("C"), Nil, ctor, Template(Nil, Nil, Self(Name(""), None), List(
    Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(3))))),
   Defn.Trait(Nil, Type.Name("T"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), List(
     Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(4)))))
   )))
    )
  }
  
  test("indent-if") {
    val code = """|if a == true then
                  |  f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit())
    )
  }

  test("indent-if-multiline".only) {
    val code = """|if a == true
                  |  || b == true
                  |then f(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Lit.Unit())
    )
  }

  test("old-if-else") {
    val code = """|if (a == true)
                  |  f(x)
                  |else
                  |  g(x)
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Boolean(true))), Term.Apply(Term.Name("f"), List(Term.Name("x"))), Term.Apply(Term.Name("g"), List(Term.Name("x"))))
    )
  }

  test("old-if-else-oneliner") {
    val code = """if es.length != that.elems.length then return false"""
    runTestAssert[Stat](code, assertLayout = None)(
      Term.If(Term.ApplyInfix(Term.Select(Term.Name("es"), Term.Name("length")), Term.Name("!="), Nil, List(Term.Select(Term.Select(Term.Name("that"), Term.Name("elems")), Term.Name("length")))), Term.Return(Lit.Boolean(false)), Lit.Unit())
    )
  }

  test("indent-if-else") {
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

  test("indent-match-two") {
    val code = """|
                  |x match
                  |  case 1 => "OK"
                  |  case 2 => "ERROR"
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Match(Term.Name("x"), List(Case(Lit.Int(1), None, Lit.String("OK")), Case(Lit.Int(2), None, Lit.String("ERROR"))))
    )
  }

  test("indent-while-do") {
    val code = """|while cond do
                  |  run()
                  |  3
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.While(Term.Name("cond"), Term.Block(List(Term.Apply(Term.Name("run"), Nil), Lit.Int(3))))
    )
  }

  test("indent-try") {
    val code = """|try
                  |  run()
                  |  3
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Try(Term.Block(List(Term.Apply(Term.Name("run"), Nil), Lit.Int(3))), Nil, None)
    )
  }



  test("indent-match-zero".ignore) {
    val code = """|
                  |x match
                  |case 1 => "OK"
                  |case 2 => "ERROR"
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
Term.Match(Term.Name("x"), List(Case(Lit.Int(1), None, Lit.String("OK")), Case(Lit.Int(2), None, Lit.String("ERROR"))))
    )
  }

  test("indent-def-parens") {
    val code = """|object A:
                  |  def f =
                  |    (a ++ b).x
                  |  private def c = 3
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Object(Nil, Term.Name("A"), Template(Nil, Nil, Self(Name(""), None), List(Defn.Def(Nil, Term.Name("f"), Nil, Nil, None, Term.Select(Term.ApplyInfix(Term.Name("a"), Term.Name("++"), Nil, List(Term.Name("b"))), Term.Name("x"))), Defn.Def(List(Mod.Private(Name(""))), Term.Name("c"), Nil, Nil, None, Lit.Int(3)))))
    )
  }

  test("indent-def") {
    val code = """|def f =
                  |  a()
                  |  b()
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Def(Nil, Term.Name("f"), Nil, Nil, None, Term.Block(List(Term.Apply(Term.Name("a"), Nil), Term.Apply(Term.Name("b"), Nil))))
    )
  }

  test("indent-given") {
    val code = """|given String =
                  |  "aaa"
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
Defn.GivenAlias(Nil, Name(""), Nil, Nil, Type.Name("String"), Lit.String("aaa"))
    )
  }

  test("indent-seq") {
    val code = """|class A {
                  |  def x =
                  |    a()
                  |
                  |  def b: String = {
                  |    c()
                  |  }
                  |
                  |  private class T() { }
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), List(Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Term.Apply(Term.Name("a"), Nil)), Defn.Def(Nil, Term.Name("b"), Nil, Nil, Some(Type.Name("String")), Term.Block(List(Term.Apply(Term.Name("c"), Nil)))), Defn.Class(List(Mod.Private(Name(""))), Type.Name("T"), Nil, Ctor.Primary(Nil, Name(""), List(List())), Template(Nil, Nil, Self(Name(""), None), Nil)))))
    )
  }

  test("indent-val") {
    val code = """|val where =
                  |  if cond then 1
                  |  else if c2 then 2
                  |  else 3
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Val(Nil, List(Pat.Var(Term.Name("where"))), None, Term.If(Term.Name("cond"), Lit.Int(1), Term.If(Term.Name("c2"), Lit.Int(2), Lit.Int(3))))
    )
  }




}
