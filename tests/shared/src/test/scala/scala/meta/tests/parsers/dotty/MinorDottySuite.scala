package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html
   */
  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) =
      templStat("open class A {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) =
      templStat("open trait C {}")(dialects.Dotty)

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) =
      templStat("open object X {}")(dialects.Dotty)

  }

  test("open-class-negative-cases") {
    runTestError[Stat]("final open class A {}", "illegal combination of modifiers: open and final")
    runTestError[Stat](
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed"
    )
    runTestError[Stat]("open def f(): Int = 3", "error: expected start of definition")
    runTestError[Stat]("def f(open a: Int): Int = 3", "error")
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(
        List(pparam("X"), pparam("Y")),
        Type.Apply(pname("Map"), List(pname("Y"), pname("X")))
      )
    )
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(
        List(
          Type
            .Param(Nil, pname("X"), Nil, Type.Bounds(Some(pname("L")), Some(pname("U"))), Nil, Nil)
        ),
        pname("R")
      )
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(
      Type.Lambda(
        List(pparam("X")),
        Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
      )
    )
  }

  test("case-classes-empty-plist") {
    templStat("case class A()")(dialects.Dotty)
    templStat("case class A @deprecated() ()")(dialects.Dotty)
    templStat("case class A private ()")(dialects.Dotty)
  }

  test("xml-literals") {
    intercept[TokenizeException] { term("<foo>{bar}</foo>")(dialects.Dotty) }
  }

  test("opaque-type-alias") {
    runTestAssert[Stat]("opaque type F = X")(
      Defn.OpaqueTypeAlias(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        Type.Bounds(None, None),
        pname("X")
      )
    )(parseTempl)
  }

  test("opaque-type-bounded-alias") {
    runTestAssert[Stat]("opaque type F <: A & B = AB")(
      Defn.OpaqueTypeAlias(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        Type.Bounds(None, Some(Type.And(pname("A"), pname("B")))),
        pname("AB")
      )
    )(parseTempl)
  }

  test("opaque-type-in-object") {
    runTestAssert[Source]("object X { opaque type IArray[+T] = Array }")(
      Source(
        List(
          Defn.Object(
            Nil,
            tname("X"),
            tpl(
              List(
                Defn.OpaqueTypeAlias(
                  List(Mod.Opaque()),
                  pname("IArray"),
                  List(
                    Type.Param(
                      List(Mod.Covariant()),
                      Type.Name("T"),
                      Nil,
                      Type.Bounds(None, None),
                      Nil,
                      Nil
                    )
                  ),
                  Type.Bounds(None, None),
                  pname("Array")
                )
              )
            )
          )
        )
      )
    )(parseSource)
  }

  test("opaque-type-mix-mods") {
    runTestAssert[Stat]("object X { private opaque type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.OpaqueTypeAlias(
              List(Mod.Private(Name("")), Mod.Opaque()),
              Type.Name("T"),
              Nil,
              Type.Bounds(None, None),
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
    runTestAssert[Stat]("object X { opaque private type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.OpaqueTypeAlias(
              List(Mod.Opaque(), Mod.Private(Name(""))),
              Type.Name("T"),
              Nil,
              Type.Bounds(None, None),
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
  }

  test("trait-parameters") {
    runTestAssert[Stat]("trait Foo(val foo: Int)(bar: Int)")(
      Defn.Trait(
        Nil,
        pname("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(
            List(tparamval("foo", "Int")),
            List(tparam("bar", "Int"))
          )
        ),
        tpl(Nil)
      )
    )
  }

  test("trait-parameters-generic") {
    runTestAssert[Stat]("trait Foo[T](bar: T)")(
      Defn.Trait(Nil, pname("Foo"), List(pparam("T")), ctorp(List(tparam("bar", "T"))), tpl(Nil))
    )
  }

  test("vararg-class-parameter") {
    runTestAssert[Stat]("class Tpe(expected: Type, addend: => String*)")(
      Term.EndMarker(Term.Name("token"))
    )(parseTempl)
  }


  test("end-marker") {
    runTestAssert[Stat]("end token")(
      Term.EndMarker(Term.Name("token"))
    )(parseTempl)
  }

  test("end-marker-keyword") {
    val markers = List("if", "while", "for", "match", "try", "new", "this", "given", "extension", "val")
    for (m <- markers) {
      parseTempl(s"end ${m}")
    }
  }

  test("end-marker-toplevel") {
    val code = """|object a:
                  |  val x = 3
                  |end a
                  |
                  |type K = Map
                  |""".stripMargin
    runTestAssert[Source](code, assertLayout = None)(
      Source(List(Defn.Object(Nil, Term.Name("a"), tpl(List(
        Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(3))))),
         Term.EndMarker(Term.Name("a")), Defn.Type(Nil, Type.Name("K"), Nil, Type.Name("Map"))))
    )(parseSource)
  }

  test("trait-extends-coma-separated") {
    runTestAssert[Stat]("trait Foo extends A, B, C", assertLayout = Some("trait Foo extends A with B with C"))(
      Defn.Trait(Nil, Type.Name("Foo"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, List(init("A"), init("B"), init("C")), Self(Name(""), None), Nil))
    )(parseTempl)
  }

  test("super-trait") {
    runTestAssert[Stat]("super trait Foo")(
      Defn.Trait(List(Mod.Super()), Type.Name("Foo"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), Nil))
    )(parseTempl)
  }

  test("class-parameters-using") {
    runTestAssert[Stat]("trait A(using String)")(
      Defn.Trait(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("class A(using String)")(
      Defn.Class(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("case class A(a: Int)(using b: String)")(
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        Ctor.Primary(Nil, anon, List(List(tparam("a", "Int")), List(tparamUsing("b", "String")))),
        tpl(Nil)
      )
    )
  }

  test("trait-extends-coma-separated") {
    runTestAssert[Stat](
      "trait Foo extends A, B, C",
      assertLayout = Some("trait Foo extends A with B with C")
    )(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, List(init("A"), init("B"), init("C")), Self(Name(""), None), Nil)
      )
    )(parseTempl)

    runTestAssert[Stat](
      "(new A(), new B())"
    )(
      Term.Tuple(
        List(
          Term.New(Init(Type.Name("A"), Name(""), List(List()))),
          Term.New(Init(Type.Name("B"), Name(""), List(List())))
        )
      )
    )(parseTempl)

  }

  test("super-trait") {
    runTestAssert[Stat]("super trait Foo")(
      Defn.Trait(
        List(Mod.Super()),
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )(parseTempl)
  }
}
