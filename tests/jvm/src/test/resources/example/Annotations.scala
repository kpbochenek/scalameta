package annot

import com.javacp.annot._
import scala.annotation.meta._
import scala.language/*=>scala.language.*/.experimental/*=>scala.language.experimental.*/.macros/*=>scala.language.experimental.macros.*/

@ClassAnnotation/*=>com.javacp.annot.ClassAnnotation#*//*=>com.javacp.annot.ClassAnnotation#`<init>`().*/
class Annotations/*<=annot.Annotations#*/[@TypeParameterAnnotation/*=>com.javacp.annot.TypeParameterAnnotation#*/ T/*<=annot.Annotations#[T]*/](@ParameterAnnotation/*=>com.javacp.annot.ParameterAnnotation#*/ /*=>com.javacp.annot.ParameterAnnotation#`<init>`().*/x/*<=annot.Annotations#x.*/: T/*=>annot.Annotations#[T]*/) { self/*<=local0*/: AnyRef =>
  @FieldAnnotation/*=>com.javacp.annot.FieldAnnotation#*//*=>com.javacp.annot.FieldAnnotation#`<init>`().*/
  val field/*<=annot.Annotations#field.*/ = 42

  @MethodAnnotation/*=>com.javacp.annot.MethodAnnotation#*//*=>com.javacp.annot.MethodAnnotation#`<init>`().*/
  def method/*<=annot.Annotations#method().*/ = {
    @LocalAnnotation/*=>com.javacp.annot.LocalAnnotation#*//*=>com.javacp.annot.LocalAnnotation#`<init>`().*/
    val local/*<=local1*/ = 42
    local/*=>local1*/
  }
  @TypeAnnotation/*=>com.javacp.annot.TypeAnnotation#*//*=>com.javacp.annot.TypeAnnotation#`<init>`().*/
  type T/*<=annot.Annotations#T#*/
}

class B/*<=annot.B#*/ @ConstructorAnnotation/*=>com.javacp.annot.ConstructorAnnotation#*//*=>com.javacp.annot.ConstructorAnnotation#`<init>`().*/() (x/*<=annot.B#x.*/: Int/*=>scala.Int#*/) {
  @ConstructorAnnotation/*=>com.javacp.annot.ConstructorAnnotation#*//*=>com.javacp.annot.ConstructorAnnotation#`<init>`().*/
  def this() = this(42)
}

@ObjectAnnotation/*=>com.javacp.annot.ObjectAnnotation#*//*=>com.javacp.annot.ObjectAnnotation#`<init>`().*/
object M/*<=annot.M.*/ {
  @MacroAnnotation/*=>com.javacp.annot.MacroAnnotation#*//*=>com.javacp.annot.MacroAnnotation#`<init>`().*/
  def m/*<=annot.M.m().*/[TT/*<=annot.M.m().[TT]*/]: Int/*=>scala.Int#*/ = macro ???/*=>scala.Predef.`???`().*/
}

@TraitAnnotation/*=>com.javacp.annot.TraitAnnotation#*//*=>com.javacp.annot.TraitAnnotation#`<init>`().*/
trait T/*<=annot.T#*/

object Alias/*<=annot.Alias.*/ {
  type A/*<=annot.Alias.A#*/ = ClassAnnotation/*=>com.javacp.annot.ClassAnnotation#*/ @param
}
