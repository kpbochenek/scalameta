package traits

trait T/*<=traits.T#*/ {/*<=traits.T#$init$().*/
  def x/*<=traits.T#x().*/ = 2
}

sealed trait U/*<=traits.U#*/
object U/*<=traits.U.*/ {
  def u/*<=traits.U.u().*/: U/*=>traits.U#*/ = new /*<=local0*/U/*=>local0*/ /*=>java.lang.Object#`<init>`().*/{}
}

class C/*<=traits.C#*/
trait V/*<=traits.V#*/ { self/*<=local1*/: C/*=>traits.C#*/ => }
