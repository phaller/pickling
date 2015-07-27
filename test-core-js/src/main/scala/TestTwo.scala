import scala.pickling._
import scala.pickling.json._
import scala.pickling.pickler._
import scala.pickling.static._
import scala.pickling.shareNothing._

case class Container(elems: Iterable[String])

object TestTwo extends scala.scalajs.js.JSApp with AllPicklers {

  def main(): Unit = {
    val x: Container = Container(List("a", "c", "b"))

    val p = functions.pickle(x)
    println(p.toString)

    val up = functions.unpickle[Container](p)
    assert(x == up)
    println("success")
  }

}
