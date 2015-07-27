import scala.pickling._
import scala.pickling.json._
import scala.pickling.pickler._

object TestZero extends scala.scalajs.js.JSApp with PrimitivePicklers {

  def main(): Unit = {
    val x: Double = 2.3

    val p = functions.pickle(x)
    println(p.toString)

    val up = functions.unpickle[Double](p)
    assert(x == up)
    println("success")
  }

}
