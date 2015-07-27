import scala.pickling._
import scala.pickling.json._
import scala.pickling.pickler._

final case class Person(name: String, age: Int)

object TestOne extends scala.scalajs.js.JSApp with AllPicklers {

  def main(): Unit = {
    val x: Double = 2.3

    val p = functions.pickle(x)
    println(p.toString)

    val up = functions.unpickle[Double](p)
    assert(x == up)
    println("success")

    val person = Person("Peter", 50)
    val p2 = functions.pickle(person)
    println(p2.toString)

    val up2 = functions.unpickle[Person](p2)
    assert(up2 == person)
    println("success")
  }

}
