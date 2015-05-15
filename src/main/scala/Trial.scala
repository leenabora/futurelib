import scala.concurrent.ExecutionContext.Implicits.global

object Trial extends App {
  val ee= scala.concurrent.Future {
    if (true)
      throw new NullPointerException
  }
  println(ee.value)
}
