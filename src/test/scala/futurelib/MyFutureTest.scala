package futurelib

import org.scalatest._

import scala.util.{Failure, Success}

class MyFutureTest extends FunSuite with Matchers {
  val sleepTime = 300

  val f = MyFuture[Int] {
    val res = 100
    Thread.sleep(sleepTime)
    res
  }

  val npe = new NullPointerException("NPE")
  val ff = MyFuture[Int] {
    val res = 100
    Thread.sleep(sleepTime)
    if (true)
      throw npe
    res
  }

  test("get") {
    val futureValue = f.recover { case _ => 1 }
    futureValue.get should be(Success(100))
  }

  test("get and fail") {
    ff.get should be(Failure(npe))
  }

  test("get and recover") {
    val futureValue = ff.recover { case _ => 1 }
    futureValue.get should be(Success(1))
  }

  test("map") {
    val tf = f.map[Int](x => x * 5)
    tf.get should be(Success(500))
  }

  test("flatmap") {
    def oneMoreTaskInBackground(someInt: Int) = {
      MyFuture[String] {
        Thread.sleep(500)
        someInt + "--FinalResult"
      }
    }

    val tf = f.flatMap[String](x => oneMoreTaskInBackground(x))

    tf.get should be(Success("100--FinalResult"))
  }

  test("map and flatmap") {
    val tf = f.map[String](x => "Mapped=" + x).flatMap[String](x => MyFuture[String] {
      "FlatMapped=" + x
    }).recover { case _ => "failing..." }

    tf.get should be(Success("FlatMapped=Mapped=100"))
  }

  test("recover") {
    val tf = ff.recover {
      case n: NullPointerException => 0
      case e: Exception => -1
    }

    tf.get should be(Success(0))
  }

  test("recover complex") {
    val tf = ff.map[String](x => "--" + x + "--").recover {
      case n: NullPointerException => "NPE"
      case _ => "EX"
    }
    tf.get should be(Success("NPE"))
  }

  test("recover more complex") {
    val tf = ff.map[String](x => "Mapped=" + x).flatMap[String](x => MyFuture[String] {
      "FlatMapped=" + x
    }).recover {
      case n: NullPointerException => "NPE"
      case _ => "EX"
    }
    tf.get should be(Success("NPE"))
  }

  test("recover with") {
    val tf = ff.map[String](x => "--" + x + "--").recoverWith {
      case n: NullPointerException => MyFuture[String] {
        "NPE_IN_BG"
      }
      case _ => MyFuture[String] {
        "EX_IN_BG"
      }
    }
    tf.get should be(Success("NPE_IN_BG"))
  }

  test("for comprehensions") {
    def future1(num: Int) = {
      MyFuture[Int] {
        Thread.sleep(sleepTime)
        num * 10
      }
    }

    def future2(num: Int) = {
      MyFuture[Int] {
        Thread.sleep(sleepTime)
        num + 10
      }
    }

    val result = for {
      firstFuture <- future1(10)
      secondFuture <- future2(firstFuture)
    } yield secondFuture

    result.get should be(Success(110))
  }

}