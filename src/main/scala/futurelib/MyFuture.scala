package futurelib

import scala.util.{Success, Failure, Try}

object MyFuture {
  def apply[T](t: => T): FutureResultHolder[T] = {
    val bg = new BackgroundRunner[T](t)
    bg.start()
    bg.futureResultHolder
  }
}

class BackgroundRunner[T](body: => T) extends Thread {
  val futureResultHolder = new FutureResultHolder[T]()

  override def run() {
    try {
      futureResultHolder.result = body
      futureResultHolder.isDone = true
    } catch {
      case ex: Throwable =>
        futureResultHolder.isFailed = true
        futureResultHolder.error = ex
    }
  }
}

class FutureResultHolder[T] {
  self =>
  var result: T = _
  var isDone: Boolean = false

  var error: Throwable = new Exception
  var isFailed: Boolean = false

  def get(): Try[T] = {
    execute[Try[T]](Success(result))(Failure(error))
  }

  def map[U](f: T => U): FutureResultHolder[U] = {
    execute {
      MyFuture[U] {
        f(result)
      }
    }(copy[U])
  }

  def flatMap[U](f: T => FutureResultHolder[U]): FutureResultHolder[U] = {
    execute(f(result))(copy[U]())
  }

  def recover(p: PartialFunction[Throwable, T]): FutureResultHolder[T] = {
    execute(this) {
      if (p.isDefinedAt(error)) {
        MyFuture {
          p(error)
        }
      } else {
        throw error
      }
    }
  }

  def recoverWith(p: PartialFunction[Throwable, FutureResultHolder[T]]): FutureResultHolder[T] = {
    execute(this) {
      if (p.isDefinedAt(error)) {
        p(error)
      } else throw error
    }
  }

  private def copy[U]() = {
    new FutureResultHolder[U] {
      isDone = self.isDone
      error = self.error
      isFailed = self.isFailed
    }
  }

  private def execute[U](successBody: => FutureResultHolder[U])(failureBody: => FutureResultHolder[U]): FutureResultHolder[U] = {
    execute[FutureResultHolder[U]](successBody)(failureBody)
  }

  private def execute[U](successBody: => U)(failureBody: => U): U = {
    readyOrFailed()
    if (isDone) successBody else failureBody
  }

  private def readyOrFailed() {
    if (!isDone && !isFailed) {
      Thread.sleep(100)
      readyOrFailed()
    }
  }
}


