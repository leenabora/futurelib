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
      case ex =>
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
    readyOrFailed()
    if (isFailed)
      Failure(error)
    else
      Success(result)
  }

  def map[U](f: T => U): FutureResultHolder[U] = {
    readyOrFailed()
    if (isFailed) {
      new FutureResultHolder[U] {
        isDone = self.isDone
        error = self.error
        isFailed = self.isFailed
      }
    }
    else {
      MyFuture[U] {
        f(result)
      }
    }
  }

  def flatMap[U](f: T => FutureResultHolder[U]): FutureResultHolder[U] = {
    readyOrFailed()
    if (isFailed) {
      new FutureResultHolder[U] {
        isDone = self.isDone
        error = self.error
        isFailed = self.isFailed
      }
    }
    else
      f(result)
  }

  def recover(p: PartialFunction[Throwable, T]): FutureResultHolder[T] = {
    readyOrFailed()
    if (isFailed) {
      if (p.isDefinedAt(error)) {
        MyFuture {
          p(error)
        }
      } else {
        throw error
      }
    } else {
      this
    }
  }

  def recoverWith(p: PartialFunction[Throwable, FutureResultHolder[T]]): FutureResultHolder[T] = {
    readyOrFailed()
    if (isFailed) {
      if (p.isDefinedAt(error)) {
        p(error)
      } else throw error
    } else {
      this
    }
  }

 def execute() = {
   readyOrFailed()
   if(isFailed) {

   }
   else{
     
   }
 }

  def readyOrFailed() {
    if (!isDone && !isFailed) {
      Thread.sleep(100)
      readyOrFailed()
    }
  }
}


