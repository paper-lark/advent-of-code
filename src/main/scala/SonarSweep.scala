import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

@main def sonarSweep(): Unit =
  var increases: Int = 0
  val measurements = mutable.Queue[Int]()
  var shouldRead = true

  while shouldRead do
    Try(StdIn.readInt()) match
      case Success(i) =>
        if measurements.size == 3 then
          val oldSum = measurements.sum
          val newSum = oldSum - measurements.front + i
          if oldSum < newSum then
            increases += 1
          measurements.dequeue()

        measurements.enqueue(i)

      case Failure(_) => shouldRead = false

  println(s"Total increases: $increases")
