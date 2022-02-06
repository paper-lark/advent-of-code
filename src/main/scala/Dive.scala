import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

@main def dive(): Unit =
  var vertical: Int = 0
  var horizontal: Int = 0
  var aim: Int = 0
  var shouldRead = true

  while shouldRead do
    StdIn.readLine() match
      case null => shouldRead = false
      case s =>
        val parts = s.split(' ')
        val direction = parts(0)
        val value = parts(1).toInt
        direction match
          case "forward" =>
            vertical += value * aim
            horizontal += value
          case "up" => aim -= value
          case "down" => aim += value
          case _ => throw IllegalArgumentException(s"Invalid direction=$direction")

  println(s"Vertical: $vertical, horizontal: $horizontal")
  println(s"Result: ${vertical * horizontal}")
