import scala.collection.mutable
import scala.io.StdIn
import scala.util.control.Breaks.break
import scala.util.{Failure, Success, Try}

@main def binaryDiagnostic(): Unit =
  var shouldRead = true
  val inputLength = 12
  val ones = Array.fill(inputLength)(0)
  val zeros = Array.fill(inputLength)(0)

  while shouldRead do
    StdIn.readLine() match
      case null => shouldRead = false
      case s => {
        if s.length != inputLength then
          throw IllegalArgumentException(s"received input '$s' with length ${s.length}, expected: $inputLength")

        for ((c, i) <- s.zipWithIndex) {
          c match
            case '1' => ones(i) += 1
            case '0' => zeros(i) += 1
            case _ => throw IllegalArgumentException(s"invalid bit value: $c")
        }
      }

  var gamma = 0
  for (i <- 0 until inputLength)
    gamma <<= 1
    if ones(i) > zeros(i) then
      gamma += 1
  val eps = invertBits(gamma, inputLength)

  println(s"Gamma: $gamma, epsilon: $eps")
  println(s"Power: ${gamma * eps}")


@main def binaryDiagnosticAdvanced(): Unit =
  var shouldRead = true
  val inputLength = 12
  var lines = List[Int]()

  while shouldRead do
    StdIn.readLine() match
      case null => shouldRead = false
      case s => lines = lines ++ List(Integer.parseInt(s, 2))

  def getResult(selector: (zeros: Int, ones: Int) => Int) =
    var filtered = lines
    for (i <- (0 until inputLength).reverse)
      val counters = filtered.foldLeft((0, 0))((acc, n) => if getBitValue(n, i) == 1 then (acc(0), acc(1) + 1) else (acc(0) + 1, acc(1)))
      val value = selector(counters._1, counters._2)
      filtered = filtered.filter(n => getBitValue(n, i) == value)
    filtered.head

  val oxygen = getResult((z, o) => if z > o then 0 else 1)
  val co2 = getResult((z, o) => if z > o && o > 0 || z == 0 then 1 else 0)

  println(s"Oxygen: $oxygen, CO2: $co2")
  println(s"Rating: ${oxygen * co2}")


private def getBitValue(n: Int, i: Int) = (n >> i) & 1

private def invertBits(n: Int, len: Int) = ((0x1 << len) - 1) & ~n
