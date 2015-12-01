
import scala.collection.mutable.ListBuffer
import scala.math.{hypot,pow,min}
import scala.util.Random

object ClosestPair {
  type Pair = (Point, Point)
  type Points = List[Point]

  case class Point(x: Double, y: Double)

  object Point {
    def distance(p1: Point, p2: Point) = hypot(p1.x - p2.x, p1.y - p2.y)
    def closest(points: Points): Pair =
      points.combinations(2)
            .collect {case p1 :: p2 :: tail => (p1,p2)}
            .minBy {Pair.distance}
  }

  object Pair {
    def distance: (Pair) => Double = {case (p1,p2) => Point.distance(p1,p2)}
    def closest(pairs: Pair*): Pair = pairs.minBy {distance}
  }

  def splitPair(px: Points, py: Points, delta: Double): Pair = {
    var pair: Pair = (py.head, py.last)
    var distance = delta
    val n = px.size
    val pivot = px(n / 2).x
    val (min, max) = (pivot - delta, pivot + delta)
    for (i <- py.indices if min < py(i).x && py(i).x < max)
      for (j <- i to i + 7 if j < py.size) {
        val d = Point.distance(py(i), py(j))
        if (0 < d && d < distance) {
          distance = d
          pair = (py(i), py(j))
        }
      }
    pair
  }

  def closestPair(px: Points, py: Points): Pair = {
    val n = px.size
    if (n < 4) {
      Point.closest(px)
    } else {
      val (qx, rx) = px.splitAt(n / 2)
      val (qy, ry) = py.partition(_.x <= qx.last.x)
      val p1 = closestPair(qx, qy)
      val p2 = closestPair(rx, ry)
      val delta = min(Pair.distance(p1),Pair.distance(p2))
      val p3 = splitPair(px, py, delta)
      Pair.closest(p1, p2, p3)
    }
  }

  def closestPair(points: Points): Pair = {
    val px = points.sortBy(_.x)
    val py = points.sortBy(_.y)
    closestPair(px, py)
  }

  def closestPairNaive(p: Points): Pair = {
    var distance = Double.MaxValue
    var pair = (p.head, p.last)
    for (i <- p.indices)
      for(j <- p.indices if j != i)
        if (Point.distance(p(i), p(j)) < distance) {
          distance = Point.distance(p(i), p(j))
          pair = (p(i), p(j))
        }
    pair
  }

  def main (args: Array[String]) {
    val size = pow(10, 3).toInt
    val max = pow(10, 3)
    val points = ListBuffer[Point]()
    def rand = () => Random.nextDouble()
    def time = () => System.currentTimeMillis() / 1000.0

    for(i <- 0 until size) {
      val (x, y) = (rand() * max, rand() * max)
      points += Point(x, y)
    }

    var (start, stop) = (0.0, 0.0)

    start = time()
    val pair = closestPair(points.toList)
    stop = time()
    println(s"pair: $pair | distance: ${Pair.distance(pair)} | duration: ${stop - start} sec")

    start = time()
    val pairNaive = closestPairNaive(points.toList)
    stop = time()
    println(s"pair: $pairNaive | distance: ${Pair.distance(pairNaive)} | duration: ${stop - start} sec")
  }
}