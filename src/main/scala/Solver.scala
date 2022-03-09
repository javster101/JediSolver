import java.awt.{BorderLayout, Color, Graphics, Graphics2D, Image}
import java.util.concurrent.{ExecutorService, Executors}
import javax.swing.{JComponent, JFrame, WindowConstants}
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.immutable.StringOps
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.math.pow
import javax.swing.ImageIcon
import java.awt.BasicStroke

object Solver {
  case class Vec2 (
    x: Double,
    y: Double
  )

  case class Path (
    part1: Array[Int],
    part2: Array[Int],
    cost: Double
  )

  case class Result (
    indexData: (Int, Int),
    path: Path
  )

  implicit class PowerInt(val i:Double) extends AnyVal {
    def ** (exp:Double):Double = pow(i,exp)
  }

  def nextRand(seed: Int): Int = {
    var mid: Int = (seed ^ 0x75bd924) * 0x41a7 + ((seed ^ 0x75bd924) / 0x1f31d) * -0x7fffffff;

    if (mid < 0) {
      mid = mid + 0x7fffffff;
    }

    mid ^ 0x75bd924;
  }

  def pick(seedIndex: Int, seedsArray: Array[Int]): Array[Int] = {
    val items = new Array[Int](6)
    var rand: Int = 0

    for (x <- 0 until 6) {
      rand = seedsArray(seedIndex + x + 1)

      var idx = rand % 121
      while (items.contains(idx)) {
        idx = (idx + 1) % 121
      }

      items(x) = idx
    }

    items
  }

  def distance(p1: Vec2, p2: Vec2): Double = (p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2

  def calculateCost(points: Array[Int], costs: Array[Double]): Double =
    points
      .sliding(2)
      .map(p => costs(p.head * 122 + p.last))
      .sum

  def getPathsCosts(points: Array[Int], start: Int,  costs: Array[Double]): Seq[(Array[Int], Double)] =
    points
      .permutations
      .map(start +: _)
      .map(p => (p, calculateCost(p, costs)))
      .toSeq

  def getPathsByEnding(points: Array[Int], start: Int,  costs: Array[Double]): Map[Int, (Array[Int], Double)] =
    getPathsCosts(points, start, costs)
      .toList
      .groupBy(_._1.last)
      .view.mapValues(p => p.minBy(_._2))
      .toMap

  def optimalPairBySeed(seedIndex: Int, costs: Array[Double], seedsArray: Array[Int]): Path = {
    val paths = getAll(seedIndex, seedsArray)

    val firstPath = getPathsByEnding(paths._1, 121, costs)

    paths._1
      .map(p => (p, getPathsCosts(paths._2, p, costs).minBy(_._2)))
      .map(p => (firstPath(p._1), p._2))
      .map(p => Path(p._1._1, p._2._1, p._1._2 + p._2._2))
      .minBy(_.cost)
  }

  def getAll(seedIndex: Int, seedsArray: Array[Int]): (Array[Int], Array[Int]) = {
    val firstSet = pick(seedIndex, seedsArray);
    val secondSet = pick(seedIndex + 6, seedsArray);

    (firstSet, secondSet)
  }

  def getMinMax(threadIdx: Int, startIdx: Int, endIdx: Int, costs: Array[Double], seedsArray: Array[Int]): (Result, Result) =
  {
    val count = endIdx - startIdx

    var best: Result = Result((0, 0), optimalPairBySeed(0, costs, seedsArray))
    var worst: Result = Result((0, 0), optimalPairBySeed(0, costs, seedsArray))

    for (x <- startIdx until endIdx) {
      val path = optimalPairBySeed(x, costs, seedsArray)

      if (path.cost < best.path.cost) {
        best = Result((x, seedsArray(x)), path)
      }

      if (path.cost > worst.path.cost) {
        worst = Result((x, seedsArray(x)), path)
      }

      if (x % (count / 100) == 0) {
        println("Worker " + threadIdx + " at " + x + " (" + ((x - startIdx) * 100 / count) + "%)")
      }
    }

    (best, worst)
  }

  def main(args: Array[String]): Unit = {
    val max = 1200;
    val threadCount = 8

    println("Loading points: ")

    val points = Source.fromFile("jedibattleinitial.csv")
      .getLines()
      .map(line => line.split(","))
      .map(line => Vec2(line(0).toDouble, line(1).toDouble))
      .toList :+ Vec2(5.15, -5.0)

    println("Computing costs")
    val costsArray = points
      .flatMap(p => points.map(p2 => distance(p, p2)))
      .toArray

    println("Computing seeds")
    val seeds = new Array[Int](max + 20)
    seeds(0) = 0;

    for (x <- 1 until max + 20) {
      seeds(x) = nextRand(seeds(x-1))
    }

    println("Calculating paths")

    implicit val ec: ExecutionContext = new ExecutionContext {
      val threadPool: ExecutorService = Executors.newFixedThreadPool(threadCount)

      def execute(runnable: Runnable): Unit = {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable): Unit = {}
    }

    val workers = (Range(0, max, max / threadCount) :+ max)
      .sliding(2)
      .zipWithIndex
      .map(p => Future { getMinMax(p._2, p._1.head, p._1.last, costsArray, seeds) })

    val work = Future.sequence(workers)
    val results = Await.result(work, Duration.Inf).toSeq

    val best = results.minBy(_._1.path.cost)._1
    val worst = results.maxBy(_._2.path.cost)._2

    println()
    println("Best: ")
    println(best.indexData)

    println(best.path.part1.map(p => points(p)).mkString("Array(", ", ", ")"))
    println(best.path.part2.map(p => points(p)).mkString("Array(", ", ", ")"))
    println(best.path.cost)

    println()
    println("Worst: ")
    println(worst.indexData)

    println(worst.path.part1.map(p => points(p)).mkString("Array(", ", ", ")"))
    println(worst.path.part2.map(p => points(p)).mkString("Array(", ", ", ")"))
    println(worst.path.cost)

    def toVisible(point: Vec2): (Int, Int) =
      (((point.x + 11) * 48).asInstanceOf[Int], ((point.y + 11) * 48).asInstanceOf[Int])


    def drawPoints(p1: Seq[Int], p2: Seq[Int], g2: Graphics2D, c1: Color, c2: Color): Unit = {

      g2.setColor(c1)
      p1.sliding(2).foreach(ps => {
        g2.drawLine(1064 - toVisible(points(ps.head))._1, toVisible(points(ps.head))._2, 1064 - toVisible(points(ps.last))._1, toVisible(points(ps.last))._2)
      })

      g2.setColor(c2)
      p2.sliding(2).foreach(ps => {
        g2.drawLine(1064 - toVisible(points(ps.head))._1, toVisible(points(ps.head))._2, 1064 - toVisible(points(ps.last))._1, toVisible(points(ps.last))._2)
      })
    }

    def createContentPane =
      new JComponent {
        val image: Image = new ImageIcon("background.png").getImage
        override def paintComponent(g: Graphics): Unit = {
          super.paintComponent(g)

          val g2 = g.create.asInstanceOf[Graphics2D]
          g2.setStroke(new BasicStroke(4))

          g2.setColor(Color.BLACK)
          g2.setBackground(Color.BLACK)
          g2.clearRect(0, 0, 6000, 6000)

          g2.drawImage(image, 0, 0, null)

          points.foreach(p => {
            g2.fillRect(1064 - toVisible(p)._1, toVisible(p)._2, 3, 5)
          })

          drawPoints(best.path.part1, best.path.part2, g2, Color.GREEN, Color.CYAN)
          drawPoints(worst.path.part1, worst.path.part2, g2, Color.RED, Color.YELLOW)
          g2.dispose()
        }
      }

    val frame = new JFrame(){
      setName("JediSolver")
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLayout(new BorderLayout)
      add(createContentPane)
      pack()
      setSize(600, 600)
      setVisible(true)
    }
  }
}
