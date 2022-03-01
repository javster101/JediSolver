import java.awt.{Color, Graphics, Graphics2D, BorderLayout}
import javax.swing.{JComponent, JFrame, WindowConstants}
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.immutable.StringOps
import scala.math.pow

object Solver {
  type Vec2 = (Double, Double)

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

  def calcRand(count: Int): List[(Int, Int)] = {
    var items: List[(Int, Int)] = List((0, 0));

    for (x <- 1 to count) {
      items = items :+ (count, nextRand(items(x - 1)._2))
    }

    items
  }

  def pick(points: List[Vec2], seed: Int): (Int, List[Vec2]) = {
    var items: List[(Double, Double)] = List()
    var rand: Int = seed

    for (x <- 0 to 6) {
      rand = nextRand(rand)

      var idx = rand % 121
      while (items.contains(points(idx))) {
        idx = (idx + 1) % 121
      }

      items = items :+ points(idx)
    }

    (rand, items)
  }

  def calculateCost(points: List[Vec2]): Double =
    points
      .sliding(2)
      .map(p => (p.head._1 - p.last._1) ** 2 + (p.head._2 - p.last._2) ** 2)
      .sum

  def getPathsCosts(points: List[Vec2], start: Vec2): Seq[(Seq[Vec2], Double)] =
    points
      .permutations
      .map(start +: _)
      .map(p => (p, calculateCost(p)))
      .toSeq

  def getPathsByEnding(points: List[Vec2], start: Vec2): Map[Vec2, (Seq[Vec2], Double)] =
    getPathsCosts(points, start)
      .toList
      .groupBy(_._1.last)
      .view.mapValues(p => p.minBy(_._2))
      .toMap

  def optimalPairBySeed(points: List[Vec2], seed: Int): (Seq[Vec2], Seq[Vec2], Double) = {
    val paths = getAll(points, seed)

    val firstPath = getPathsByEnding(paths._1, (5.15, -5))

    paths._1
      .map(p => (p, getPathsCosts(paths._2, p).minBy(_._2)))
      .map(p => (firstPath(p._1), p._2))
      .map(p => (p._1._1, p._2._1, p._1._2 + p._2._2))
      .minBy(_._3)
  }

  def getAll(points: List[Vec2], seed: Int): (List[Vec2], List[Vec2]) = {
    val firstSet = pick(points, seed);
    val secondSet = pick(points, firstSet._1);

    (firstSet._2, secondSet._2)
  }

  def main(args: Array[String]): Unit = {
    val GOAL = 300;

    val points = Source.fromFile("jedibattleinitial.csv")
      .getLines()
      .map(line => line.split(","))
      .map(line => (line(0).toDouble, line(1).toDouble))
      .toList

    var seed = 0

    var best: ((Int, Int), (Seq[(Vec2)], Seq[(Vec2)], Double)) = ((0, 0), optimalPairBySeed(points, 0))
    var worst: ((Int, Int), (Seq[(Vec2)], Seq[(Vec2)], Double)) = ((0, 0), optimalPairBySeed(points, 0))

    val max = 1_000_000;
    for (x <- 1 to max) {
      seed = nextRand(seed)

      val path = optimalPairBySeed(points, seed)

      if (path._3 < best._2._3) {
        best = ((x, seed), path)
      }

      if (path._3 > worst._2._3) {
        worst = ((x, seed), path)
      }

      if (x % (max / 1000) == 0) {
        println(x + " " + (x.asInstanceOf[Double] / max.asInstanceOf[Double]))
      }
    }

    println("Best: ")
    println(best._1)

    println(best._2._1.map(p => points.indexOf(p)))
    println(best._2._2.map(p => points.indexOf(p)))
    println(best._2._3)

    println("Worst: ")
    println(worst._1)

    println(worst._2._1.map(p => points.indexOf(p)))
    println(worst._2._2.map(p => points.indexOf(p)))
    println(worst._2._3)

    def toVisible(point: Vec2): (Int, Int) =
      (((point._1 + 8) * 64).asInstanceOf[Int], ((point._2 + 8) * 64).asInstanceOf[Int])


    def drawPoints(p1: Seq[Vec2], p2: Seq[Vec2], g2: Graphics2D, c1: Color, c2: Color): Unit = {
      g2.setColor(c1)
      p1.sliding(2).foreach(ps => {
        g2.drawLine(toVisible(ps.head)._1, toVisible(ps.head)._2, toVisible(ps.last)._1, toVisible(ps.last)._2)
      })

      g2.setColor(c2)
      p2.sliding(2).foreach(ps => {
        g2.drawLine(toVisible(ps.head)._1, toVisible(ps.head)._2, toVisible(ps.last)._1, toVisible(ps.last)._2)
      })
    }

    def createContentPane =
      new JComponent {
        override def paintComponent(g: Graphics): Unit = {
          super.paintComponent(g)

          val g2 = g.create.asInstanceOf[Graphics2D]
          g2.setColor(Color.WHITE)
          g2.setBackground(Color.BLACK)

          g2.clearRect(0, 0, 6000, 6000)
          points.foreach(p => {
            g2.fillRect(toVisible(p)._1, toVisible(p)._2, 3, 3)
          })

          drawPoints(best._2._1, best._2._2, g2, Color.GREEN, Color.CYAN)
          drawPoints(worst._2._1, worst._2._2, g2, Color.RED, Color.YELLOW)
          g2.dispose
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
