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
import java.util.Calendar
import java.time.Instant
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.format.FormatStyle
import java.util.Locale
import java.time.ZoneId
import java.nio.file.{Files, Path}
import java.nio.file.Paths
import org.lwjgl.system.MemoryUtil
import org.lwjgl.system.MemoryStack
import scala.util.Using
import org.lwjgl.opencl.CL10
import org.lwjgl.opencl.CL
import java.nio.IntBuffer
import org.lwjgl.opencl.CLCapabilities
import scala.util.Try
import org.lwjgl.opencl.CLProgramCallback
import java.nio.ByteBuffer
import java.awt.image.BufferedImage
import java.io.File

object Solver {
  case class Vec2 (
    x: Double,
    y: Double
  )

  case class Solution (
    part1: Array[Int],
    part2: Array[Int],
    cost: Double
  )

  case class Result (
    indexData: (Int, Int),
    path: Solution
  )

  case class CLInfo (
    platform: Long,
    capabilities: CLCapabilities,
    device: Long,
    context: Long,
    queue: Long
  )

  implicit class PowerInt(val i:Double) extends AnyVal {
    def ** (exp:Double):Double = pow(i,exp)
  }

  def nextRand(seed: Int): Int = {
    var mid: Int = (seed ^ 0x75bd924) * 0x41a7 + ((seed ^ 0x75bd924) / 0x1f31d) * -0x7fffffff;
    if mid < 0 then (mid + 0x7fffffff) ^ 0x75bd924 else mid ^ 0x75bd924
  }

  def nextNRand(seed: Int, count: Int): Array[Int] = {
    val seeds = new Array[Int](count + 16)
    seeds(0) = seed;

    for (x <- 1 until count + 16) {
      seeds(x) = nextRand(seeds(x-1))
    }

    seeds
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

  def distance(p1: Vec2, p2: Vec2): Double = Math.sqrt((p1.x - p2.x) ** 2 + (p1.y - p2.y) ** 2)

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

  def optimalPairBySeed(seedIndex: Int, costs: Array[Double], seedsArray: Array[Int]): Solution = {
    val paths = getAll(seedIndex, seedsArray)

    val firstPath = getPathsByEnding(paths._1, 121, costs)

    paths._1
      .map(p => (p, getPathsCosts(paths._2, p, costs).minBy(_._2)))
      .map(p => (firstPath(p._1), p._2))
      .map(p => Solution(p._1._1, p._2._1, p._1._2 + p._2._2))
      .minBy(_.cost)
  }

  def getAll(seedIndex: Int, seedsArray: Array[Int]): (Array[Int], Array[Int]) = 
    (pick(seedIndex, seedsArray), pick(seedIndex + 8, seedsArray))

  def getMinMax(threadIdx: Int, startIdx: Int, endIdx: Int, costs: Array[Double], seedsArray: Array[Int]): (Result, Result) =
  {
    val percentPrint = 10

    val count = endIdx - startIdx

    var best: Result = Result((0, 0), Solution(Array(), Array(), Double.MaxValue))
    var worst: Result = Result((0, 0), Solution(Array(), Array(), Double.MaxValue))

    for (x <- startIdx until endIdx) {
      val path = optimalPairBySeed(x, costs, seedsArray)
      
      if (path.cost < best.path.cost) {
        best = Result((x, seedsArray(x)), path)
      }

      if (path.cost > worst.path.cost) {
        worst = Result((x, seedsArray(x)), path)
      }

      if (x % (count / percentPrint) == 0) {
       // println("Worker " + threadIdx + " at " + x + 
        //  " (" + ((x - startIdx) * 100 / count) + "%)")
      }
    }

    (best, worst)
  }

  def checkCLError(errcode: IntBuffer): Unit = {
    checkCLError(errcode.get(errcode.position()));
  }

  def checkCLError(errcode: Int): Unit = {
    if (errcode != CL10.CL_SUCCESS) {
      throw new RuntimeException(String.format("OpenCL error [%d]", errcode));
    }
  }

  def getCLDevice(platform: Long, platformCaps: CLCapabilities, deviceType: Int): Try[Long] = Using(MemoryStack.stackPush()) { stack =>  
    val pi = stack.mallocInt(1);
    checkCLError(CL10.clGetDeviceIDs(platform, deviceType, null, pi))

    val devices = stack.mallocPointer(pi.get(0))
    checkCLError(CL10.clGetDeviceIDs(platform, deviceType, devices, null.asInstanceOf[IntBuffer]))
    devices.get(0)
  }

  def initializeOpenCL(): Try[CLInfo] = Using(MemoryStack.stackPush()) { stack => 
    val pi = stack.mallocInt(1)
    checkCLError(CL10.clGetPlatformIDs(null, pi))
    if (pi.get(0) == 0) {
        throw new IllegalStateException("No OpenCL platforms found.");
    }

    val platformIDs = stack.mallocPointer(pi.get(0))
    checkCLError(CL10.clGetPlatformIDs(platformIDs, null.asInstanceOf[IntBuffer]))

    val platform = platformIDs.get(0)
    val clPlatformCapabilities = CL.createPlatformCapabilities(platform)
    val device = getCLDevice(platform, clPlatformCapabilities, CL10.CL_DEVICE_TYPE_GPU).get
    val contextProps = MemoryUtil.memAllocPointer(7).put(CL10.CL_CONTEXT_PLATFORM).put(platform).put(0).flip() 
    val context = CL10.clCreateContext(contextProps, device, (error, privInfo, cb, userData) => println("Error"), 0, pi)
    checkCLError(pi) 
    
    val queue = CL10.clCreateCommandQueue(context, device, 0, pi)
    checkCLError(pi) 

    CLInfo(platform, clPlatformCapabilities, device, context, queue)
  }
  
  def getProgramString(program: Long, device: Long, param: Int): Try[String] = Using(MemoryStack.stackPush()) { stack => 
    val pp = stack.mallocPointer(1)
    checkCLError(CL10.clGetProgramBuildInfo(program, device, param, null.asInstanceOf[ByteBuffer], pp));
    
    val bytes = pp.get(0).asInstanceOf[Int]
    val buffer = stack.malloc(bytes)
    checkCLError(CL10.clGetProgramBuildInfo(program, device, param, buffer, null))

    MemoryUtil.memASCII(buffer, bytes - 1)
  }

  def computeGPU(startSeed: Int, seedCount: Int, seedsPerKernel: Int, points: List[Vec2], resultCount: Int): (List[Result], List[Result]) = {
    val openCL = initializeOpenCL().get
    
    val err = MemoryUtil.memAllocInt(1)
    val program = CL10.clCreateProgramWithSource(openCL.context, Files.readString(Path.of("src/main/opencl/kernel.cl")), err)
    val constructed = CL10.clBuildProgram(program, openCL.device, "", CLProgramCallback.create((p, u) => println("Building")), 0)
    println("Kernel compilation returned " + getProgramString( program, openCL.device, CL10.CL_PROGRAM_BUILD_LOG))
    
    val kernel = CL10.clCreateKernel(program, "brute", err)
    checkCLError(err)

    val permutations = 
      Seq(0,1,2,3,4,5)
      .permutations.toList
      .sortBy(_.last)
      .map(_.take(5))

    val permutationBuffer = MemoryUtil.memAlloc(permutations.length * permutations(0).length)
    permutations.foreach(p => p.foreach(i => permutationBuffer.put(i.asInstanceOf[Byte])))
    permutationBuffer.flip()

    val permuteMemory = CL10.clCreateBuffer(openCL.context, CL10.CL_MEM_WRITE_ONLY | CL10.CL_MEM_COPY_HOST_PTR, permutationBuffer, err)
    checkCLError(err)

    val tasksNeeded = Math.max(seedCount / seedsPerKernel, 1)
    val tasks = (Range(startSeed, startSeed + seedCount, seedCount / tasksNeeded) :+ (startSeed + seedCount)).sliding(2).toList
    
    println("Computing in " + tasks.length + " task(s)")
    
    val results = tasks.map(t => computeGPUIndividual(t.head/(seedCount/tasksNeeded), t.head, t.last - t.head, points, permutations, permuteMemory, kernel, openCL, resultCount))
   
    (results.flatMap(r => r._1).sortBy(m => m.path.cost).take(resultCount), results.flatMap(r => r._2).sortBy(m => -m.path.cost).take(resultCount))
  }

  def computeGPUIndividual(idx: Int, startSeed: Int, seedCount: Int, points: List[Vec2], permutations: List[Seq[Int]], permutationBuffer: Long, kernel: Long, openCL: CLInfo, resultCount: Int): (List[Result], List[Result]) = {
    val err = MemoryUtil.memAllocInt(1)

    var seed = 0;
    for (x <- 1 until startSeed) {
      seed = nextRand(seed)
    }

    val seeds = nextNRand(seed, seedCount); 

    val allPoints = Range(0, seedCount, 1)
      .map(getAll(_, seeds))
      .map(p => p._1 ++ p._2)
      .flatMap(p => p.map(points(_))) 

    val pointsBuffer = MemoryUtil.memAllocFloat(allPoints.length * 2)
    allPoints.foreach(p => pointsBuffer.put(p._1.asInstanceOf[Float]).put(p._2.asInstanceOf[Float]))
    pointsBuffer.flip()
    
    val pointsMemory = CL10.clCreateBuffer(openCL.context, CL10.CL_MEM_WRITE_ONLY | CL10.CL_MEM_COPY_HOST_PTR, pointsBuffer, err)
    checkCLError(err)

    val resultMemory = CL10.clCreateBuffer(openCL.context, CL10.CL_MEM_WRITE_ONLY, seedCount * 4 * 6 * 2, err)
    checkCLError(err)

    val idMemory = CL10.clCreateBuffer(openCL.context, CL10.CL_MEM_WRITE_ONLY, seedCount * 6 * 2 * 2, err)
    checkCLError(err)
    
    CL10.clSetKernelArg1p(kernel, 0, permutationBuffer)
    CL10.clSetKernelArg1p(kernel, 1, pointsMemory)
    CL10.clSetKernelArg1p(kernel, 2, resultMemory)
    CL10.clSetKernelArg1p(kernel, 3, idMemory)
    CL10.clSetKernelArg1i(kernel, 4, seedCount)

    val workSize = MemoryUtil.memAllocPointer(3)
    workSize.put(0, seedCount * 120)
    workSize.put(1, 6)
    workSize.put(2, 2)

    val localWorkSize = MemoryUtil.memAllocPointer(3)
    localWorkSize.put(0, 120)
    localWorkSize.put(1, 1)
    localWorkSize.put(2, 1)
    
    checkCLError(CL10.clEnqueueNDRangeKernel(openCL.queue, kernel, 3, null, workSize, localWorkSize, null, null))
    
    val resultBuffer = MemoryUtil.memAllocFloat(seedCount * 6 * 2)
    val idBuffer = MemoryUtil.memAllocShort(seedCount * 6 * 2)
    
    checkCLError(CL10.clEnqueueReadBuffer(openCL.queue, resultMemory, true, 0, resultBuffer, null, null))
    checkCLError(CL10.clEnqueueReadBuffer(openCL.queue, idMemory, true, 0, idBuffer, null, null))
   
    val values = Range(0, seedCount, 1)
      .map(seed => 
        (for (firstPath <- 0 until 6; secondPath <- 0 until 6) yield (firstPath, secondPath, 
          resultBuffer.get(seed * 12 + firstPath) + 
          resultBuffer.get(seed * 12 + secondPath + 6) + 
          distance(allPoints(seed * 12 + firstPath), allPoints(seed * 12 + secondPath + 6)))).minBy(_._3))
        .zipWithIndex
    
    val sorted = values.sortBy(_._1._3)  
    
    val mins = for (x <- 0 until Math.min(resultCount, values.length)) yield {

      val min = sorted(x)
    
      val minFirstPoints  = Vec2(5.15, -5.0) +: permutations(idBuffer.get(min._2 * 12 + min._1._1) / 5).map(idx => allPoints(min._2 * 12 + idx)) :+ allPoints(min._2 * 12 + min._1._1)
      val minSecondPoints = List(allPoints(min._2 * 12 + min._1._1), allPoints(min._2 * 12 + min._1._2 + 6)) ++ permutations(idBuffer.get(min._2 * 12 + min._1._2 + 6) / 5).reverse.map(idx => allPoints(min._2 * 12 + idx + 6))
      
      Result((startSeed + min._2, seeds(min._2)), Solution(minFirstPoints.map(points.indexOf(_)).toArray, minSecondPoints.map(points.indexOf(_)).toArray, min._1._3))
    }

    val maxs = for (x <- (values.length - resultCount until values.length).reverse) yield {
      val max = sorted(x)
    
      val maxFirstPoints  = Vec2(5.15, -5.0) +: permutations(idBuffer.get(max._2 * 12 + max._1._1) / 5).map(idx => allPoints(max._2 * 12 + idx)) :+ allPoints(max._2 * 12 + max._1._1)
      val maxSecondPoints = List(allPoints(max._2 * 12 + max._1._1), allPoints(max._2 * 12 + max._1._2 + 6)) ++ permutations(idBuffer.get(max._2 * 12 + max._1._2 + 6) / 5).reverse.map(idx => allPoints(max._2 * 12 + idx + 6))
    
      Result((startSeed + max._2, seeds(max._2)), Solution(maxFirstPoints.map(points.indexOf(_)).toArray, maxSecondPoints.map(points.indexOf(_)).toArray, max._1._3))
    }

    checkCLError(CL10.clReleaseMemObject(pointsMemory))
    checkCLError(CL10.clReleaseMemObject(resultMemory))
    checkCLError(CL10.clReleaseMemObject(idMemory))

    MemoryUtil.memFree(pointsBuffer)
    MemoryUtil.memFree(resultBuffer)
    MemoryUtil.memFree(idBuffer)
    
    println("Computed batch " + idx + " (" + startSeed + "->" + (startSeed + seedCount) + "), best is " + mins.head._2._3)

    (mins.toList, maxs.toList)
  }


  def computeLocal(threadCount: Int, start: Int, seedCount: Int, costsArray: Array[Double], seeds: Array[Int]): (Result, Result) = {
    implicit val ec: ExecutionContext = new ExecutionContext {
      val threadPool: ExecutorService = Executors.newFixedThreadPool(threadCount)

      def execute(runnable: Runnable): Unit = {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable): Unit = {}
    }

    val workers = (Range(start, start + seedCount, seedCount / threadCount) :+ (start + seedCount))
      .sliding(2)
      .zipWithIndex
      .map(p => Future { getMinMax(p._2, p._1.head, p._1.last, costsArray, seeds) })

    val work = Future.sequence(workers)
    val results = Await.result(work, Duration.Inf).toSeq

    val best = results.minBy(_._1.path.cost)._1
    val worst = results.maxBy(_._2.path.cost)._2

    (best, worst)
  }

  def parseOptions(map: Map[String, String], options: List[String]): Map[String, String] = 
    options match {
      case Nil => map
      case ("--out" | "-o") :: outFile :: rest => parseOptions(map ++ Map("output" -> outFile), rest)
      case ("--preview" | "-p") :: rest => parseOptions(map ++ Map("showOutput" -> "true"), rest)
      case ("--startIndex" | "-i") :: start :: rest => parseOptions(map ++ Map("start" -> start), rest)
      case ("--startSeed" | "-s") :: startSeed :: rest => parseOptions(map ++ Map("startSeed" -> startSeed), rest)
      case ("--kernelSize" | "-k") :: size :: rest => parseOptions(map ++ Map("seedsPerKernel" -> size), rest)
      case ("--outputCount" | "-c") :: count :: rest => parseOptions(map ++ Map("resultCount" -> count), rest)
      case ("--generateAll" | "-g") :: rest => parseOptions(map ++ Map("showAll" -> "true"), rest)
      case seedCount :: rest => parseOptions(map ++ Map("seedCount" -> seedCount), rest) 
    }

  def main(args: Array[String]): Unit = {
    val options = parseOptions(
      Map("showOutput" -> "true",
          "start" -> "0",
          "startSeed" -> "0",
          "seedCount" -> "1000000000",
          "seedsPerKernel" -> "1000000",
          "output" -> "./",
          "resultCount" -> "5",
          "dumpAll" -> "false"),
        args.toList)
    
    val seedCount = options("seedCount").toInt
    val start = options("start").toInt
    val startSeed = options("startSeed").toInt
    val outPath = Path.of(options("output"))
    val seedsPerKernel = options("seedsPerKernel").toInt
    val dumpAll = options("dumpAll").toBoolean
    val resultCount = if dumpAll then seedCount else options("resultCount").toInt
   
    if (seedsPerKernel <= 0) {
      println("Kernel size must be larger than 0")
      return
    }

    if (seedCount <= 0) {
      println("Seed count must be larger than 0")
      return
    }

    if (resultCount > seedCount || resultCount <= 0) {
      println(s"Result count of $resultCount must be between 1 and $seedCount")
      return
    }

    val formatter = DateTimeFormatter
      .ofLocalizedDateTime(FormatStyle.MEDIUM)
      .withLocale(Locale.UK)
      .withZone(ZoneId.systemDefault)

    val startTime = Instant.now()

    println("Starting at " + formatter.format(startTime))
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

    println("Calculating paths")
   
    val gpuStartTime = System.nanoTime()
    val gpuResult = computeGPU(start, seedCount, seedsPerKernel, points, resultCount)
    val gpuEndTime = System.nanoTime()
    println("GPU took " + (gpuEndTime - gpuStartTime) / 100000000 + " seconds")

    val endTime = Instant.now()
    val runtime = java.time.Duration.between(startTime, endTime)
    println("Finished, calculated optimal path in " + (runtime.getNano.doubleValue) / 100000000 + " seconds")
    
    for (i <- 0 until resultCount) {
      val best = gpuResult._1(i)

      val out = 
        "Result #" + i + "\n" + 
        "Points (1): " + best.path.part1.map(p => points(p)).mkString("(", ", ", ")") + "\n" + 
        "Indices (1): " + best.path.part1.mkString(", ") + "\n" + 
        "Points (2): " + best.path.part2.map(p => points(p)).mkString("(", ", ", ")") + "\n" + 
        "Indices (2): " + best.path.part2.mkString(", ") + "\n" + 
        "Cost: " + best.path.cost + 
        "RNG Index: " + best.indexData._1 + " RNG Value: " + best.indexData._2

      println(out)

      if (Files.isDirectory(outPath)) Files.writeString(outPath.resolve(best.indexData._2 + ".txt"), out)
    }
    

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
  
    val image: Image = new ImageIcon("background.png").getImage
    def paintFrame(g: Graphics, result: Result): Unit = {
      val g2 = g.create.asInstanceOf[Graphics2D]
      g2.setStroke(new BasicStroke(4))

      g2.setColor(Color.BLACK)
      g2.setBackground(Color.BLACK)
      g2.clearRect(0, 0, 6000, 6000)

      g2.drawImage(image, 0, 0, null)

      drawPoints(result.path.part1, result.path.part2, g2, Color.GREEN, Color.CYAN)
      
      g2.setColor(Color.BLACK)
      g2.setBackground(Color.BLACK)
      points.foreach(p => {
        g2.fillRect(1064 - toVisible(p)._1, toVisible(p)._2, 3, 5)
      })

      //drawPoints(worst.path.part1, worst.path.part2, g2, Color.RED, Color.YELLOW)
      g2.dispose()
    }

    def createContentPane =
      new JComponent {
        override def paintComponent(g: Graphics): Unit = {
          super.paintComponent(g)
          paintFrame(g, gpuResult._1(0)) 
        }
      }
   

    for (result <- gpuResult._1) {
      val outImage = new BufferedImage(1064, 1064, BufferedImage.TYPE_INT_ARGB);
      val graphics = outImage.createGraphics()
      paintFrame(graphics, result)
      javax.imageio.ImageIO.write(outImage, "PNG", outPath.resolve(result.indexData._2 + ".png").toFile);
    }

    
    if (options("showOutput").toBoolean) {
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
}
