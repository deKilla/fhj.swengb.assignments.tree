package fhj.swengb.assignments.tree.mfuchs

import javafx.scene.paint.Color

import sun.security.util.Length

import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object Graph {

  val colorscheme = "random"
  val colorMap:Map[Int,Color] = colorMap(colorscheme)

  def colorMap(colorscheme:String):Map[Int,Color] = {
    val colorMapNature =
      Map[Int, Color](
        0 -> Color.ROSYBROWN,
        1 -> Color.BROWN,
        2 -> Color.SADDLEBROWN,
        3 -> Color.INDIANRED,
        4 -> Color.DARKGREEN,
        5 -> Color.GREEN,
        6 -> Color.YELLOWGREEN,
        7 -> Color.GREENYELLOW,
        8 -> Color.YELLOW
      )

    val colorMapHell =
      Map[Int, Color](
        0 -> Color.BLACK,
        1 -> Color.MAROON,
        2 -> Color.FIREBRICK,
        3 -> Color.BROWN,
        4 -> Color.CRIMSON,
        5 -> Color.ORANGERED,
        6 -> Color.DARKORANGE,
        7 -> Color.GOLD,
        8 -> Color.NAVAJOWHITE
      )

    def randomColor():Color = Color.rgb(Random.nextInt(255),Random.nextInt(255),Random.nextInt(255))
    val colorMapRandom =
      Map[Int, Color](
        0 -> randomColor,
        1 -> randomColor,
        2 -> randomColor,
        3 -> randomColor,
        4 -> randomColor,
        5 -> randomColor,
        6 -> randomColor,
        7 -> randomColor,
        8 -> randomColor
      )

    colorscheme match {
      case "hell" => colorMapHell
      case "random" => colorMapRandom
      case _ => colorMapNature
    }
  }
  /**
    * creates a random tree
    *
    * @param pt
    * @return
    */
  def randomTree(pt: Pt2D): Tree[L2D] =
    mkGraph(pt, Random.nextInt(360), Random.nextDouble() * 150, 3 + Random.nextInt(6)) //depth minimum 3 and maximum increased to 9


  /**
    * Given a Tree of L2D's and a function which can convert any L2D to a Line,
    * you have to traverse the tree (visit all nodes) and create a sequence
    * of Line's. The ordering of the lines is not important.
    *
    * @param tree  a tree which contains L2D instances
    * @param convert a converter function
    * @return
    */
  def traverse[A, B](tree: Tree[A])(convert: A => B): Seq[B] = {
    tree match {
      case Node(value) => Seq(convert(value))
      case Branch(left, right) => traverse(left)(convert) ++ traverse(right)(convert)
    }
  }

  /**
    * Creates a tree graph.
    *
    * @param start the startpoint (root) of the tree
    * @param initialAngle initial angle of the tree
    * @param length the initial length of the tree
    * @param treeDepth the depth of the tree
    * @param factor the factor which the length is decreasing for every iteration
    * @param angle the angle between a branch and the root
    * @param colorMap color map, by default it is the colormap given in the companion object Graph
    *
    * @return a Tree[L2D] which can be traversed by other algorithms
    */
  def mkGraph(start: Pt2D,
              initialAngle: AngleInDegrees,
              length: Double,
              treeDepth: Int,
              factor: Double = 0.75,
              angle: Double = 45.0,
              colorMap: Map[Int, Color] = Graph.colorMap): Tree[L2D] = {
    assert(treeDepth <= colorMap.size, s"Treedepth higher than color mappings - bailing out ...")


    def createGraph(start:L2D, acc: Int): Tree[L2D] = acc match {

      case root if treeDepth == 0 =>    Node(start)

      case nodes if acc == treeDepth => Branch(
                                          Node(start),
                                          Branch(
                                            Node(start.left(factor,angle,colorMap(acc-1))),
                                            Node(start.right(factor,angle,colorMap(acc-1)))
                                          )
                                        )

      case _ =>                         Branch(
                                          Node(start),
                                          Branch(
                                            createGraph(start.left(factor,angle,colorMap(acc-1)),acc+1),
                                            createGraph(start.right(factor,angle,colorMap(acc-1)),acc+1)
                                          )
                                        )
    }

    val acc = 1
    val p = L2D(start,initialAngle,length,colorMap(acc-1))

    createGraph(p,acc)

 }

}

object MathUtil {

  /**
    * rounds the given value to 3 decimal places.
    *
    * @param value  a double value
    * @return
    */
  def round(value: Double): Double = {
    BigDecimal(value).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  /**
    * turns an angle given in degrees to a value in radiants.
    *
    * @param angle
    * @return
    */
  def toRadiants(angle: AngleInDegrees): AngleInRadiants = {
    (angle*math.Pi)/180
  }
}


object L2D {

  import MathUtil._

  /**
    * Given a startpoint, an angle and a length the endpoint of the line
    * is calculated and finally a L2D class is returned.
    *
    * @param start the startpoint
    * @param angle the angle
    * @param length the length of the line
    * @param color the color
    * @return
    */
  def apply(start: Pt2D, angle: AngleInDegrees, length: Double, color: Color): L2D = {
    val pointX = round(start.x + length * Math.cos(toRadiants(angle)))
    val pointY = round(start.y + length * Math.sin(toRadiants(angle)))
    val end = Pt2D(pointX,pointY)
    val l2d = L2D(start,end,color)
    l2d
  }


}

case class L2D(start: Pt2D, end: Pt2D, color: Color) {

  lazy val xDist = end.x - start.x
  lazy val yDist = end.y - start.y

  lazy val angle = {
    assert(!((xDist == 0) && (yDist == 0)))
    (xDist, yDist) match {
      case (x, 0) if x > 0 => 0
      case (0, y) if y > 0 => 90
      case (0, y) if y < 0 => 270
      case (x, 0) if x < 0 => 180
      case (x, y) if x < 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x < 0 && y > 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x > 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 360
      case (x, y) => Math.atan(y / x) * 180 / Math.PI
    }
  }

  lazy val length: Double = {
    Math.sqrt(xDist * xDist + yDist * yDist)
  }

  def left(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle - deltaAngle, length * factor, c)
  }

  def right(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle + deltaAngle, length * factor, c)
  }


}

