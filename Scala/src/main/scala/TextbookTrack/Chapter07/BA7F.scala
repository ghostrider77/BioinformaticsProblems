package TextbookTrack.Chapter07

object BA7F {
  import scala.annotation.tailrec

  type Node = Int
  type Children = (Node, Node)

  private val Nucleotides: List[Char] = List('A', 'C', 'G', 'T')

  class Tree(adjacencyList: Map[Node, Children], val nrLeaves: Int) {
    val nrNodes: Int = adjacencyList.keysIterator.max + 1

    private val parents: Map[Node, Node] =
      adjacencyList.flatMap{ case (parent, (leftChild, rightChild)) => Map(leftChild -> parent, rightChild -> parent) }

    val root: Node = adjacencyList.keysIterator.find(!parents.contains(_)) match {
      case Some(node) => node
      case None => throw new Exception("Root of the tree is not found.")
    }

    def getParent(node: Node): Option[Node] = parents.get(node)

    def getChildren(node: Node): Option[Children] = adjacencyList.get(node)
  }

  private def readData(reader: Iterator[String]): (Map[Node, Children], List[String]) = {
    val leafIdGenerator: Iterator[Int] = Iterator.from(0)
    val lines: Iterator[(String, String)] = reader.map(_.split("->").toList).collect{ case List(a, b) => (a, b) }

    @tailrec
    def loop(edges: List[(Node, Node)], characters: List[String]): (Map[Node, Children], List[String]) =
      lines.nextOption() match {
        case None =>
          val adjacencyList: Map[Node, Children] =
            edges
              .groupMap{ case (nodeFrom, _) => nodeFrom}{ case (_, nodeTo) => nodeTo }
              .collect{ case (node, List(right, left)) => node -> (left, right) }
          (adjacencyList, characters.reverse)
        case Some((nodeId, b)) => b.toIntOption match {
          case None => loop((nodeId.toInt, leafIdGenerator.next()) :: edges, b :: characters)
          case Some(neighbourId) => loop((nodeId.toInt, neighbourId) :: edges, characters)
        }
    }

    loop(Nil, Nil)
  }

  private def calcHammingDistance(s1: String, s2: String): Int = s1.lazyZip(s2).count{ case (c1, c2) => c1 != c2 }

  private def initializeMinimumParsimonyScoreForLeaves(leafLabels: List[Char]): Map[Node, List[Double]] =
    leafLabels
      .zipWithIndex
      .map{
        case (label, leafId) =>
          leafId -> Nucleotides.map(nucleotide => if (nucleotide == label) 0.0 else Double.PositiveInfinity)
      }
      .toMap

  private def isRipe(tree: Tree, node: Node, minimumScores: Map[Node, List[Double]]): Boolean =
    tree.getChildren(node) match {
      case Some((left, right)) if minimumScores.contains(left) && minimumScores.contains(right) => true
      case _ => false
    }

  private def getRipeNodes(tree: Tree, leafScores: Map[Node, List[Double]]): List[Node] =
    (0 until tree.nrNodes).filter(isRipe(tree, _, leafScores)).toList

  private def calcParsimonyScoresForNode(tree: Tree,
                                         minimumParsimonyScores: Map[Node, List[Double]],
                                         node: Node): List[Double] = {
    def calcChildTreeScore(nodeLabel: Char, childScores: List[Double]): Double =
      Nucleotides.lazyZip(childScores).map{ case (char, score) => if (char == nodeLabel) score else score + 1 }.min

    tree.getChildren(node) match {
      case Some((left, right)) =>
        val leftChildScores: List[Double] = minimumParsimonyScores(left)
        val rightChildScores: List[Double] = minimumParsimonyScores(right)
        Nucleotides.map{ nucleotide =>
          calcChildTreeScore(nucleotide, leftChildScores) + calcChildTreeScore(nucleotide, rightChildScores)
        }
      case None => throw new Exception("Ripe node always have children.")
    }
  }

  private def solveSmallParsimonyForSingleLabel(tree: Tree, leafLabels: List[Char]): Map[Node, List[Double]] = {
    @tailrec
    def loop(minimumScores: Map[Node, List[Double]], ripeNodes: List[Node]): Map[Node, List[Double]] =
      ripeNodes match {
        case Nil => minimumScores
        case node :: rest =>
          val nodeScores: List[Double] = calcParsimonyScoresForNode(tree, minimumScores, node)
          val updatedMinimumScores: Map[Node, List[Double]] = minimumScores.updated(node, nodeScores)
          tree.getParent(node) match {
            case Some(parent) if isRipe(tree, parent, updatedMinimumScores) =>
              loop(updatedMinimumScores, parent :: rest)
            case _ => loop(updatedMinimumScores, rest)
          }
      }

    val leafScores: Map[Node, List[Double]] = initializeMinimumParsimonyScoreForLeaves(leafLabels)
    val initialRipeNodes: List[Node] = getRipeNodes(tree, leafScores)
    loop(leafScores, initialRipeNodes)
  }

  private def getMostParsimoniousLabeling(tree: Tree, minimumScores: Map[Int, List[Double]]): List[Char] = {
    def getLabel(scores: List[Double]): Char = scores.lazyZip(Nucleotides).minBy{ case (score, _) => score }._2

    val nodeLabels: Array[Char] = new Array[Char](tree.nrNodes)
    nodeLabels(tree.root) = getLabel(minimumScores(tree.root))

    @tailrec
    def loop(stack: List[Node]): List[Char] = stack match {
      case Nil => nodeLabels.toList
      case nodeId :: rest =>
        tree.getParent(nodeId) match {
          case None => loop(rest)
          case Some(parent) =>
            val parentLabel: Char = nodeLabels(parent)
            val nodeScores: List[Double] = minimumScores(nodeId)
            val modifiedScores: List[Double] =
              Nucleotides.lazyZip(nodeScores).map{ case (char, score) => if (char == parentLabel) score else score + 1 }
            nodeLabels(nodeId) = getLabel(modifiedScores)
            tree.getChildren(nodeId) match {
              case None => loop(rest)
              case Some((left, right)) => loop(left :: right :: rest)
            }
        }
    }

    val initialStack: List[Node] = tree.getChildren(tree.root) match {
      case None => Nil
      case Some((left, right)) => List(left, right)
    }
    loop(initialStack)
  }

  def solveSmallParsimonyProblem(tree: Tree, characters: List[String]): (Int, Vector[String]) = {
    val root: Node = tree.root
    val (totalScore, nodeLabels): (Double, List[List[Char]]) =
      characters.transpose.foldRight((0.0, Nil: List[List[Char]])){
        case (leafLabels, (score, nodeLabels)) =>
          val minimumScores: Map[Node, List[Double]] = solveSmallParsimonyForSingleLabel(tree, leafLabels)
          val labels: List[Char] = getMostParsimoniousLabeling(tree, minimumScores)
          (score + minimumScores(root).min, labels :: nodeLabels)
    }
    (totalScore.toInt, nodeLabels.transpose.map(_.mkString).toVector)
  }

  private def createEdgeOutput(tree: Tree, nodeLabels: Vector[String]): List[String] = {
    (0 until tree.nrNodes).flatMap{ nodeId =>
      val nodeLabel: String = nodeLabels(nodeId)
      tree.getChildren(nodeId).map{ case (left, right) => List(left, right) } match {
        case None => Nil
        case Some(children) =>
          children.flatMap{ child =>
            val childLabel: String = nodeLabels(child)
            val distance: Int = calcHammingDistance(nodeLabel, childLabel)
            List(s"$nodeLabel->$childLabel:$distance", s"$childLabel->$nodeLabel:$distance")
          }
      }
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrLeaves: Int = reader.next().toInt
    val (adjacencyList, characters): (Map[Node, Children], List[String]) = readData(reader)
    val tree = new Tree(adjacencyList, nrLeaves)
    val (parsimonyScore, nodeLabels): (Int, Vector[String]) = solveSmallParsimonyProblem(tree, characters)
    val edges: List[String] = createEdgeOutput(tree, nodeLabels)
    println(parsimonyScore)
    edges.foreach(println)
  }
}
