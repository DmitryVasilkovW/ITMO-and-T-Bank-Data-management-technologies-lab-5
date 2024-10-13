package collections

import scala.annotation.tailrec

sealed trait Tree
case class Node(value: Int, left: Option[Tree] = None, right: Option[Tree] = None) extends Tree
case object Empty extends Tree

object BinaryTree {
  def add(value: Int, tree: Tree): Tree = tree match {
    case Empty => Node(value)
    case Node(v, Some(left: Node), right) =>
      if (value < v) Node(v, Some(add(value, left)), right)
      else Node(v, Some(left), Some(add(value, right.getOrElse(Empty))))
    case Node(v, None, right) =>
      if (value < v) Node(v, Some(Node(value)), right)
      else Node(v, None, Some(add(value, right.getOrElse(Empty))))
  }


  def delete(value: Int, tree: Tree): Tree = {
    @tailrec
    def minValue(node: Node): Int = node.left match {
      case Some(l: Node) => minValue(l)
      case _ => node.value
    }

    tree match {
      case Empty => Empty
      case Node(v, left, right) if value < v => Node(v, Some(delete(value, left.getOrElse(Empty))), right)
      case Node(v, left, right) if value > v => Node(v, left, Some(delete(value, right.getOrElse(Empty))))
      case Node(_, left, right) =>
        (left, right) match {
          case (None, None) => Empty
          case (Some(l), None) => l
          case (None, Some(r)) => r
          case (Some(l), Some(r)) => Node(minValue(r), left, Some(delete(minValue(r), r)))
        }
    }
  }

  def foldLeft[A](tree: Tree, acc: A)(f: (A, Int) => A): A = tree match {
    case Empty => acc
    case Node(value, left, right) =>
      val leftAcc = foldLeft(left.getOrElse(Empty), acc)(f)
      val currentAcc = f(leftAcc, value)
      foldLeft(right.getOrElse(Empty), currentAcc)(f)
  }

  def breadthFirstSearch(tree: Tree): List[Int] = {
    def bfs(queue: List[Tree], acc: List[Int]): List[Int] = queue match {
      case Nil => acc
      case Empty :: rest => bfs(rest, acc)
      case Node(value, left, right) :: rest =>
        bfs(rest ++ List(left.getOrElse(Empty), right.getOrElse(Empty)), acc :+ value)
    }
    bfs(List(tree), List.empty)
  }

  def depthFirstSearch(tree: Tree): List[Int] = foldLeft(tree, List.empty[Int])((acc, value) => acc :+ value)

  def max(search: Tree => List[Int], tree: Tree): Option[Int] =
    search(tree).reduceOption((x, y) => if (x > y) x else y)

  def min(search: Tree => List[Int], tree: Tree): Option[Int] =
    search(tree).reduceOption((x, y) => if (x < y) x else y)

  def size(tree: Tree): Int = foldLeft(tree, 0)((acc, _) => acc + 1)

  def print(tree: Tree): Unit = {
    def printLevel(nodes: List[Tree], level: Int): Unit = nodes match {
      case Nil => ()
      case _ =>
        val nextLevel = nodes.flatMap {
          case Empty => List(Empty, Empty)
          case Node(_, left, right) => List(left.getOrElse(Empty), right.getOrElse(Empty))
        }
        println(nodes.collect { case Node(value, _, _) => value }.mkString(" "))
        printLevel(nextLevel, level + 1)
    }
    printLevel(List(tree), 0)
  }
}
