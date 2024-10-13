package collections

import scala.annotation.tailrec

sealed trait Tree
case class Node(value: Int, left: Option[Tree] = None, right: Option[Tree] = None) extends Tree
case object Empty extends Tree

object BinaryTree {
  def add(value: Int, tree: Tree): Tree = tree match {
    case Empty => Node(value)
    case Node(v, left, right) =>
      if (value < v) Node(v, Some(add(value, left.getOrElse(Empty))), right)
      else Node(v, left, Some(add(value, right.getOrElse(Empty))))
  }

  def delete(node: Option[Tree], value: Int): Option[Tree] = {
    node match {
      case None => None
      case Some(Node(v, left, right)) =>
        if (value < v) {
          Some(Node(v, delete(left, value), right))
        } else if (value > v) {
          Some(Node(v, left, delete(right, value)))
        } else {
          None
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
    @tailrec
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
    @tailrec
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
