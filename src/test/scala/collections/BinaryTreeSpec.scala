package collections

import org.scalatest.funsuite.AnyFunSuite

class BinaryTreeSpec extends AnyFunSuite {
  test("add elements to binary tree") {
    val tree = BinaryTree.add(5, Empty)
    assert(BinaryTree.add(3, tree) == Node(5, Some(Node(3))))
  }

  test("foldLeft works correctly") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.foldLeft(tree, 0)(_ + _) == 15)
  }

}