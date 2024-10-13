package collections

import collections.BinaryTree.delete
import org.scalatest.funsuite.AnyFunSuite

class BinaryTreeSpec extends AnyFunSuite {

  test("add elements to binary tree") {
    val tree = BinaryTree.add(5, Empty) // Добавляем корневой элемент 5
    val treeWith3 = BinaryTree.add(3, tree) // Добавляем элемент 3
    val treeWith7 = BinaryTree.add(7, treeWith3) // Добавляем элемент 7 в дерево, уже содержащее 3

    // Теперь дерево должно содержать элементы 3 и 7
    assert(treeWith7 == Node(5, Some(Node(3)), Some(Node(7))))
  }

  test("delete element from binary tree") {
    val tree = Node(5, None, Some(Node(7, None, None)))
    val result = delete(Some(tree), 5) // Assume you have this logic in place
    assert(result.isEmpty) // Adjust based on expected behavior
  }


  test("delete root element with two children") {
    val tree = Node(5, Some(Node(3)), Some(Node(7))) // Исходное дерево
    val deletedTree = BinaryTree.delete(Some(tree), 5)     // Удаляем корень
    assert(deletedTree == Node(7, Some(Node(3)), None))  // 7 становится новым корнем, 3 остается левым дочерним элементом
  }


  test("foldLeft works correctly for sum") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.foldLeft(tree, 0)(_ + _) == 15)
  }

  test("breadthFirstSearch works correctly") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.breadthFirstSearch(tree) == List(5, 3, 7))
  }

  test("depthFirstSearch works correctly") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.depthFirstSearch(tree) == List(3, 5, 7))  // Прямой обход
  }

  test("max value from breadthFirstSearch") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.max(BinaryTree.breadthFirstSearch, tree).contains(7))
  }

  test("min value from breadthFirstSearch") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.min(BinaryTree.breadthFirstSearch, tree).contains(3))
  }

  test("max value from depthFirstSearch") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.max(BinaryTree.depthFirstSearch, tree).contains(7))
  }

  test("min value from depthFirstSearch") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.min(BinaryTree.depthFirstSearch, tree).contains(3))
  }

  test("size of tree") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    assert(BinaryTree.size(tree) == 3)
  }

  test("size of empty tree") {
    assert(BinaryTree.size(Empty) == 0)
  }

  test("print tree levels") {
    val tree = Node(5, Some(Node(3)), Some(Node(7)))
    BinaryTree.print(tree)
    // Ожидаемый вывод:
    // 5
    // 3 7
  }

  test("add element to left child") {
    val tree = Node(5, Some(Node(3)))
    val newTree = BinaryTree.add(1, tree)
    assert(newTree == Node(5, Some(Node(3, Some(Node(1)))), None))
  }

  test("add element to right child") {
    val tree = Node(5, None, Some(Node(7)))
    val newTree = BinaryTree.add(8, tree)
    assert(newTree == Node(5, None, Some(Node(7, None, Some(Node(8))))))
  }
}
