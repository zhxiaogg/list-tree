package me.zhxiaog.list.tree

import org.scalatest.{Matchers, WordSpec}

class TreeNodeTest extends WordSpec with Matchers {

  "tree node" should {
    "display single line for single node" in {
      TreeNode.asciiDisplay(TreeNode("Root")) should ===(Seq("+-Root"))
    }

    "display children nodes with a indented tab" in {
      val tree = TreeNode("Root", List(TreeNode("level1-1"), TreeNode("level1-2"), TreeNode("level1-3")))
      TreeNode.asciiDisplay(tree) should ===(Seq("+-Root", "\t+-level1-1", "\t+-level1-2", "\t+-level1-3"))
    }

    "disaply children nodes recursively" in {
      val child1 = TreeNode("level1-1", TreeNode("level2-1", TreeNode("level3-1") :: Nil) :: Nil)
      val tree = TreeNode("Root", List(child1, TreeNode("level1-2"), TreeNode("level1-3")))
      TreeNode.asciiDisplay(tree) should ===(
        Seq(
          "+-Root",
          "\t+-level1-1",
          "\t|\t+-level2-1",
          "\t|\t\t+-level3-1",
          "\t|",
          "\t+-level1-2",
          "\t+-level1-3"))
    }

    "test" in {
      val child1 = TreeNode("level1-1", TreeNode("level2-1", TreeNode("level3-1") :: Nil) :: Nil)
      val child2 = TreeNode("level1-2", TreeNode("level2-2-1", TreeNode("level3-2-1") :: Nil) :: TreeNode("level2-2-2") :: Nil)
      val tree = TreeNode("Root", List(child1, child2, TreeNode("level1-3", TreeNode("level2-3-1") :: Nil)))
      TreeNode.asciiDisplay(tree).foreach(println)
    }
  }
}
