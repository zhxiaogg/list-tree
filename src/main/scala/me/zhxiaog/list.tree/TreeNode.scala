package me.zhxiaog.list.tree

import scala.annotation.tailrec

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object TreeNode {

  @tailrec
  def _asciiDisplay(
    node: Option[TreeNode[String]],
    siblings: Seq[TreeNode[String]],
    uncles: Seq[Seq[TreeNode[String]]],
    result: Seq[String]): Seq[String] = {

    lazy val prefix = uncles.reverse.map {
      case Nil => "\t"
      case _ => "|\t"
    } mkString ("")

    lazy val validUncles = uncles.dropWhile(_.isEmpty)
    lazy val emptyLine = validUncles.reverse.map {
      case Nil => ""
      case _ => "|"
    } mkString "\t"

    (node, siblings, uncles) match {
      case (Some(TreeNode(data, Nil)), Nil, uncles) => _asciiDisplay(None, Nil, uncles, result :+ s"$prefix+-$data")
      case (Some(TreeNode(data, Nil)), siblings, uncles) => _asciiDisplay(siblings.headOption, siblings.tail, uncles, result :+ s"$prefix+-$data")
      case (Some(TreeNode(data, children)), siblings, uncles) => _asciiDisplay(children.headOption, children.tail, siblings +: uncles, result :+ s"$prefix+-$data")
      case (None, Nil, _) if validUncles.size == 0 => result
      case (None, Nil, _) => _asciiDisplay(validUncles.head.headOption, validUncles.head.tail, validUncles.tail, result :+ emptyLine)
    }
  }

  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    _asciiDisplay(Some(root), Nil, Nil, Nil)
  }
}