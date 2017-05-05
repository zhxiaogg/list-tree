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

    node match {
      case Some(TreeNode(data, Nil)) => siblings match {
        case Nil => _asciiDisplay(None, Nil, uncles, result :+ s"$prefix+-$data")
        case _ => _asciiDisplay(siblings.headOption, siblings.tail, uncles, result :+ s"$prefix+-$data")
      }

      case Some(TreeNode(data, children)) => _asciiDisplay(children.headOption, children.tail, siblings +: uncles, result :+ s"$prefix+-$data")

      case None => validUncles match {
        case Nil => result
        case head :: tail => _asciiDisplay(head.headOption, head.tail, tail, result :+ emptyLine)
      }
    }
  }

  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    _asciiDisplay(Some(root), Nil, Nil, Nil)
  }
}