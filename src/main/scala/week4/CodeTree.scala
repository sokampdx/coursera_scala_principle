package week4

abstract class CodeTree {
  case class Fork(left: CodeTree, right: CodeTree, char: List[Char], weight: Int) extends CodeTree
  case class Left(char: Char, weight: Int) extends CodeTree
}
