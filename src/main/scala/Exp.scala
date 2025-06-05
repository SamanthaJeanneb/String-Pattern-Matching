trait Exp

case class C(value: Char) extends Exp

case class Concat(left: Exp, right: Exp) extends Exp

case class Optional(left: Exp) extends Exp

case class Alternation(left: Exp, right: Exp) extends Exp

object Exp {
  def matcher(expression: Exp, input: String): Boolean = {
    expression match {
      case C(char) =>
        input.nonEmpty && input.length == 1 && (char == input.head || char == '.')
      case Concat(firstPart, secondPart) =>
        var isMatched = false
        for (splitIndex <- 0 to input.length) {
          if (matcher(firstPart, input.take(splitIndex)) && matcher(secondPart, input.drop(splitIndex))) {
            isMatched = true
          }
        }
        isMatched
      case Optional(subExpression) =>
        matcher(subExpression, input) || input.isEmpty
      case Alternation(choice1, choice2) =>
        matcher(choice1, input) || matcher(choice2, input)
    }
  }
}
