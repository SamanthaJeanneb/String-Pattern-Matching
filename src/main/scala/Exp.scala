// Defines a trait called Exp, which serves as the base type for different expressions
// Each expression type will extend this trait, allowing all expressions to be treated as Exp
trait Exp

// Defines a case class C that represents a single character expression
// - Takes a Char parameter called value, storing the character that this expression represents
// - Extends the Exp trait, so instances of C can be treated as Exp types in the matcher function
case class C(value: Char) extends Exp

// Defines a case class Concat that represents concatenation of two expressions
// - Has two parameters, left and right, which are both Exp types
// - Represents expressions that appear in sequence, like "AB" in regular expressions
// - Extends Exp, so Concat expressions can be processed by functions that handle Exp types
case class Concat(left: Exp, right: Exp) extends Exp

// Defines a case class Optional that represents an optional expression
// - Takes a single parameter, left, which is an Exp type, representing an expression that may or may not appear
// - For example, "A?" would match either "A" or an empty string
// - Extends Exp to allow Optional expressions to be used wherever Exp types are accepted
case class Optional(left: Exp) extends Exp

// Defines a case class Alternation that represents an alternation (choice) between two expressions
// - Has two parameters, left and right, both of Exp type, representing two alternative options
// - For example, "A|B" would match either "A" or "B"
// - Extends Exp to allow Alternation expressions to be processed as Exp types
case class Alternation(left: Exp, right: Exp) extends Exp

// Companion object for the Exp trait, containing utility functions related to Exp expressions
object Exp {

  // Defines a function matcher that checks if an expression matches a given input string
  // - Takes two parameters:
  //   - expression: An Exp type that represents the expression to match against
  //   - input: A String that represents the input text we want to match with the expression
  // - Returns a Boolean indicating if the expression matches the input
  def matcher(expression: Exp, input: String): Boolean = {

    // Uses pattern matching on the expression to handle each type of Exp case
    expression match {

      // Case for C, which represents a single character
      // - Matches if expression is an instance of C with a specific char value
      case C(char) =>
        // Checks if input is exactly one character long, and if that character matches char or is a dot (.)
        // - input.nonEmpty: Ensures input is not an empty string
        // - input.length == 1: Ensures input is only one character long
        // - (char == input.head || char == '.'): Checks if the character matches char or is a wildcard (.)
        input.nonEmpty && input.length == 1 && (char == input.head || char == '.')

      // Case for Concat, representing concatenation of two expressions
      // - Matches if expression is an instance of Concat with two sub-expressions, firstPart and secondPart
      case Concat(firstPart, secondPart) =>
        var isMatched = false // Initializes isMatched as false to track if a match is found

        // Loops through each possible split point in the input string
        for (splitIndex <- 0 to input.length) {
          // Checks if the input can be split into two parts that match firstPart and secondPart
          // - matcher(firstPart, input.take(splitIndex)): Checks if the left part matches firstPart
          // - matcher(secondPart, input.drop(splitIndex)): Checks if the right part matches secondPart
          if (matcher(firstPart, input.take(splitIndex)) && matcher(secondPart, input.drop(splitIndex))) {
            isMatched = true // Sets isMatched to true if a valid split is found that matches both parts
          }
        }
        isMatched // Returns true if any split matched both expressions, otherwise false

      // Case for Optional, representing an optional expression (can be present or absent)
      // - Matches if expression is an instance of Optional with a sub-expression subExpression
      case Optional(subExpression) =>
        // Checks if subExpression matches the input, or if the input is empty (optional case)
        matcher(subExpression, input) || input.isEmpty

      // Case for Alternation, representing a choice between two expressions
      // - Matches if expression is an instance of Alternation with two choices, choice1 and choice2
      case Alternation(choice1, choice2) =>
        // Matches if either choice1 or choice2 matches the input
        matcher(choice1, input) || matcher(choice2, input)
    }
  }
}
