
enum Token:
  case Tok_Char(value: Char)
  case Tok_OR
  case Tok_Q
  case Tok_LPAREN
  case Tok_RPAREN
  case Tok_End

// Tokenize function
def tokenize(s: String): List[Token] =
  if s.isEmpty then List(Token.Tok_End)
  else
    val head = s.charAt(0)
    val tail = s.substring(1)
    if head.isLetterOrDigit then Token.Tok_Char(head) :: tokenize(tail)
    else if head == '|' then Token.Tok_OR :: tokenize(tail)
    else if head == '?' then Token.Tok_Q :: tokenize(tail)
    else if head == '(' then Token.Tok_LPAREN :: tokenize(tail)
    else if head == ')' then Token.Tok_RPAREN :: tokenize(tail)
    else if head == '.' then Token.Tok_Char('.') :: tokenize(tail)
    else if head == ' ' then Token.Tok_Char(' ') :: tokenize(tail)
    else throw new Exception("Exception thrown from tokenize")

