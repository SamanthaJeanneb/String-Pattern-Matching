object RDP {

  def parse(input: String): Exp = {
    val tokens = tokenize(input)
    val pResult = parse_E(tokens)
    val tree = pResult._1
    val rem = pResult._2

    if rem.head == Token.Tok_End then
      tree
    else
      throw new Exception("Illegal parse_E")
  }

  def parse_E(toks: List[Token]): (Exp, List[Token]) = {
    val tRes = parse_T(toks)
    val tTree = tRes._1
    val rest = tRes._2

    val next = lookahead(rest)
    if next == Token.Tok_OR then
      val restOr = matchToken(Token.Tok_OR, rest)
      val eRes = parse_E(restOr)
      (Alternation(tTree, eRes._1), eRes._2)
    else
      (tTree, rest)
  }

  def parse_T(tokens: List[Token]): (Exp, List[Token]) = {
    val fRes = parse_F(tokens)
    val fTree = fRes._1
    val restFact = fRes._2

    val next = lookahead(restFact)
    if next.isInstanceOf[Token.Tok_Char] || next == Token.Tok_LPAREN then
      val tRes = parse_T(restFact)
      (Concat(fTree, tRes._1), tRes._2)
    else
      (fTree, restFact)
  }

  def parse_F(tokens: List[Token]): (Exp, List[Token]) = {
    val aRes = parse_A(tokens)
    val aTree = aRes._1
    val restAtom = aRes._2

    val next = lookahead(restAtom)
    if next == Token.Tok_Q then
      val restQ = matchToken(Token.Tok_Q, restAtom)
      (Optional(aTree), restQ)
    else
      (aTree, restAtom)
  }

  def parse_A(toks: List[Token]): (Exp, List[Token]) = {
    val next = lookahead(toks)
    if next.isInstanceOf[Token.Tok_Char] then
      val charVal = next.asInstanceOf[Token.Tok_Char].value
      (C(charVal), matchToken(Token.Tok_Char(charVal), toks))
    else if next == Token.Tok_LPAREN then
      val rem1 = matchToken(Token.Tok_LPAREN, toks)
      val eRes = parse_E(rem1)
      if lookahead(eRes._2) == Token.Tok_RPAREN then
        (eRes._1, matchToken(Token.Tok_RPAREN, eRes._2))
      else
        throw new Exception("missing parenthesis")
    else
      throw new Exception("Illegal parse_A")
  }

  def lookahead(l: List[Token]): Token = l match
    case List() => throw new Exception("no Tokens")
    case head :: tail => head

  def matchToken(tok: Token, tokList: List[Token]): List[Token] = tokList match
    case head :: tail if tok == head => tail
    case _ => throw new Exception("token is not matched")
}
