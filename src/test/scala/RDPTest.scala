import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.a
import org.scalatest.matchers.should.Matchers.shouldBe

import Token.*



class RDPTest extends AnyFunSuite:
   test("Tokenizer") {
    assert(tokenize("ab") ===List(Tok_Char('a'),Tok_Char('b'),Token.Tok_End))
    assert(tokenize("12") ===List(Tok_Char('1'),Tok_Char('2'),Token.Tok_End))
    assert(tokenize("a.b") === List(Tok_Char('a'),Tok_Char('.'),Tok_Char('b'),Token.Tok_End))
    assert(tokenize("a|b") === List(Tok_Char('a'),Tok_OR,Tok_Char('b'),Token.Tok_End))
    assert(tokenize("ab?") === List(Tok_Char('a'),Tok_Char('b'),Tok_Q,Token.Tok_End))
    assert(tokenize("(ab)?") === List(Tok_LPAREN,Tok_Char('a'),Tok_Char('b'),Tok_RPAREN,Tok_Q, Token.Tok_End))
    assert(tokenize("ab|cd?") === List(Tok_Char('a'),Tok_Char('b'),Tok_OR,Tok_Char('c'),Tok_Char('d'),Tok_Q,Token.Tok_End))

    val exception1: Exception = intercept[Exception](tokenize("+"))
    exception1 shouldBe a[Exception]
  }
    test("parse") {
      assert(RDP.parse("ab") === Concat(C('a'), C('b')))
      assert(RDP.parse("a.b") === Concat(C('a'), Concat(C('.'), C('b'))))
      assert(RDP.parse("a|b") === Alternation(C('a'), C('b')))
      assert(RDP.parse("ab?") === Concat(C('a'), Optional(C('b'))))
      assert(RDP.parse("(ab)?") === Optional(Concat(C('a'), C('b'))))
      assert(RDP.parse("ab|cd?") === Alternation(Concat(C('a'), C('b')), Concat(C('c'), Optional(C('d')))))
  }
    test("match") {
    assert(Exp.matcher(RDP.parse("ab"), "ab") === true)
    assert(Exp.matcher (RDP.parse("a.b"), "ab") === false)
    assert(Exp.matcher (RDP.parse("a|b"), "a") === true)
    assert(Exp.matcher (RDP.parse("a|b"), "b") === true)
    assert(Exp.matcher (RDP.parse("ab?"), "a") === true)
    assert(Exp.matcher (RDP.parse("ab?"), "ab") === true)

    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "hello world") === true)
    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "jello word") === true)
    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "jelly word") === true)
    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "42") === true)
    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "24") === false)
    assert(Exp.matcher (RDP.parse("((h|j)ell. worl?d)|(42)"), "hello world42") === false)

    assert(Exp.matcher (RDP.parse("I (like|love|hate)( (cat|dog))? people"), "I like cat people") === true)
    assert(Exp.matcher (RDP.parse("I (like|love|hate)( (cat|dog))? people"), "I love dog people") === true)
    assert(Exp.matcher (RDP.parse("I (like|love|hate)( (cat|dog))? people"), "I hate people") === true)
    assert(Exp.matcher (RDP.parse("I (like|love|hate)( (cat|dog))? people"), "I people") === false)
    assert(Exp.matcher (RDP.parse("I (like|love|hate)( (cat|dog))? people"), "I likelovehate people") === false)
  }
    test("BonusTest") {
    assert(Exp.matcher(RDP.parse("a(b|bb)?bc"), "abc") === true)
    assert(Exp.matcher(RDP.parse("a(b|bb)?bc"),"abbc") === true)
    assert(Exp.matcher(RDP.parse("a(b|bb)?bc"),"abbbc") === true)
    assert(Exp.matcher(RDP.parse("a(b|bb)?bc"),"ab") === false)
    assert(Exp.matcher(RDP.parse("a(b|bb)?bc"),"abbbbc") === false)
  }




