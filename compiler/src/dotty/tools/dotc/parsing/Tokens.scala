package dotty.tools
package dotc
package parsing

import collection.immutable.BitSet
import core.Decorators._
import core.StdNames.nme

abstract class TokensCommon {
  def maxToken: Int

  type Token = Int
  type TokenSet = BitSet

  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi: _*)

  def showTokenDetailed(token: Int): String = debugString(token)

  def showToken(token: Int): String = {
    val str = tokenString(token)
    if (isKeyword(token)) s"'$str'" else str
  }

  val tokenString, debugString: Array[String] = new Array[String](maxToken + 1)

  def enter(token: Int, str: String, debugStr: String = ""): Unit = {
    assert(tokenString(token) == null)
    tokenString(token) = str
    debugString(token) = if (debugStr.isEmpty) str else debugStr
  }

  /** special tokens */
  final val EMPTY = 0;             enter(EMPTY, "<empty>") // a missing token, used in lookahead
  final val ERROR = 1;             enter(ERROR, "erroneous token") // an erroneous token
  final val EOF = 2;               enter(EOF, "eof")

  /** literals */
  final val CHARLIT = 3;           enter(CHARLIT, "character literal")
  final val INTLIT = 4;            enter(INTLIT, "integer literal")
  final val DECILIT = 5;           enter(DECILIT, "number literal")  // with decimal point
  final val EXPOLIT = 6;           enter(EXPOLIT, "number literal with exponent")
  final val LONGLIT = 7;           enter(LONGLIT, "long literal")
  final val FLOATLIT = 8;          enter(FLOATLIT, "float literal")
  final val DOUBLELIT = 9;         enter(DOUBLELIT, "double literal")
  final val STRINGLIT = 10;         enter(STRINGLIT, "string literal")
  final val STRINGPART = 11;       enter(STRINGPART, "string literal", "string literal part")
  //final val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  //final val QUOTEID = 13;        enter(QUOTEID, "quoted identifier") // TODO: deprecate

  /** identifiers */
  final val IDENTIFIER = 14;       enter(IDENTIFIER, "identifier")
  //final val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  /** alphabetic keywords */
  final val IF = 20;               enter(IF, "if")
  final val FOR = 21;              enter(FOR, "for")
  final val ELSE = 22;             enter(ELSE, "else")
  final val THIS = 23;             enter(THIS, "this")
  final val NULL = 24;             enter(NULL, "null")
  final val NEW = 25;              enter(NEW, "new")
  //final val WITH = 26;             enter(WITH, "with")
  final val SUPER = 27;            enter(SUPER, "super")
  //final val CASE = 28;             enter(CASE, "case")
  //final val CASECLASS = 29;        enter(CASECLASS, "case class")
  //final val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  //final val VAL = 31;              enter(VAL, "val")
  final val ABSTRACT = 32;         enter(ABSTRACT, "abstract")
  final val FINAL = 33;            enter(FINAL, "final")
  final val PRIVATE = 34;          enter(PRIVATE, "private")
  final val PROTECTED = 35;        enter(PROTECTED, "protected")
  final val OVERRIDE = 36;         enter(OVERRIDE, "override")
  //final val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  //final val VAR = 38;              enter(VAR, "var")
  //final val DEF = 39;              enter(DEF, "def")
  //final val TYPE = 40;             enter(TYPE, "type")
  final val EXTENDS = 41;          enter(EXTENDS, "extends")
  final val TRUE = 42;             enter(TRUE, "true")
  final val FALSE = 43;            enter(FALSE, "false")
  //final val OBJECT = 44;           enter(OBJECT, "object")
  final val CLASS = 45;            enter(CLASS, "class")
  final val IMPORT = 46;           enter(IMPORT, "import")
  final val PACKAGE = 47;          enter(PACKAGE, "package")
  //final val YIELD = 48;            enter(YIELD, "yield")
  final val DO = 49;               enter(DO, "do")
  //final val TRAIT = 50;            enter(TRAIT, "trait")
  //final val SEALED = 51;           enter(SEALED, "sealed")
  final val THROW = 52;            enter(THROW, "throw")
  final val TRY = 53;              enter(TRY, "try")
  final val CATCH = 54;            enter(CATCH, "catch")
  final val FINALLY = 55;          enter(FINALLY, "finally")
  final val WHILE = 56;            enter(WHILE, "while")
  final val RETURN = 57;           enter(RETURN, "return")
  //final val MATCH = 58;            enter(MATCH, "match")
  //final val LAZY = 59;             enter(LAZY, "lazy")
  //final val THEN = 60;             enter(THEN, "then")
  //final val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  //final val ENUM = 62;             enter(ENUM, "enum")

  /** special symbols */
  final val COMMA = 70;            enter(COMMA, "','")
  final val SEMI = 71;             enter(SEMI, "';'")
  final val DOT = 72;              enter(DOT, "'.'")
  //final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  //final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  //final val USCORE = 73;           enter(USCORE, "_")
  final val COLON = 74;            enter(COLON, ":")
  final val EQUALS = 75;           enter(EQUALS, "=")
  //final val LARROW = 76;           enter(LARROW, "<-")
  //final val ARROW = 77;            enter(ARROW, "=>")
  //final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  //final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  //final val HASH = 82;             enter(HASH, "#")
  final val AT = 83;               enter(AT, "@")
  //final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")

  val keywords: TokenSet

  def isKeyword(token: Token): Boolean = keywords contains token

  /** parentheses */
  final val LPAREN = 91;           enter(LPAREN, "'('")
  final val RPAREN = 92;           enter(RPAREN, "')'")
  final val LBRACKET = 93;         enter(LBRACKET, "'['")
  final val RBRACKET = 94;         enter(RBRACKET, "']'")
  final val LBRACE = 95;           enter(LBRACE, "'{'")
  final val RBRACE = 96;           enter(RBRACE, "'}'")
  final val INDENT = 97;           enter(INDENT, "indent")
  final val OUTDENT = 98;          enter(OUTDENT, "unindent")

  final val firstParen = LPAREN
  final val lastParen = OUTDENT

  def buildKeywordArray(keywords: TokenSet): (Int, Array[Int]) = {
    def start(tok: Token) = tokenString(tok).toTermName.asSimpleName.start
    def sourceKeywords = keywords.toList.filter { (kw: Token) =>
      val ts = tokenString(kw)
      (ts != null) && !ts.contains(' ')
    }

    val lastKeywordStart = sourceKeywords.map(start).max

    val arr = Array.fill(lastKeywordStart + 1)(IDENTIFIER)
    for (kw <- sourceKeywords) arr(start(kw)) = kw
    (lastKeywordStart, arr)
  }
}

object Tokens extends TokensCommon {
  final val minToken = EMPTY
  final def maxToken: Int = XMLSTART

  final val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  final val QUOTEID = 13;          enter(QUOTEID, "quoted identifier") // TODO: deprecate

  final val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  final val identifierTokens: TokenSet = BitSet(IDENTIFIER, BACKQUOTED_IDENT)

  def isIdentifier(token : Int): Boolean =
    token >= IDENTIFIER && token <= BACKQUOTED_IDENT

  /** alphabetic keywords */
  final val WITH = 26;             enter(WITH, "with")
  final val CASE = 28;             enter(CASE, "case")
  final val CASECLASS = 29;        enter(CASECLASS, "case class")
  final val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  final val VAL = 31;              enter(VAL, "val")
  final val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  final val VAR = 38;              enter(VAR, "var")
  final val DEF = 39;              enter(DEF, "def")
  final val TYPE = 40;             enter(TYPE, "type")
  final val OBJECT = 44;           enter(OBJECT, "object")
  final val YIELD = 48;            enter(YIELD, "yield")
  final val TRAIT = 50;            enter(TRAIT, "trait")
  final val SEALED = 51;           enter(SEALED, "sealed")
  final val MATCH = 58;            enter(MATCH, "match")
  final val LAZY = 59;             enter(LAZY, "lazy")
  final val THEN = 60;             enter(THEN, "then")
  final val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  final val ENUM = 62;             enter(ENUM, "enum")
  final val GIVEN = 63;            enter(GIVEN, "given")
  final val EXPORT = 64;           enter(EXPORT, "export")
  final val MACRO = 65;            enter(MACRO, "macro") // TODO: remove
  final val END = 66;              enter(END, "end")

  /** special symbols */
  final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  final val USCORE = 73;           enter(USCORE, "_")
  final val LARROW = 76;           enter(LARROW, "<-")
  final val ARROW = 77;            enter(ARROW, "=>")
  final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  final val HASH = 82;             enter(HASH, "#")
  final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")
  final val TLARROW = 85;          enter(TLARROW, "=>>")
  final val CTXARROW = 86;         enter(CTXARROW, "?=>")

  final val QUOTE = 87;            enter(QUOTE, "'")

  final val COLONEOL = 88;         enter(COLONEOL, ":", ": at eol")
  final val SELFARROW = 89;        enter(SELFARROW, "=>") // reclassified ARROW following self-type

  /** XML mode */
  final val XMLSTART = 99;         enter(XMLSTART, "$XMLSTART$<") // TODO: deprecate

  final val alphaKeywords: TokenSet = tokenRange(IF, END)
  final val symbolicKeywords: TokenSet = tokenRange(USCORE, CTXARROW)
  final val keywords: TokenSet = alphaKeywords | symbolicKeywords

  final val allTokens: TokenSet = tokenRange(minToken, maxToken)

  final val simpleLiteralTokens: TokenSet =
    tokenRange(CHARLIT, STRINGLIT) | BitSet(TRUE, FALSE)
  final val literalTokens: TokenSet = simpleLiteralTokens | BitSet(INTERPOLATIONID, QUOTEID, NULL)  // TODO: drop QUOTEID when symbol literals are gone

  final val atomicExprTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    USCORE, NULL, THIS, SUPER, TRUE, FALSE, RETURN, QUOTEID, XMLSTART)

  final val openParensTokens = BitSet(LBRACE, LPAREN, LBRACKET)

  final val canStartExprTokens3: TokenSet =
      atomicExprTokens
    | openParensTokens
    | BitSet(INDENT, QUOTE, IF, WHILE, FOR, NEW, TRY, THROW)

  final val canStartExprTokens2: TokenSet = canStartExprTokens3 | BitSet(DO)

  final val canStartTypeTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    THIS, SUPER, USCORE, LPAREN, AT)

  final val templateIntroTokens: TokenSet = BitSet(CLASS, TRAIT, OBJECT, ENUM, CASECLASS, CASEOBJECT)

  final val dclIntroTokens: TokenSet = BitSet(DEF, VAL, VAR, TYPE, GIVEN)

  final val defIntroTokens: TokenSet = templateIntroTokens | dclIntroTokens

  final val localModifierTokens: TokenSet = BitSet(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)

  final val accessModifierTokens: TokenSet = BitSet(
    PRIVATE, PROTECTED)

  final val modifierTokens: TokenSet = localModifierTokens | accessModifierTokens | BitSet(
    OVERRIDE)

  final val modifierTokensOrCase: TokenSet = modifierTokens | BitSet(CASE)

  final val modifierFollowers = modifierTokensOrCase | defIntroTokens

  /** Is token only legal as start of statement (eof also included)? */
  final val mustStartStatTokens: TokenSet = defIntroTokens | modifierTokens | BitSet(IMPORT, EXPORT, PACKAGE)

  final val canStartStatTokens2: TokenSet = canStartExprTokens2 | mustStartStatTokens | BitSet(
    AT, CASE, END) // END is included since it might be tested before being converted back to IDENTIFIER
  final val canStartStatTokens3: TokenSet = canStartExprTokens3 | mustStartStatTokens | BitSet(
    AT, CASE, END)

  final val canEndStatTokens: TokenSet = atomicExprTokens | BitSet(TYPE, GIVEN, RPAREN, RBRACE, RBRACKET, OUTDENT)

  /** Tokens that stop a lookahead scan search for a `<-`, `then`, or `do`.
   *  Used for disambiguating between old and new syntax.
   */
  final val stopScanTokens: BitSet = mustStartStatTokens |
    BitSet(IF, ELSE, WHILE, DO, FOR, YIELD, NEW, TRY, CATCH, FINALLY, THROW, RETURN, MATCH, SEMI, EOF)

  final val numericLitTokens: TokenSet = BitSet(INTLIT, DECILIT, EXPOLIT, LONGLIT, FLOATLIT, DOUBLELIT)

  final val statCtdTokens: BitSet = BitSet(THEN, ELSE, DO, CATCH, FINALLY, YIELD, MATCH)

  final val closingRegionTokens = BitSet(RBRACE, RPAREN, RBRACKET, CASE) | statCtdTokens

  final val canStartIndentTokens: BitSet =
    statCtdTokens | BitSet(COLONEOL, WITH, EQUALS, ARROW, CTXARROW, LARROW, WHILE, TRY, FOR, IF, THROW, RETURN)

  /** Faced with the choice between a type and a formal parameter, the following
   *  tokens determine it's a formal parameter.
   */
  final val startParamTokens: BitSet = modifierTokens | BitSet(VAL, VAR, AT)

  final val scala3keywords = BitSet(ENUM, GIVEN)

  final val endMarkerTokens = identifierTokens | BitSet(IF, WHILE, FOR, MATCH, TRY, NEW, THROW, GIVEN, VAL, THIS)

  final val skipStopTokens = BitSet(SEMI, NEWLINE, NEWLINES, RBRACE, RPAREN, RBRACKET, OUTDENT)

  final val softModifierNames = Set(nme.inline, nme.opaque, nme.open, nme.transparent, nme.infix)
}
