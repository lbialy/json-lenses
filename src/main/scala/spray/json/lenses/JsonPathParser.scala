/*
 *    Copyright 2012-2017 Johannes Rudolph
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package spray.json
package lenses

import java.lang.StringBuilder

import org.parboiled.Context
import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

/** A parser for json-path expression as specified here: [[http://goessner.net/articles/JsonPath/]]
  */
object JsonPathParser extends Parser with BasicRules {
  def JsonPathExpr = rule { Path ~ EOI }

  def Path: Rule1[JsonPath.Path] = rule { Root ~ OptionalSelection }

  def Root: Rule1[JsonPath.Root.type] = rule {
    // we don't distinguish between '$' and '@'
    anyOf("$@") ~ push(JsonPath.Root)
  }

  def OptionalSelection: ReductionRule1[JsonPath.Path, JsonPath.Path] = rule {
    Projection ~~> JsonPath.Selection ~ OptionalSelection |
      EMPTY ~~> identity
  }

  def Projection: Rule1[JsonPath.Projection] = rule {
    "." ~ DotProjection |
      "[" ~ BracketProjection ~ "]"
  }

  def DotProjection: Rule1[JsonPath.Projection] = rule {
    ByFieldName
  }
  def AllElements = rule { "*" ~ push(JsonPath.AllElements) }
  def ByFieldName = rule { FieldName ~~> JsonPath.ByField }

  def BracketProjection: Rule1[JsonPath.Projection] = rule {
    Digits ~> (d => JsonPath.ByIndex(d.toInt)) |
      SingleQuotedString ~~> JsonPath.ByField |
      AllElements |
      "?(" ~ WhiteSpace ~ Predicate ~ WhiteSpace ~ ")" ~~> JsonPath.ByPredicate
  }

  def Predicate: Rule1[JsonPath.Predicate] = rule {
    Lt | Gt | Eq | Exists
  }
  def Eq: Rule1[JsonPath.Eq] = rule { op("==")(JsonPath.Eq) }
  def Lt: Rule1[JsonPath.Lt] = rule { op("<")(JsonPath.Lt) }
  def Gt: Rule1[JsonPath.Gt] = rule { op(">")(JsonPath.Gt) }
  def Exists: Rule1[JsonPath.Exists] = rule {
    Path ~~> JsonPath.Exists
  }

  def op[T](op: String)(cons: (JsonPath.Expr, JsonPath.SimpleExpr) => T) =
    Expr ~ WhiteSpace ~ op ~ WhiteSpace ~ SimpleExpr ~~> cons

  def Expr: Rule1[JsonPath.Expr] = rule {
    Path ~~> JsonPath.PathExpr |
      SimpleExpr
  }
  def SimpleExpr: Rule1[JsonPath.SimpleExpr] = rule {
    JsConstant ~~> JsonPath.Constant
  }
  def JsConstant: Rule1[JsValue] = rule {
    JsonNumber |
      SingleQuotedString ~~> (JsString(_))
  }

  val WhiteSpaceChars = " \n\r\t\f"
  def FieldName: Rule1[String] = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | anyOf("_-")) ~> identity
  }

  def SingleQuotedString: Rule1[String] =
    rule { "'" ~ push(new java.lang.StringBuilder) ~ zeroOrMore(!anyOf("'") ~ ("\\" ~ EscapedChar | NormalChar)) } ~ "'" ~~> (_.toString)

  /** The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
    */
  def apply(path: String): JsonPath.Path = apply(path.toCharArray)

  /** The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
    */
  def apply(path: Array[Char]): JsonPath.Path = {
    val parsingResult = ReportingParseRunner(JsonPathExpr).run(path)
    parsingResult.result.getOrElse {
      throw new ParsingException("Invalid JSON source:\n" + ErrorUtils.printParseErrors(parsingResult))
    }
  }
}

// a set of basic rules taken from the old spray-json parser
// see https://github.com/spray/spray-json/blob/v1.2.6/src/main/scala/spray/json/JsonParser.scala
trait BasicRules { _: Parser =>
  def EscapedChar = rule(
    anyOf("\"\\/") ~:% withContext(appendToSb(_)(_))
      | "b" ~ appendToSb('\b')
      | "f" ~ appendToSb('\f')
      | "n" ~ appendToSb('\n')
      | "r" ~ appendToSb('\r')
      | "t" ~ appendToSb('\t')
      | Unicode ~~% withContext((code, ctx) => appendToSb(code.asInstanceOf[Char])(ctx))
  )

  def NormalChar = rule { !anyOf("\"\\") ~ ANY ~:% (withContext(appendToSb(_)(_))) }
  def Unicode = rule { "u" ~ group(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  def JsonNumber = rule { group(Integer ~ optional(Frac) ~ optional(Exp)) ~> (JsNumber(_)) ~ WhiteSpace }
  def Frac = rule { "." ~ Digits }
  def Exp = rule { ignoreCase("e") ~ optional(anyOf("+-")) ~ Digits }

  def Integer = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) }
  def Digits = rule { oneOrMore(Digit) }
  def Digit = rule { "0" - "9" }
  def HexDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def appendToSb(c: Char): Context[Any] => Unit = { ctx =>
    ctx.getValueStack.peek.asInstanceOf[StringBuilder].append(c)
    ()
  }
}

import fastparse._
import NoWhitespace._

/** A parser for json-path expression as specified here: [[http://goessner.net/articles/JsonPath/]]
  */
object JsonPathParserFastParse {

  def apply(path: String): JsonPath.Path = {
    fastparse.parse(path, JsonPathExpr(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure        => throw new ParsingException("Invalid JSON path:\n" + f.trace().longMsg)
    }
  }

  def JsonPathExpr[$: P]: P[JsonPath.Path] = P(Path ~ End)

  def Path[$: P]: P[JsonPath.Path] = P(Root.flatMap(root => OptionalSelections(root)))

  def OptionalSelections[$: P](path: JsonPath.Path): P[JsonPath.Path] = P(
    (Projection).rep.map { projections =>
      projections.foldLeft(path)((p, proj) => JsonPath.Selection(p, proj))
    }
  )

  def Root[$: P]: P[JsonPath.Root.type] = P(CharIn("$@").map(_ => JsonPath.Root))

  def Projection[$: P]: P[JsonPath.Projection] = P(
    "." ~ DotProjection |
      "[" ~ BracketProjection ~ "]"
  )

  def DotProjection[$: P]: P[JsonPath.Projection] = P(ByFieldName)

  def ByFieldName[$: P]: P[JsonPath.Projection] = P(FieldName.map(JsonPath.ByField))

  def AllElements[$: P]: P[JsonPath.Projection] = P("*").map(_ => JsonPath.AllElements)

  def BracketProjection[$: P]: P[JsonPath.Projection] = P(
    Digits.!.map(d => JsonPath.ByIndex(d.toInt)) |
      SingleQuotedString.map(JsonPath.ByField) |
      AllElements |
      ("?(" ~ WhiteSpace ~ Predicate ~ WhiteSpace ~ ")").map(JsonPath.ByPredicate)
  )

  def Predicate[$: P]: P[JsonPath.Predicate] = P(Lt | Gt | Eq | Exists)

  def op[$: P, T](opStr: String)(cons: (JsonPath.Expr, JsonPath.SimpleExpr) => T): P[T] = P(
    Expr ~ WhiteSpace ~ opStr ~ WhiteSpace ~ SimpleExpr
  ).map { case (expr, simpleExpr) => cons(expr, simpleExpr) }

  def Eq[$: P]: P[JsonPath.Eq] = op("==")(JsonPath.Eq)

  def Lt[$: P]: P[JsonPath.Lt] = op("<")(JsonPath.Lt)

  def Gt[$: P]: P[JsonPath.Gt] = op(">")(JsonPath.Gt)

  def Exists[$: P]: P[JsonPath.Exists] = P(Path.map(JsonPath.Exists))

  def Expr[$: P]: P[JsonPath.Expr] = P(
    Path.map(JsonPath.PathExpr) |
      SimpleExpr
  )

  def SimpleExpr[$: P]: P[JsonPath.SimpleExpr] = P(JsConstant.map(JsonPath.Constant))

  def JsConstant[$: P]: P[JsValue] = P(
    JsonNumber |
      SingleQuotedString.map(JsString.apply)
  )

  def FieldName[$: P]: P[String] = P(CharsWhileIn("a-zA-Z0-9_\\-").!)

  def SingleQuotedString[$: P]: P[String] = P(
    "'" ~ (("\\" ~ EscapedChar) | NormalChar).repX.! ~ "'"
  )

  def EscapedChar[$: P]: P[Char] = P(
    "\\" ~ (CharIn("\"\\/").!.map(_.charAt(0))
      | "b".!.map(_ => '\b')
      | "f".!.map(_ => '\f')
      | "n".!.map(_ => '\n')
      | "r".!.map(_ => '\r')
      | "t".!.map(_ => '\t')
      | Unicode)
  )

  def Unicode[$: P]: P[Char] = P("u" ~ HexDigit.rep(exactly = 4).!).map { digits =>
    java.lang.Integer.parseInt(digits, 16).toChar
  }

  def HexDigit[$: P]: P[Unit] = P(CharIn("0-9a-fA-F"))

  def NormalChar[$: P]: P[String] = P(CharPred(c => c != '\\' && c != '\'')).!

  def JsonNumber[$: P]: P[JsValue] = P(
    (Integer ~ Frac.? ~ Exp.?).!.map(s => JsNumber(s))
  )

  def Integer[$: P]: P[Unit] = P("-".? ~ (CharIn("1-9") ~ Digits | Digit))

  def Digit[$: P]: P[Unit] = P(CharIn("0-9"))

  def Digits[$: P]: P[Unit] = P(Digit.rep(1))

  def Frac[$: P]: P[Unit] = P("." ~ Digits)

  def Exp[$: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ Digits)

  def WhiteSpace[$: P]: P[Unit] = P(CharsWhileIn(" \n\r\t\f", 0))
}
