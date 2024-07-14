import scala.util.chaining.scalaUtilChainingOps
import Ast.LetInExpr
import Ast.Node
import Ast.LiteralExpr
import scala.math.Ordering
import scala.math.Ordered.orderingToOrdered
import scala.annotation.varargs
import Ast.BinaryOp
import Ast.UnaryOp
import Ast.FnDef
import Ast.FnArgs
import Ast.FnCall
import Evaluator.EvaluatedIdentifier
import scala.io.Source
import Ast.IfExpr

type SpannedIx = Either[Int, (Int, Int)]

trait Spanned {
  def position: SpannedIx
}

sealed trait Token
// Binary Ops
case class Assign(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"="
}
case class Add(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"+"
}
case class Mul(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"*"
}
case class Div(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"/"

}
case class Sub(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"-"
}

case class Concat(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"<>"
}
case class GT(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f">"
}
case class GTE(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f">="
}
case class LT(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"<"
}

case class LTE(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"<="
}
case class Eq(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"=="
}
case class NotEq(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"!="
}
case class And(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"&&"
}
case class Or(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"||"
}
case class Pipe(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"|>"
}
case class Let(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"let"
}
case class In(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"in"
}

case class If(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"if"
}
case class Then(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"then"
}
case class Else(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"else"
}

case class LBrace(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"{"
}
case class RBrace(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"}"
}
case class LParen(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"("
}
case class RParen(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f")"
}

case class Comma(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f","
}

case class EndLine(position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f";"
}

case class Integer(value: Int, position: SpannedIx) extends Token with Spanned {
  override def toString(): String = f"$value"
}
case class StringVal(value: String, position: SpannedIx)
    extends Token
    with Spanned {
  override def toString(): String = f"\"$value\""
}
case class Identifier(value: String, position: SpannedIx)
    extends Token
    with Spanned {
  override def toString(): String = f"$value"
}

type Tokenized = Either[String, (List[Char], List[Token])]

object Tokens {
  def isDoubleCharOperator(h: Char, t: Char): Boolean = {
    (h, t) match {
      case (
            ('!', '=') | ('=', '=') | ('|', '|') | ('&', '&') | ('|', '>') |
            ('<', '>') | ('>', '=') | ('<', '=')
          ) =>
        true
      case _ => false
    }
  }
  def isSingleCharOperator(c: Char): Boolean = {
    c match {
      case '+' | '-' | '/' | '=' | '*' | '<' | '>' => true
      case _                                       => false
    }
  }

  def getOpToken(h: Char, t: Char, ix: Int): Token = {
    (h, t) match {
      case ('!', '=') => NotEq(position = Right(ix, ix + 1))
      case ('=', '=') => Eq(position = Right(ix, ix + 1))
      case ('|', '|') => Or(position = Right(ix, ix + 1))
      case ('&', '&') => And(position = Right(ix, ix + 1))
      case ('|', '>') => Pipe(position = Right(ix, ix + 1))
      case ('<', '>') => Concat(position = Right(ix, ix + 1))
      case ('<', '=') => LTE(position = Right(ix, ix + 1))
      case ('>', '=') => GTE(position = Right(ix, ix + 1))
    }
  }

  def getOpToken(c: Char, ix: Int): Token = {
    c match {
      case '+' => Add(position = Left(ix))
      case '-' => Sub(position = Left(ix))
      case '*' => Mul(position = Left(ix))
      case '/' => Div(position = Left(ix))
      case '=' => Assign(position = Left(ix))
      case '<' => LT(position = Left(ix))
      case '>' => GT(position = Left(ix))
    }
  }

  def tokenize(
      chars: List[Char],
      tokens: List[Token] = List(),
      ix: Int = 0
  ): Tokenized = {
    chars match {
      case Nil => Right(Nil, tokens.reverse)
      case opH :: opT :: remaining if isDoubleCharOperator(opH, opT) =>
        tokenize(remaining, getOpToken(opH, opT, ix) :: tokens, ix + 2)
      case op :: remaining if isSingleCharOperator(op) =>
        tokenize(remaining, getOpToken(op, ix) :: tokens, ix + 1)
      case content @ (c :: _) if c.isLetter || c == '_' =>
        tokenizeIdentifier(content, tokens, ix)
      case content @ (c :: _) if c.isDigit => tokenizeNum(content, tokens, ix)
      case ';' :: remaining =>
        tokenize(remaining, EndLine(position = Left(ix)) :: tokens, ix + 1)
      case ',' :: remaining =>
        tokenize(remaining, Comma(position = Left(ix)) :: tokens, ix + 1)
      case '{' :: remaining =>
        tokenize(remaining, LBrace(position = Left(ix)) :: tokens, ix + 1)
      case '}' :: remaining =>
        tokenize(remaining, RBrace(position = Left(ix)) :: tokens, ix + 1)
      case '(' :: remaining =>
        tokenize(remaining, LParen(position = Left(ix)) :: tokens, ix + 1)
      case ')' :: remaining =>
        tokenize(remaining, RParen(position = Left(ix)) :: tokens, ix + 1)
      case '"' :: remaining => tokenizeString(remaining, tokens, ix + 1)
      case (' ' | '\n' | '\t') :: remaining =>
        tokenize(remaining, tokens, ix + 1)
      case x :: _ => Left(f"Unknown token $x")
    }
  }

  def isIdentifierChar(c: Char): Boolean = {
    return c.isDigit || c.isLetter || c == '_'
  }

  def tokenizeIdentifier(
      chars: List[Char],
      tokens: List[Token],
      ix: Int
  ): Tokenized = {
    val identifier = chars.takeWhile(c => isIdentifierChar(c)).mkString
    val endIx = ix + identifier.length() - 1

    val tok = getKeyword(identifier, ix, endIx) match {
      case Some(keyword) => keyword
      case None => {
        val position = if ix == endIx then Left(ix) else Right(ix, endIx)
        Identifier(value = identifier, position = position)
      }
    }
    val remaining = chars.slice(identifier.length(), chars.length)
    return tokenize(remaining, tok :: tokens, endIx + 1)
  }

  def getKeyword(identifier: String, ix: Int, endIx: Int): Option[Token] = {
    identifier match {
      case "let"  => Some(Let(position = Right(ix, endIx)))
      case "in"   => Some(In(position = Right(ix, endIx)))
      case "if"   => Some(If(position = Right(ix, endIx)))
      case "then" => Some(Then(position = Right(ix, endIx)))
      case "else" => Some(Else(position = Right(ix, endIx)))
      case _      => None
    }

  }

  def tokenizeString(
      chars: List[Char],
      tokens: List[Token],
      ix: Int
  ): Tokenized = {
    val stringVal = chars.takeWhile(c => c != '"').mkString
    val endIx = ix + stringVal.length
    val tok = StringVal(
      value = stringVal,
      position = Right(ix, endIx)
    )
    val remaining =
      chars.slice(from = stringVal.length() + 1, until = chars.length)
    return tokenize(remaining, tok :: tokens, endIx + 1)
  }

  def tokenizeNum(
      chars: List[Char],
      tokens: List[Token],
      ix: Int
  ): Tokenized = {
    val digits = chars.takeWhile(c => c.isDigit)
    val intValue = digits.mkString.toIntOption
    val endIx = ix + digits.length - 1
    intValue match {
      case Some(value) => {
        val tok =
          Integer(value = value, position = Right(ix, endIx))
        val remaining =
          chars.slice(from = digits.length, until = chars.length)
        return tokenize(remaining, tok :: tokens, endIx + 1)
      }
      case None => Left(f"failed to parse integer $digits")
    }
  }
}

type Parsed = Either[String, (Node, List[Token])]

enum DType:
  case Int, String, Bool, Fn

enum Precedence:
  case Lowest, In, Addition, Multiplication, Paren, ThenElse, Comp

given Ordering[Precedence] with
  def compare(x: Precedence, y: Precedence): Int = x.ordinal compare y.ordinal

given Ordering[DType] with
  def compare(x: DType, y: DType): Int = x.ordinal compare y.ordinal

object Ast {
  sealed trait Node
  case class LetInExpr(name: Node, value: Node, scope: Node) extends Node {
    // override def toString(): String = f"let $name = $value in\n$scope"
  }
  case class FnCall(name: Identifier, args: FnArgs) extends Node
  case class FnDef(name: Identifier, args: FnArgs) extends Node
  case class FnArgs(args: List[Node]) extends Node

  case class IfExpr(cond: Node, thenExpr: Node, elseExpr: Node) extends Node {
    // override def toString(): String =
    //   f"if ( $cond )\nthen ( $thenExpr )\nelse ( $elseExpr )"
  }

  case class BinaryOp(op: Token, l: Node, r: Node) extends Node {
    // override def toString(): String = f"( $l $op $r )"
  }
  case class UnaryOp(op: Token, r: Token) extends Node
  case class LiteralExpr(value: Token) extends Node {
    // override def toString(): String = f"$value"
  }
}

object Parser {
  def parse(
      tokens: List[Token],
      precedence: Precedence = Precedence.Lowest
  ): Parsed = {
    tokens match {
      case Nil => Left("None")

      case Let(_) :: (ident @ (Identifier(_, _))) :: Assign(_) :: t =>
        parseLet(toIdentNode(ident), t)

      case Let(_) :: (ident @ (Identifier(_, _))) :: LParen(_) :: t =>
        parseFnDef(ident, t)
          .flatMap((fnVal, remaining) => parseLet(fnVal, remaining))

      case (fn @ (Identifier(_, _))) :: LParen(_) :: t => parseFnCall(fn, t)
      case If(_) :: t                                  => parseIf(t, precedence)
      case LParen(_) :: t                              => parse(t)
      case (intval @ Integer(_, _)) :: t   => parseIdent(intval, t, precedence)
      case (strval @ StringVal(_, _)) :: t => parseIdent(strval, t, precedence)
      case (ident @ Identifier(_, _)) :: t => parseIdent(ident, t, precedence)
      case e :: _ => Left(f"Parsing Error -> Started at $e")
    }
  }

  def parseIf(tokens: List[Token], precedence: Precedence): Parsed = {
    for {
      (condition, remaining) <- parse(tokens, Precedence.Lowest)
      (thenExpr, remaining) <- remaining match {
        case Then(_) :: t => parse(t, Precedence.ThenElse)
        case x =>
          Left(
            f"expected 'then' expression after 'if condition': $condition - found $x"
          )
      }
      (elseExpr, remaining) <-
        remaining match {
          case Else(_) :: t => parse(t, Precedence.Lowest)
          case x =>
            Left(
              f"expected 'else' expression after 'then' expression: $thenExpr- found $x"
            )
        }
    } yield (
      IfExpr(cond = condition, thenExpr = thenExpr, elseExpr = elseExpr),
      remaining
    )
  }

  def parseFnCall(fnIdent: Identifier, tokens: List[Token]): Parsed = {
    for {
      (args, remaining) <- parseArgs(tokens)
    } yield (FnCall(name = fnIdent, args = args), remaining)
  }

  def toIdentNode(ident: Identifier): LiteralExpr = {
    LiteralExpr(value = ident)
  }

  def parseArgs(
      tokens: List[Token],
      acc: List[Node] = List()
  ): Either[String, (FnArgs, List[Token])] = {
    // Lambda

    tokens match {
      case RParen(_) :: t =>
        val args = FnArgs(args = acc.reverse)
        Right((args, t))
      case Comma(_) :: t =>
        parse(t)
          .flatMap((parsedArg, remaining) =>
            parseArgs(remaining, parsedArg :: acc)
          )
      case total @ (h :: t) =>
        parse(total)
          .flatMap((parsedArg, remaining) =>
            parseArgs(remaining, parsedArg :: acc)
          )
      case Nil => Left("Unexpectedly reached end of program")
    }
  }

  def parseFnDef(identifier: Identifier, tokens: List[Token]): Parsed = {
    for {
      (args, remaining) <- parseArgs(tokens)
      result <- remaining match {
        case Assign(_) :: t =>
          val fnDef = FnDef(name = identifier, args = args)
          Right(fnDef, t)
        case _ => Left("Expected fn declaration to be followed by definition")
      }
    } yield (result)
  }

  def parseWith(
      l: Node,
      tokens: List[Token],
      precedence: Precedence
  ): Parsed = {
    tokens match {
      case Nil => Right(l, Nil)
      case h :: t if isBinaryOp(h) =>
        parseBinaryOp(l, h, t, precedence)
      case _ => Right(l, tokens)
    }
  }
  def parseIdent(
      identifier: Token,
      tokens: List[Token],
      precedence: Precedence
  ): Parsed = {
    val literal = LiteralExpr(value = identifier)
    tokens match {
      case h :: t if isBinaryOp(h) =>
        parseBinaryOp(literal, h, t, precedence) match {
          case l @ Left(_)             => l
          case Right(value, remaining) =>
            // Handle the cases where we have a new binary to parse
            remaining match {
              case h :: t if isBinaryOp(h) =>
                parseWith(value, remaining, precedence)
              case RParen(_) :: t =>
                parseWith(value, t, precedence)
              case _ =>
                Right(value, remaining)
            }
        }
      case remaining @ (_ :: t) =>
        val expr = LiteralExpr(value = identifier)
        Right(expr, remaining)
      case Nil =>
        val expr = LiteralExpr(value = identifier)
        Right(expr, List())
    }
  }

  def opToPrecendence(op: Token): Precedence = {
    op match {
      case Add(_) | Sub(_) => Precedence.Addition
      case Div(_) | Mul(_) => Precedence.Multiplication
      case RParen(_)       => Precedence.Paren
      case GT(_) | LT(_) | GTE(_) | LTE(_) | Eq(_) | NotEq(_) => Precedence.Comp
      case _ => Precedence.Lowest
    }

  }

  def parseBinaryOp(
      l: Node,
      op: Token,
      tokens: List[Token],
      precedence: Precedence
  ): Parsed = {
    val operatorPrecedence = opToPrecendence(op)
    if operatorPrecedence < precedence then return Right(l, op :: tokens)

    parse(tokens, operatorPrecedence) match {
      case Right(r, remaining) =>
        Right((BinaryOp(op = op, l = l, r = r), remaining))
      case l @ Left(_) => l
    }
  }

  def parseLet(
      ident: Node,
      tokens: List[Token]
  ): Parsed = {
    return parse(tokens) match {
      case Right((expr, remaining)) =>
        remaining match {
          case In(_) :: t =>
            parse(t).map((r, remaining) =>
              (LetInExpr(name = ident, value = expr, scope = r), remaining)
            )
          case x :: _ =>
            Left(f"Let expression must be followed by scope, found $x")
          case Nil => Left("Unexpectedly Reached End")
        }
      case left @ Left(value) => left
    }
  }

  def isBinaryOp(token: Token): Boolean = {
    token match {
      case Add(_) | Mul(_) | Div(_) | Sub(_) | Eq(_) | NotEq(_) | And(_) | Or(
            _
          ) | Pipe(_) | GT(_) | LT(_) | LTE(_) | GTE(_) =>
        true
      case _ => false
    }
  }
}

object Evaluator {

  sealed trait Evaluated
  case class EvaluatedInt(value: Int) extends Evaluated {
    override def toString(): String = f"$value"
  }
  case class EvaluatedBool(value: Boolean) extends Evaluated {
    override def toString(): String = f"$value"
  }
  case class EvaluatedString(value: String) extends Evaluated {
    override def toString(): String = f"$value"
  }
  case class EvaluatedIdentifier(value: String) extends Evaluated

  case class Lazy(value: Node, args: FnArgs) extends Evaluated

  type Context = Map[String, Evaluated]

  def evalNode(
      node: Node,
      context: Context = Map.empty[String, Evaluated]
  ): Either[String, Evaluated] = {
    node match {
      case LiteralExpr(value) => evalLiteral(value, context)
      case BinaryOp(op, l, r) => evalBinary(op, l, r, context)
      case LetInExpr(LiteralExpr(Identifier(name, _)), value, scope) =>
        for {
          assignedVal <- evalNode(value, context)
          result <- evalNode(scope, addToContext(name, assignedVal, context))
        } yield result
      case LetInExpr(FnDef(ident @ Identifier(name, _), args), value, scope) =>
        for {
          fnVal <- evalFnDef(ident, args, value, context)
          result <- evalNode(scope, addToContext(name, fnVal, context))
        } yield result
      case FnCall(Identifier(name, _), args) =>
        for {
          evalArgs <- evalFnArgs(args, context)
          result <- evalFnCall(name, evalArgs, context)
        } yield result
      case IfExpr(cond, thenExpr, elseExpr) =>
        for {
          condition <- evalNode(cond, context)
          result <- condition match {
            case EvaluatedBool(true)  => evalNode(thenExpr, context)
            case EvaluatedBool(false) => evalNode(elseExpr, context)
            case x =>
              val msg =
                f"Invalid If Expression: Expected Boolean condition, found: $x"
              Left(msg)
          }
        } yield result
      case x => Left(f"Error when evaluating $x")
    }
  }

  def evalFnCall(
      name: String,
      args: List[Evaluated],
      context: Context
  ): Either[String, Evaluated] = {
    context.get(name) match {
      case None => Left(f"Function $name not found")
      case Some(Lazy(value, fnArgs)) =>
        for {
          fnContext <- makeFnContext(context, args, fnArgs)
          result <- evalNode(value, fnContext)
        } yield result
      case Some(x) => Left(f"Invalid function found: $x")
    }
  }

  def makeFnContext(
      context: Context,
      callArgs: List[Evaluated],
      defArgs: FnArgs
  ): Either[String, Context] = {
    for {
      matchedArgs <- checkFnArgs(callArgs, defArgs.args)
    } yield matchedArgs.foldRight[Context](context)((toAdd, context) =>
      val (k, v) = toAdd
      addToContext(k, v, context)
    )
  }

  def checkFnArgs(
      callArgs: List[Evaluated],
      defArgs: List[Node],
      acc: List[(String, Evaluated)] = List()
  ): Either[String, List[(String, Evaluated)]] = {
    (callArgs, defArgs) match {
      case (Nil, _ :: _) | (_ :: _, Nil) => Left("Wrong Number of args")
      case (
            evaluated :: restCallArgs,
            LiteralExpr(Identifier(name, _)) :: restDefArgs
          ) =>
        checkFnArgs(restCallArgs, restDefArgs, (name, evaluated) :: acc)
      case (Nil, Nil) => Right(acc.reverse)
      case (a, b)     => Left(f"Error evaluaing function args $a, $b")
    }

  }

  def evalFnArgs(
      args: FnArgs,
      context: Context
  ): Either[String, List[Evaluated]] = {
    args.args
      .map(a => evalNode(a, context))
      .foldRight[Either[String, List[Evaluated]]](Right(Nil))((either, acc) =>
        for {
          list <- acc
          value <- either
        } yield value :: list
      )
  }

  def intoLazy(node: Node, args: FnArgs): Lazy = {
    // returns an 'evaluated value' that can be further evaluated
    Lazy(value = node, args = args)
  }

  def evalFnDef(
      name: Identifier,
      args: FnArgs,
      body: Node,
      context: Context
  ): Either[String, Evaluated] = {
    Right(intoLazy(body, args))
  }

  def addToContext(key: String, value: Evaluated, context: Context): Context = {
    context + (key -> value)
  }

  def evalLiteral(value: Token, context: Context): Either[String, Evaluated] = {
    value match {
      case Integer(value, _)    => Right(EvaluatedInt(value = value))
      case StringVal(value, _)  => Right(EvaluatedString(value = value))
      case Identifier(value, _) => Right(EvaluatedIdentifier(value = value))
      case _                    => Left("Invalid Literal")
    }

  }

  def getConcreteValue(
      node: Evaluated,
      context: Context
  ): Either[String, Evaluated] = {
    node match {
      case EvaluatedIdentifier(name) =>
        context.get(name) match {
          case Some(v) => Right(v)
          case None    => Left(f"Identifier $name not defined")
        }
      case x => Right(x)
    }
  }

  def evalBinary(
      operator: Token,
      l: Node,
      r: Node,
      context: Context
  ): Either[String, Evaluated] = {
    for {
      lEvaluated <- evalNode(l, context)
      rEvaluated <- evalNode(r, context)
      lConcrete <- getConcreteValue(lEvaluated, context)
      rConcrete <- getConcreteValue(rEvaluated, context)
      result <- (lConcrete, rConcrete) match {
        case (EvaluatedString(lval), EvaluatedString(rval)) =>
          evalStringBinary(lval, rval, operator)

        case (EvaluatedInt(lval), EvaluatedInt(rval)) =>
          evalIntegerBinary(lval, rval, operator)
        case _ => Left("Invalid Evaluated")
      }
    } yield result
  }

  def evalStringBinary(
      l: String,
      r: String,
      op: Token
  ): Either[String, EvaluatedString] = {
    val value = op match {
      case Concat(_) => Some(l + r)
      case x         => None
    }
    value match {
      case Some(v) => Right(EvaluatedString(value = v))
      case None    => Left(f"Invalid Operator $op for strings $l and $r")
    }

  }
  def evalIntegerBinary(
      l: Int,
      r: Int,
      op: Token
  ): Either[String, Evaluated] = {
    val value = op match {
      case Add(_) => Some(l + r)
      case Sub(_) => Some(l - r)
      case Mul(_) => Some(l * r)
      case Div(_) => Some(l / r)
      case LT(_) => Some(l < r)
      case LTE(_) => Some(l <= r)
      case GT(_) => Some(l > r)
      case GTE(_) => Some(l >= r)
      case x      => None
    }
    value match {
      case Some(v: Int) => Right(EvaluatedInt(value = v))
      case Some(v: Boolean) => Right(EvaluatedBool(value = v))
      case None    => Left(f"Invalid Operator $op for integers $l and $r")
    }
  }
}

object MiniFP {
  def main(args: Array[String]) = {
    val filename = "test.mfp"
    val input = Source.fromFile(filename).getLines().mkString("\n")
    val result = Tokens
      .tokenize(input.toList)
      .flatMap((_, c) => {
        Parser.parse(c)
      })

    val evaluated = result match {
      case l @ Left(value) => l
      case Right(node, _)  => Evaluator.evalNode(node)
    }

    evaluated match {
      case Left(err)        => println(f"Error: $err")
      case Right(evaluated) => println(f"Evaluated to: $evaluated")
    }
  }
}
