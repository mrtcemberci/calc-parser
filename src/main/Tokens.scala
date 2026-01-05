package calc

sealed trait Token
case class  Num(value: Int) extends Token
case object Plus   extends Token
case object Minus  extends Token
case object Times  extends Token
case object Div    extends Token
case object LParen extends Token
case object RParen extends Token