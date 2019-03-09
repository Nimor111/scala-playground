package jsonparser

sealed trait JExpr

final case class JNumber(value: Double) extends JExpr
final case class JString(value: String) extends JExpr
final case object JNil extends JExpr
final case class JBoolean(value: Boolean) extends JExpr
final case class JArray(value: List[JExpr]) extends JExpr
final case class JObject(value: Map[String, JExpr]) extends JExpr
