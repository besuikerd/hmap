package com.besuikerd.hmap.util

object BuilderGenerator {
  type IntToString = Int => String
  type TypeParameterFunction = IntToString
  type ParameterFunction = IntToString
  type ImplicitFunction = IntToString
  type ReturnType = IntToString
  type BodyFunction = IntToString

  case class ApplyFunction(
    typeParameterFn: TypeParameterFunction,
    parameterFn: ParameterFunction,
    bodyFn: BodyFunction,
    returnTypeFn: ReturnType,
    implicitFn: Option[ImplicitFunction] = None
  )

  def wrap(prefix: String, fn: IntToString, suffix: String = ""): IntToString = n => prefix + fn(n) + suffix
  def wrap(prefix: String, suffix: String)(fn: IntToString): IntToString = wrap(prefix, fn, suffix)

  def combine(fns: IntToString*): IntToString = combine("")(fns: _*)
  def combine(sep: String)(fns: IntToString*): IntToString = n => fns.foldLeft(""){case (acc, fn) => acc ++ sep ++ fn(n)}

  def const[A, B](a: A): B => A = _ => a

  def repeatUntil(sep: String, fn: Int => String): IntToString = n => (for(m <- 0 to n) yield fn(m)).mkString(sep)

  val keyValueTypeParameters: TypeParameterFunction = repeatUntil(", ", n => s"K$n, V$n")

  val keyValueTupleTypeParameters: TypeParameterFunction = repeatUntil(", ", n => s"K$n, V$n, T$n")

  val updateImplicits: IntToString = repeatUntil(", ", n => s"r$n: Rel[K$n,V$n], m$n: shapeless.ops.hlist.Modifier.Aux[MapType, Map[K$n,V$n], Map[K$n,V$n], T$n], l$n: shapeless.ops.tuple.Last.Aux[T$n, MapType]")

  val hmapUnderlyingHList = wrap("", repeatUntil(" :: ", n => s"Map[K$n, V$n]"), " :: HNil")
  val hmapReturnTypeFn: ReturnType = wrap("HMap[Rel, ", hmapUnderlyingHList, "]")

  val hmapApply = ApplyFunction(
    typeParameterFn = keyValueTypeParameters,
    parameterFn = repeatUntil(", ", n => s"r$n: Rel[K$n, V$n]"),
    bodyFn = combine(const("new "), hmapReturnTypeFn, wrap("(", repeatUntil(" :: ", n => s"Map.empty[K$n, V$n]"), " :: HNil)")),
    returnTypeFn = hmapReturnTypeFn
  )

  val singletonApply = ApplyFunction(
    typeParameterFn = keyValueTupleTypeParameters,
    parameterFn = repeatUntil(", ", n => s"kv$n: (K$n,V$n)"),
    bodyFn = wrap("empty + ", repeatUntil(" + ", n => s"kv$n")),
    returnTypeFn = const("HMap[Rel, MapType]"),
    implicitFn = Some(updateImplicits)
  )

  val mapApply = ApplyFunction(
    typeParameterFn = keyValueTupleTypeParameters,
    parameterFn = repeatUntil(", ", n => s"kv$n: Map[K$n,V$n]"),
    bodyFn = wrap("empty ++ ", repeatUntil(" ++ ", n => s"kv$n")),
    returnTypeFn = const("HMap[Rel, MapType]"),
    implicitFn = Some(updateImplicits)
  )

  def buildBuilder(
    constructor: String,
    applies: Seq[ApplyFunction]
  )(n: Int): String = {
    val reducedApplies = applies.map{
      case ApplyFunction(typeParameterFn, parameterFn, bodyFn, returnTypeFn, implicitFn) =>
        repeatUntil("\n  ", m => s"def apply[${typeParameterFn(m)}](${parameterFn(m)})${implicitFn.map(wrap("(implicit ", ")")(_)(m)).getOrElse("")}: ${returnTypeFn(m)} = ${bodyFn(m)}")(n - 1)
    }.mkString("\n\n  ")

    s"""${constructor} extends AnyRef{
       |  ${reducedApplies}
       |}
     """.stripMargin
  }

  lazy val hmap = buildBuilder("class HMapBuilder[Rel[_, _]]", Seq(hmapApply))(22)
  lazy val instance = buildBuilder("class RelationInstanceBuilder[Rel[_, _], MapType <: HList](empty: HMap[Rel, MapType])", Seq(singletonApply, mapApply))(1)
}
