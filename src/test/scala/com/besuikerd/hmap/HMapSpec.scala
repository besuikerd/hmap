package com.besuikerd.hmap

import org.scalatest.FlatSpec

sealed trait Relation[K, V]

object Relation{
  implicit case object RelInt extends Relation[Int, Int]
  implicit case object RelString extends Relation[String, Int]
  implicit case object RelBool extends Relation[Boolean, String]

  val empty = HMap.empty[Relation](RelInt, RelString, RelBool)
}

class HMapSpec extends FlatSpec{
  "HMap" should "accept tuples of different defined key-value types" in {
    val m1 = Relation.empty + (1 -> 2)
    assertResult(1)(m1.size)
    assertResult(Some(2))(m1.get(1))
    assertResult(None)(m1.get(2))

    val m2 = m1 + ("John" -> 42)
    assertResult(2)(m2.size)
    assertResult(Some(42))(m2.get("John"))
    assertResult(None)(m2.get("Jane"))
    assertResult(Some(2))(m2.get(1))

    val m3 = m2 + (true -> "True")
    assertResult(3)(m3.size)
    assertResult(Some("True"))(m3.get(true))
    assertResult(None)(m3.get(false))
    assertResult(Some(42))(m3.get("John"))
    assertResult(Some(2))(m3.get(1))
  }

  it should "not accept key-value types that are not defined" in {
    val m0 = Relation.empty
    assertTypeError("m0 + (1, false)")
    assertTypeError("m0 + (42f, true)")
  }

  it should "concatenate two HMap with the same relation type" in {
    val m1 = Relation.empty + (1 -> 2) + (true -> "True")
    val m2 = Relation.empty + ("John" -> 42) + ("Jane" -> 42)
    val m3 = m1 ++ m2

    assertResult(m1.size + m2.size)(m3.size)
    assertResult(4)(m3.size)
    assertResult(Some(42))(m3.get("John"))
    assertResult(Some("True"))(m3.get(true))
    assertResult(None)(m3.get(false))
  }
}
