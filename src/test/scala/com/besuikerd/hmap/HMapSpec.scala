package com.besuikerd.hmap

import com.besuikerd.hmap.util.BuilderGenerator
import org.scalatest.FlatSpec
import shapeless.{::, HNil}

sealed trait SomeRelation[K, V]


object SomeRelation extends Relation{
  implicit case object RelInt extends SomeRelation[Int, Int]
  implicit case object RelString extends SomeRelation[String, Int]
  implicit case object RelBool extends SomeRelation[Boolean, String]

  val emptyInstance = HMap.empty[SomeRelation](RelInt, RelString, RelBool)
  override type MapType = emptyInstance.type#MapType
  override type Relation[K,V] = emptyInstance.type#Relation[K,V]
  override val empty = emptyInstance
}

class HMapSpec extends FlatSpec{
  "HMap" should "accept tuples of different defined key-value types" in {
    val m1 = SomeRelation.empty + (1 -> 2)
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
    val m0 = SomeRelation.empty
    assertTypeError("m0 + (1, false)")
    assertTypeError("m0 + (42f, true)")
  }

  it should "concatenate two HMap with the same relation type" in {
    val m1 = SomeRelation.empty + (1 -> 2) + (true -> "True")
    val m2 = SomeRelation.empty + ("John" -> 42) + ("Jane" -> 42)
    val m3 = m1 ++ m2

    assertResult(m1.size + m2.size)(m3.size)
    assertResult(4)(m3.size)
    assertResult(Some(42))(m3.get("John"))
    assertResult(Some("True"))(m3.get(true))
    assertResult(None)(m3.get(false))
  }

  it should "have a constructor to create a singleton hmap" in {
    val m1 = SomeRelation(true -> "True")
    assertResult(1)(m1.size)
    assertResult(Some("True"))(m1.get(true))

    val m2 = SomeRelation(true -> "True") + (1 -> 42)
    assertResult(2)(m2.size)
    assertResult(Some("True"))(m2.get(true))
  }

  it should "have a constructor to create a hmap from an existing map" in {
    val existingMap = Map(42 -> 44, 1 -> 42)
    val m1 = SomeRelation(existingMap)
    assertResult(2)(m1.size)
    assertResult(Some(44))(m1.get(42))
    assertResult(Some(42))(m1.get(1))

    val existingMap2 = Map(true -> "True", false -> "False")
    val m2 = SomeRelation(existingMap) ++ existingMap2
    assertResult(4)(m2.size)
    assertResult(Some(44))(m2.get(42))
    assertResult(Some("False"))(m2.get(false))
  }
}
