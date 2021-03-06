# HMap

implementation of a immutable heterogeneous map, backed by [shapeless](http://github.com/milessabin/shapeless).

## Usage

A Heterogeneous map contains different types of values for different keys. Each key type is stored in a separate `scala.collection.immutable.Map`. A HMap must be supplied with a relation mapping that maps the types of the keys to their corresponding value types. This can be done by defining a trait with corresponding instances per key-value type: 

```scala

sealed trait SomeRelation[K, V]

object SomeRelation extends Relation{
  implicit case object Int2Bool extends Relation[Int, Boolean]
  implicit case object String2Int extends Relation[String, Int]
  
  val emptyInstance = HMap.empty[Relation](Int2Bool, String2Int)
  override type Relation[K,V] = emptyInstance.type#Relation
  override type MapType = emptyInstance.type#MapType
  override val empty = emptyInstance
}
```

To create a HMap, you have to create an empty one first. `HMap.empty` can be used to instantiate it with the corresponding relation.
since HMap is immutable, you can reuse this empty instance for all HMaps that would use the relation above. The `Relation` class is optional, if you specify the inner `MapType`, relation type `Relation` and the empty instance for this relation, you get a few factory methods for free defined in `RelationInstanceBuilder`. A separate variable `emptyInstance` is used to prevent cyclic references between types. 

With the empty instance we can add mappings:

```scala
val m1 = empty + (42 -> true)
val m2 = empty + ("John" -> 35)
val m3 = m1 ++ m2

val answer: Option[Boolean] = m3.get(42)
val age: Option[Int] = m3.get("John")
```

you can extract the maps that are backed by the HMap by selecting one with a relation:

```scala
val s2i : Map[String, Int] = m3.select(String2Int)
```