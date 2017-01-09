package com.besuikerd.hmap

import shapeless.HList


trait Relation{
  def apply = new RelationInstanceBuilder[Relation, MapType](empty)

  //dummy apply method to please the typechecker
  def apply(n: Nothing) = ???

  type MapType <: HList
  type Relation[_, _]
  type HMapType = HMap[Relation, MapType]
  def empty: HMapType
}