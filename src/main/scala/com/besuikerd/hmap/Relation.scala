package com.besuikerd.hmap

import shapeless.HList

trait Relation{
  type MapType <: HList
  type Rel[_, _]
  type Type = HMap[Rel, MapType]

  def apply = new RelationInstanceBuilder[Rel, MapType](empty)

  //dummy apply method to please the typechecker
  def apply(n: Nothing) = ???

  val empty: HMap[Rel, MapType]
}