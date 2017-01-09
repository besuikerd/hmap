package com.besuikerd.hmap

import shapeless.PolyDefns.~>
import shapeless._

class RelationInstanceBuilder[Rel[_, _], MapType <: HList](empty: HMap[Rel, MapType]) extends AnyRef{
  def apply[K0, V0, T0](kv0: (K0,V0))(implicit r0: Rel[K0,V0], m0: shapeless.ops.hlist.Modifier.Aux[MapType, Map[K0,V0], Map[K0,V0], T0], l0: shapeless.ops.tuple.Last.Aux[T0, MapType]): HMap[Rel, MapType] = empty + kv0

  def apply[K0, V0, T0](kv0: Map[K0,V0])(implicit r0: Rel[K0,V0], m0: shapeless.ops.hlist.Modifier.Aux[MapType, Map[K0,V0], Map[K0,V0], T0], l0: shapeless.ops.tuple.Last.Aux[T0, MapType]): HMap[Rel, MapType] = empty ++ kv0
}