package com.besuikerd.hmap

import shapeless.PolyDefns.~>
import shapeless._
import syntax._
import shapeless.ops.{hlist, tuple}

object mapAddAll extends Poly3{
  implicit def default[Rel[_, _], R <: HList, K, V, R2](
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = at[Rel[K, V], R, Map[K, V]]((_, r, map) => new HMap[Rel, R](last(modifier(r, _ ++ map))))
}

class HMap[Rel[_, _], R <: HList](val underlying: R){
  def +[K,V, R2](kv: (K,V))(
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = this.update[K,V,R2](_ + kv)

  def -[K, V, R2](k: K)(
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = this.update[K,V,R2](_ - k)

  def ++[K, V, R2](m: Map[K, V])(
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = update[K, V, R2](_ ++ m)

  def ++[K, V, R2 <: HList](m: HMap[Rel, R2])(
    implicit zip: hlist.ZipWith.Aux[R, R2, HMap.zipAppend.type, R]
  ) = new HMap[Rel, R](zip(underlying, m.underlying))

  def --[K, V, R2](keys: Seq[K])(
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = update[K, V, R2](_ -- keys)

  def --[K, V, R2 <: HList](keys: R2)(
    implicit zip: hlist.ZipWith.Aux[R, R2, HMap.zipRemove.type, R]
  ) = new HMap[Rel, R](zip(underlying, keys))

  def --[K, V, R2 <: HList](m: HMap[Rel, R2])(
    implicit zip: hlist.ZipWith.Aux[R, R2, HMap.zipRemove.type, R]
  ) = new HMap[Rel, R](zip(underlying, m.underlying))

  def update[K, V, R2](f: Map[K,V] => Map[K,V])(
    implicit rel: Rel[K, V]
    , modifier: hlist.Modifier.Aux[R, Map[K, V], Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R]
  ) = new HMap[Rel, R](last(modifier(underlying, f)))

  def remove[K, V, R2, R3 <: HList](rel: Rel[K, V])(
    implicit rm: hlist.Remove.Aux[R, Map[K, V], R2]
    , last: tuple.Last.Aux[R2, R3]
  ) = new HMap[Rel, R3](last(rm(underlying)))

  def size(implicit folder: hlist.LeftFolder.Aux[R, Int, HMap.size.type, Int]) = folder(underlying, 0)

  def get[K, V](k: K)(
    implicit rel: Rel[K,V]
    , selector: hlist.Selector[R, Map[K, V]]
  ): Option[V] = select(rel).get(k)

  def select[K, V](rel: Rel[K, V])(
    implicit selector: hlist.Selector[R, Map[K, V]]
  ): Map[K,V] = selector(underlying)

  override def toString: String = s"""HMap($underlying)"""
}




object HMap{
  def empty[Rel[_, _]] = new HMapBuilder[Rel]

  object zipAppend extends Poly2{
    implicit def caseDefault[K, V] = at[Map[K,V], Map[K,V]]{case (m1, m2) => m1 ++ m2}
  }

  object zipRemove extends Poly2{
    implicit def caseKey[K, V] = at[Map[K,V], Seq[K]]{case (m, keys) => m -- keys}
    implicit def caseKeyValue[K, V] = at[Map[K,V], Map[K,V]]{case (m1, m2) => m1 -- m2.keys}
  }

  object size extends Poly2{
    implicit def caseDefault[K,V] = at[Int, Map[K,V]](_ + _.size)
  }
}
