package mset

import spire.syntax.monoid._
import spire.syntax.eq._
import spire.algebra.Monoid
import spire.algebra.Eq
import spire.algebra.Ring
import spire.math.Natural
import spire.math.Rational
import spire.std.seq._
import spire.std.map._
import spire.std.tuples._

case class MSet[M,A](rep: Map[A,M]) {
  def occurList(implicit E: Eq[M], M: Monoid[M]): List[(A,M)] =
    rep.toList.filter { case (_, m) => !m.isEmpty }

  def apply(a: A)(implicit M: Monoid[M]): M =
    rep get a getOrElse M.empty

  def contains(a: A)(implicit E: Eq[M], M: Monoid[M]): Boolean =
    !apply(a).isEmpty

  def isFrom(m: MSet[M,A])(implicit E: Eq[M], M: Monoid[M]): Boolean =
    rep.foldLeft(true) { case (b, (a,_)) => b && (m contains a) }
}

object MSet {

  type Multiset[A] = MSet[Natural,A]
  type RatBag[A] = MSet[Rational,A]
  type IntBag[A] = MSet[Int,A]

  implicit def msetEq[M:Eq:Monoid,A:Eq]: Eq[MSet[M,A]] = new Eq[MSet[M,A]] {
    def eqv(a: MSet[M,A], b: MSet[M,A]) =
      a.occurList === b.occurList
  }

  def fromList[M,A](xs: List[(A,M)])(implicit M: Monoid[M]): MSet[M,A] =
    MSet(xs.foldLeft(Map.empty:Map[A,M]) { case (s, (a, m)) => s |+| Map(a -> m) })

  def empty[M,A]: MSet[M,A] = MSet(Map.empty)

  def singleton[M,A](a: A)(implicit M: Ring[M]): MSet[M,A] = MSet(Map(a -> M.one))
}
