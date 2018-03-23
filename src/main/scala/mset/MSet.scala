package mset

import spire.syntax.monoid._
import spire.syntax.additiveMonoid._
import spire.syntax.eq._
import spire.algebra.Monoid
import spire.algebra.Eq
import spire.algebra.MultiplicativeMonoid
import spire.algebra.AdditiveSemigroup
import spire.math.Natural
import spire.math.Rational
import spire.std.seq._
import spire.std.map._
import spire.std.tuples._

class MSet[M,A](val rep: Map[A,M]) extends AnyVal {

  import MSet._

  /** Get the occurrence list of this MSet */
  def occurList(implicit E: Eq[M], M: Monoid[M]): List[(A,M)] =
    rep.toList.filter { case (_, m) => !m.isEmpty }

  /** Get the multiplicity of a given object */
  def apply(a: A)(implicit M: Monoid[M]): M =
    rep get a getOrElse M.empty

  /** An object occurs in the MSet if its multiplicity is nonzero */
  def contains(a: A)(implicit E: Eq[M], M: Monoid[M]): Boolean =
    !apply(a).isEmpty

  /** `a` is from `b` when every element of `a` also occurs in `b` */
  def isFrom(m: MSet[M,A])(implicit E: Eq[M], M: Monoid[M]): Boolean =
    rep.foldLeft(true) { case (b, (a,_)) => b && (m contains a) }

  /** Fold an MSet with a monoid */
  def foldMap[B](f: (A,M) => B)(implicit B: Monoid[B]): B =
    rep.foldLeft(B.empty) { case (b, (a, m)) => b |+| f(a, m) }

  def insert(a: A)(implicit M: MultiplicativeMonoid[M], S: AdditiveSemigroup[M]): MSet[M,A] = {
    implicit val sg = S.additive
    new MSet(MapMonoid[A,M].combine(rep, singleton[M,A](a).rep))
  }
}

object MSet {

  type Multiset[A] = MSet[Natural,A]
  type RatBag[A] = MSet[Rational,A]
  type IntBag[A] = MSet[Int,A]

  implicit def msetEq[M:Eq:Monoid,A:Eq]: Eq[MSet[M,A]] = new Eq[MSet[M,A]] {
    def eqv(a: MSet[M,A], b: MSet[M,A]) =
      a.occurList === b.occurList
  }

  implicit def msetMonoid[M:Monoid,A]: Monoid[MSet[M,A]] =
    new Monoid[MSet[M,A]] {
      def combine(a: MSet[M,A], b: MSet[M,A]) =
        new MSet(a.rep |+| b.rep)
      val empty = MSet.empty[M,A]
    }

  /** Turn an occurrence list into an MSet */
  def fromList[M,A](xs: List[(A,M)])(implicit M: Monoid[M]): MSet[M,A] =
    new MSet(xs.foldLeft(Map.empty:Map[A,M]) { case (s, (a, m)) => s |+| Map(a -> m) })

  /** The empty mset */
  def empty[M,A]: MSet[M,A] = new MSet(Map.empty)

  /** Construct an mset where the given element occurs once */
  def singleton[M,A](a: A)(implicit M: MultiplicativeMonoid[M]): MSet[M,A] =
    new MSet(Map(a -> M.one))

}
