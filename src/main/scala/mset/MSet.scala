package mset

import spire.algebra.AdditiveSemigroup
import spire.algebra.Eq
import spire.algebra.Group
import spire.algebra.Monoid
import spire.algebra.Rig
import spire.algebra.MultiplicativeMonoid
import spire.algebra.MultiplicativeSemigroup
import spire.math.Natural
import spire.math.Rational
import spire.std.map._
import spire.std.seq._
import spire.std.tuples._
import spire.syntax.additiveMonoid._
import spire.syntax.multiplicativeSemigroup._
import spire.syntax.MultiplicativeSemigroupOps
import spire.syntax.eq._
import spire.syntax.monoid._

class MSet[M,A](private val rep: Map[A,M]) extends AnyVal {

  import MSet._

  /** Get the occurrence list of this MSet */
  def occurList(implicit M: Monoid[M]): List[(A,M)] = rep.toList

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

  /** Insert one occurrence of an object into this MSet */
  def insert(a: A)(implicit M: MultiplicativeMonoid[M],
                            S: AdditiveSemigroup[M]): MSet[M,A] = {
    implicit val sg = S.additive
    new MSet(MapMonoid[A,M].combine(rep, singleton[M,A](a).rep))
  }

  /** The size of an MSet is the sum of its multiplicities. */
  def size(implicit M: Monoid[M]): M =
    foldMap((_, m) => m)

  /** Modify the occurrences of elements by a function. */
  def mapOccurs[N](f: M => N): MSet[N,A] = new MSet(rep.mapValues(f))

  /** Scale this MSet by a value. */
  def scale(n: M)(implicit M: MultiplicativeSemigroup[M]): MSet[M,A] =
    mapOccurs(_ * n)

  /**
   * Pass the elements of this MSet to the given function and return the
   * results as an MSet.
   */
  def map[B](f: A => B)(implicit M: Rig[M]): MSet[M,B] = {
    implicit val additive = M.additive
    foldMap((a,m) => MSet.singleton(f(a)).scale(m))
  }

  /**
   * Pass the elements of this MSet to the given function and collect all
   * resulting MSets in one MSet.
   */
  def flatMap[B](f: A => MSet[M,B])(implicit M: Rig[M]): MSet[M,B] = {
    implicit val additive = M.additive
    foldMap((a,m) => f(a) scale m)
  }

  def ++(m: MSet[M,A])(implicit M: Monoid[M]): MSet[M,A] =
    new MSet(rep |+| m.rep)
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
      def combine(a: MSet[M,A], b: MSet[M,A]) = a ++ b
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
