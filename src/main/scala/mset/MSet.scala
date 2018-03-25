package mset

import spire.algebra.AdditiveSemigroup
import spire.algebra.AdditiveGroup
import spire.algebra.Eq
import spire.algebra.Group
import spire.algebra.Monoid
import spire.algebra.Rig
import spire.algebra.Ring
import spire.algebra.AdditiveMonoid
import spire.algebra.MultiplicativeMonoid
import spire.algebra.MultiplicativeSemigroup
import spire.math.Natural
import spire.math.Rational
import spire.std.map._
import spire.std.seq._
import spire.std.tuples._
import spire.syntax.all._

/**
 * An MSet[M,A] is a multiset of values of type A with multiplicities in M.
 *
 * Whereas in a regular Set a given element occurs either zero times or once,
 * in an MSet an element can have multiple occurrences, fractional occurrences,
 * or even negative occurrences.
 *
 * Based on "A new look at multisets" by Norman J Wildberger.
 */
class MSet[M,A](private val rep: Map[A,M]) extends AnyVal {

  import MSet._

  override def toString = "MSet(" + rep.toString + ")"

  /** Get the occurrence list of this MSet */
  def occurList(implicit M: AdditiveMonoid[M]): List[(A,M)] = rep.toList

  /** Get the values of this MSet as a Set */
  def toSet: Set[A] = rep.keySet

  /** Get the multiplicity of a given object */
  def apply(a: A)(implicit M: AdditiveMonoid[M]): M =
    rep get a getOrElse M.zero

  /** An object occurs in the MSet if its multiplicity is nonzero */
  def contains(a: A)(implicit E: Eq[M], M: AdditiveMonoid[M]): Boolean =
    !apply(a).isZero

  /** `a` is from `b` when every element of `a` also occurs in `b` */
  def isFrom(m: MSet[M,A])(implicit E: Eq[M], M: AdditiveMonoid[M]): Boolean =
    rep.foldLeft(true) { case (b, (a,_)) => b && (m contains a) }

  /** Fold an MSet with a monoid */
  def fold[B](f: (A,M) => B)(implicit B: Monoid[B]): B =
    rep.foldLeft(B.empty) { case (b, (a, m)) => b |+| f(a, m) }

  /** Insert one occurrence of an object into this MSet */
  def insert(a: A)(implicit M: MultiplicativeMonoid[M],
                            S: AdditiveSemigroup[M]): MSet[M,A] = {
    implicit val sg = S.additive
    new MSet(MapMonoid[A,M].combine(rep, singleton[M,A](a).rep))
  }

  /** The size of an MSet is the sum of its multiplicities. */
  def size(implicit M: AdditiveMonoid[M]): M = {
    implicit val additive = M.additive
    fold((_, m) => m)
  }

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
    fold((a,m) => MSet.singleton(f(a)).scale(m))
  }

  /**
   * Pass the elements of this MSet to the given function and collect all
   * resulting MSets in one MSet.
   */
  def flatMap[B](f: A => MSet[M,B])(implicit M: Rig[M]): MSet[M,B] = {
    implicit val additive = M.additive
    fold((a,m) => f(a) scale m)
  }

  /**
   * Union one MSet with another.
   */
  def ++(m: MSet[M,A])(implicit M: AdditiveMonoid[M]): MSet[M,A] = {
    implicit val additive = M.additive
    new MSet(rep |+| m.rep)
  }

  /**
   * Negate all the occurrences in this MSet.
   * This introduces the idea of a negative multiset.
   */
  def negate(implicit M: Ring[M]): MSet[M,A] =
    mapOccurs(m => m - M.one)
}

object MSet {

  type Multiset[A] = MSet[Natural,A]
  type RatBag[A] = MSet[Rational,A]
  type IntBag[A] = MSet[Int,A]

  implicit def msetEq[M:Eq:AdditiveMonoid,A:Eq]: Eq[MSet[M,A]] =
    new Eq[MSet[M,A]] {
      def eqv(a: MSet[M,A], b: MSet[M,A]) =
        a.occurList === b.occurList
    }

  implicit def msetMonoid[M:AdditiveMonoid,A]: AdditiveMonoid[MSet[M,A]] =
    new AdditiveMonoid[MSet[M,A]] {
      def plus(a: MSet[M,A], b: MSet[M,A]) = a ++ b
      val zero = MSet.empty[M,A]
    }

  implicit def msetAdditive[M:AdditiveMonoid,A]: Monoid[MSet[M,A]] =
    msetMonoid[M,A].additive

  /** Turn an occurrence list into an MSet */
  def fromOccursList[M,A](xs: List[(A,M)])(
    implicit M: AdditiveSemigroup[M]): MSet[M,A] = {
      implicit val additive = M.additive
      new MSet(xs.foldLeft(Map.empty:Map[A,M]) {
        case (s, (a, m)) => s |+| Map(a -> m)
      })
  }

  def fromSeq[M,A](s: Seq[A])(implicit M: MultiplicativeMonoid[M],
                                       S: AdditiveSemigroup[M]): MSet[M,A] =
    s.foldLeft(empty[M,A])(_ insert _)

  /** Turn a sequence of elements into a multiset */
  def multisetFromSeq[A](s: Seq[A]): Multiset[A] = fromSeq(s)

  /** The empty mset */
  def empty[M,A]: MSet[M,A] = new MSet(Map.empty)

  /** The empty multiset */
  def emptyMultiset[A]: Multiset[A] = empty[Natural,A]

  /** The empty rational multiset */
  def emptyRatBag[A]: RatBag[A] = empty[Rational,A]

  /** Construct an mset where the given element occurs once */
  def singleton[M,A](a: A)(implicit M: MultiplicativeMonoid[M]): MSet[M,A] =
    new MSet(Map(a -> M.one))

}
