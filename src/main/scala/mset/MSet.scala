package mset

import spire.algebra.AdditiveGroup
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.algebra.Eq
import spire.algebra.Group
import spire.algebra.lattice.JoinSemilattice
import spire.algebra.lattice.MeetSemilattice
import spire.algebra.Monoid
import spire.algebra.MultiplicativeMonoid
import spire.algebra.MultiplicativeSemigroup
import spire.algebra.PartialOrder
import spire.algebra.Rig
import spire.algebra.Ring
import spire.math.Natural
import spire.math.Rational
import spire.std.map._
import spire.std.MapMonoid
import spire.std.seq._
import spire.std.tuples._
import spire.syntax.all._

/**
 * An MSet[M,A] is a multiset of values of type A with multiplicities in M.
 *
 * Whereas in a regular Set a given element occurs either zero times or once,
 * in an MSet an element can have multiple occurrences, fractional occurrences,
 * or the occurrence can be a negative number, an interval, a probability
 * distribution, or any other type of value.
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

  /** Get the list of values in this MSet */
  def toList(h: M => Natural): List[A] =
    foldLeft(List[A]()) {
      case (as, (a, m)) => List.fill(h(m).toInt)(a) ++ as
    }

  /** Check if the given predicate holds for all elements of this MSet. */
  def forall(p: (A,M) => Boolean): Boolean = rep.forall(p.tupled)

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
    foldLeft(B.empty) { case (b, (a, m)) => b |+| f(a, m) }

  def foldLeft[B](z: B)(f: (B, (A,M)) => B): B =
    rep.foldLeft(z)(f)

  def foldRight[B](z: B)(f: ((A,M), B) => B): B =
    rep.foldRight(z)(f)

  /** Insert one occurrence of an object into this MSet */
  def insert(a: A)(implicit M: MultiplicativeMonoid[M],
                            S: AdditiveMonoid[M]): MSet[M,A] =
    sum(singleton[M,A](a))

  /** Insert n occurrences of an object into this MSet */
  def insertN(a: A, n: M)(implicit S: AdditiveMonoid[M], E: Eq[M]): MSet[M,A] =
    if (n === S.zero) this
    else {
      implicit val additive = S.additive
      new MSet(MapMonoid[A,M].combine(rep, Map(a -> n)))
    }

  /** The size of an MSet is the sum of its multiplicities. */
  def size(implicit M: AdditiveMonoid[M]): M = {
    implicit val additive = M.additive
    fold((_, m) => m)
  }

  /** Modify the occurrences of elements by a function. */
  def mapOccurs[N:Eq](f: M => N)(implicit N: AdditiveMonoid[N]): MSet[N,A] =
    new MSet(rep.mapValues(f).filterNot(_._2 === N.zero))

  /** Scale this MSet by a value. */
  def scale(n: M)(
    implicit M: MultiplicativeSemigroup[M],
             N: AdditiveMonoid[M],
             E: Eq[M]): MSet[M,A] = mapOccurs(_ * n)

  /**
   * Pass the elements of this MSet to the given function and return the
   * results as an MSet.
   */
  def map[B](f: A => B)(implicit M: Rig[M], E: Eq[M]): MSet[M,B] = {
    implicit val additive = M.additive
    fold((a,m) => MSet.singleton(f(a)).scale(m))
  }

  /**
   * Pass the elements of this MSet to the given function and collect all
   * resulting MSets in one MSet.
   */
  def flatMap[B](f: A => MSet[M,B])(implicit M: Rig[M], E: Eq[M]): MSet[M,B] = {
    implicit val additive = M.additive
    fold((a,m) => f(a) scale m)
  }

  /**
   * Union one MSet with another. Also called the "direct sum" of the
   * two multisets.
   */
  def sum(m: MSet[M,A])(implicit M: AdditiveMonoid[M]): MSet[M,A] = {
    implicit val additive = M.additive
    new MSet(rep |+| m.rep)
  }

  /**
   * Negate all the occurrences in this MSet.
   * This introduces the idea of a negative multiset.
   */
  def negate(implicit M: AdditiveGroup[M], E: Eq[M]): MSet[M,A] =
    mapOccurs(M.negate)

  /**
   * The direct product of two MSets. The multiplicity of `(a,b)` in the
   * result will be the product of the multiplicities of `a` and `b` in
   * the inputs.
   */
  def product[B](m: MSet[M,B])(implicit M: Rig[M], E: Eq[M]): MSet[M,(A,B)] = for {
    a <- this
    b <- m
  } yield (a,b)

  /**
   * The union of two MSets. The multiplicity of an element in the result
   * will be the join (usually this means the max) of that element in the
   * two inputs.
   */
  def union(m: MSet[M,A])(implicit L: JoinSemilattice[M],
                                   M: AdditiveMonoid[M],
                                   E: Eq[M]): MSet[M,A] =
    new MSet(
      MapMonoid[A,M](new Monoid[M] {
        def combine(x: M, y: M) = L.join(x,y)
        def empty = M.zero
      }).combine(rep, m.rep).filterNot(_._2 === M.zero)
    )

  def intersect(m: MSet[M,A])(implicit L: MeetSemilattice[M],
                                       M: AdditiveMonoid[M],
                                       E: Eq[M]): MSet[M,A] = {
    val ks = rep.keySet intersect m.rep.keySet
    ks.foldLeft(empty[M,A]) { (nm, k) =>
      nm.insertN(k, rep(k) meet m.rep(k))
    }
  }
}

object MSet {

  type Multiset[A] = MSet[Natural,A]
  type RatBag[A] = MSet[Rational,A]
  type IntBag[A] = MSet[Int,A]

  implicit def msetEq[M:Eq:AdditiveMonoid,A:Eq]: Eq[MSet[M,A]] =
    new Eq[MSet[M,A]] {
      def eqv(a: MSet[M,A], b: MSet[M,A]) =
        a.rep == b.rep
    }

  implicit def msetMonoid[M:AdditiveMonoid,A]: AdditiveMonoid[MSet[M,A]] =
    new AdditiveMonoid[MSet[M,A]] {
      def plus(a: MSet[M,A], b: MSet[M,A]) = a sum b
      val zero = MSet.empty[M,A]
    }

  implicit def msetAdditive[M:AdditiveMonoid,A]: Monoid[MSet[M,A]] =
    msetMonoid[M,A].additive

  /** Turn an occurrence list into an MSet */
  def fromOccurList[M,A](xs: List[(A,M)])(
    implicit M: AdditiveMonoid[M], E: Eq[M]): MSet[M,A] = {
      implicit val additive = M.additive
      new MSet(xs.foldLeft(Map.empty:Map[A,M]) {
        case (s, (a, m)) if (M.zero =!= m) => s |+| Map(a -> m)
        case (s, _) => s
      })
  }

  def fromSeq[M,A](s: Seq[A])(implicit M: MultiplicativeMonoid[M],
                                       S: AdditiveMonoid[M]): MSet[M,A] =
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

  def msetRealm[M:Realm,A:Eq]: Realm[MSet[M,A]] =
    new Realm[MSet[M,A]]()(msetEq[M,A]) {
      def join(a: MSet[M,A], b: MSet[M,A]) = a union b
      def meet(a: MSet[M,A], b: MSet[M,A]) = a intersect b
      val zero = empty[M,A]
      def plus(a: MSet[M,A], b: MSet[M,A]) = a sum b
    }

  def msetGroup[M:Ring:Eq,A]: AdditiveGroup[MSet[M,A]] =
    new AdditiveGroup[MSet[M,A]] {
      def plus(a: MSet[M,A], b: MSet[M,A]) = a sum b
      def zero = MSet.empty[M,A]
      def negate(a: MSet[M,A]) = a.negate
    }

  def msetPartialOrder[M:PartialOrder:AdditiveMonoid,A]: PartialOrder[MSet[M,A]] =
    new PartialOrder[MSet[M,A]] {
      val P = PartialOrder[M]
      def partialCompare(x: MSet[M,A], y: MSet[M,A]): Double = {
        // The atrocities we commit for performance
        var these = x.toSet ++ y.toSet
        var sofar = 0.0
        while (!these.isEmpty) {
          val k = these.head
          these = these.tail
          val p = P.partialCompare(x(k), y(k))
          if (sofar == 0.0) sofar = p
          else if (p.isNaN || p.signum != sofar.signum) return Double.NaN
        }
        sofar
      }
    }


}
