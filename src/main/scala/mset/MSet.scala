package mset

import  spire.std.MapMonoid
import  spire.std.long._
import  spire.std.map._
import  spire.std.seq._
import  spire.std.tuples._
import  spire.algebra.AdditiveGroup
import  spire.algebra.AdditiveMonoid
import  spire.algebra.AdditiveSemigroup
import  spire.algebra.Eq
import  spire.algebra.EuclideanRing
import  spire.algebra.Group
import  spire.algebra.Monoid
import  spire.algebra.MultiplicativeMonoid
import  spire.algebra.MultiplicativeSemigroup
import  spire.algebra.Order
import  spire.algebra.PartialOrder
import  spire.algebra.Rig
import  spire.algebra.Ring
import  spire.algebra.TruncatedDivision
import  spire.algebra.lattice.JoinSemilattice
import  spire.algebra.lattice.MeetSemilattice
import  spire.math._
import  spire.syntax.all._

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

  /** Check if the given predicate holds for all elements of this [[MSet]]. */
  def forall(p: (A,M) => Boolean): Boolean = rep.forall(p.tupled)

  /**
   * Get the multiplicity of a given object in this [[MSet]].
   * Alias for [[multiplicity]].
   */
  def apply(a: A)(implicit M: AdditiveMonoid[M]): M =
    rep get a getOrElse M.zero

  /**
   * Get the multiplicity of a given object in this [[MSet]].
   * Alias for [[apply]].
   */
  def multiplicity(a: A)(implicit M: AdditiveMonoid[M]): M = apply(a)

  /** An object occurs in the MSet if its multiplicity is nonzero */
  def contains(a: A)(implicit E: Eq[M], M: AdditiveMonoid[M]): Boolean =
    !apply(a).isZero

  /**
   * `a` is from `b` when every element of `a` also occurs in `b`.
   * This relation forms a partial order on `MSet`s.
   */
  def isFrom(m: MSet[M,A])(implicit E: Eq[M], M: AdditiveMonoid[M]): Boolean =
    forall((a, _) => m contains a)

  /** Fold an `MSet` with a monoid */
  def fold[B](f: (A,M) => B)(implicit B: Monoid[B]): B =
    foldLeft(B.empty) { case (b, (a, m)) => b |+| f(a, m) }

  def foldLeft[B](z: B)(f: (B, (A,M)) => B): B =
    rep.foldLeft(z)(f)

  def foldRight[B](z: B)(f: ((A,M), B) => B): B =
    rep.foldRight(z)(f)

  /**
   * Find all the elements that match a given predicate.
   */
  def filter(f: A => Boolean)(implicit N: MRealm[M], E: Eq[M]): MSet[M,A] =
    foldRight(this) {
      case ((a,_), r) => if (f(a)) r else r deleteAll a
    }

  /** Insert one occurrence of an object into this MSet */
  def insert(a: A)(implicit M: MultiplicativeMonoid[M],
                            S: AdditiveMonoid[M],
                            E: Eq[M]): MSet[M,A] =
    sum(singleton[M,A](a))

  /** Insert n occurrences of an object into this MSet */
  def insertN(a: A, n: M)(implicit S: AdditiveMonoid[M], E: Eq[M]): MSet[M,A] =
    if (n === S.zero) this
    else {
      implicit val additive = S.additive
      new MSet(MapMonoid[A,M].combine(rep, Map(a -> n)))
    }

  /** Delete an occurrence of an object from this MSet */
  def delete(a: A)(implicit N: MRealm[M],
                            M: MultiplicativeMonoid[M],
                            E: Eq[M]): MSet[M,A] =
    difference(singleton[M,A](a))

  /** Delete all occurrences of an object from this MSet */
  def deleteAll(a: A)(implicit N: MRealm[M], E: Eq[M]): MSet[M,A] =
    difference(MSet.fromOccurList(List(a -> multiplicity(a))))

  /**
   * The power-mset containin all sub-msets of this mset, equivalent to the
   * mset generated from the powerlist of the occurrence list. Contains every
   * combination of elements from this mset, where an object is counted as
   * an element as many times as it occurs in this mset.
   *
   * For example, the `powerMSet` of the mset `[1->2, 2->1]` is
   *
   * {{{
   * [[]->1, [1->1]->2, [1->2]->1, [2->1]->1, [1->1,2->1]->2, [1->2,2->1]->1]
   * }}}
   */
  def powerMSet(implicit M: Ring[M],
                         N: EuclideanRing[M],
                         L: JoinSemilattice[M],
                         E: Eq[M]): MSet[Long,MSet[Long,A]] =
    fromSeq(occurList.foldRight(List(List[(A,Long)]())) {
      case ((x,n), ps) => ps ++ (for {
        m <- ps
        nn = N.euclideanFunction(n).toLong
        k <- List.range(1L, nn + 1L)
        p <- List.range(0L, choose(nn, k).toLong).map(_ => List(x -> k))
      } yield p ++ m)
    }.map { case xs => fromOccurList(xs) })

  /** The size of an MSet is the sum of its multiplicities. */
  def size(implicit M: AdditiveMonoid[M]): M = {
    implicit val additive = M.additive
    fold((_, m) => m)
  }

  /** Modify the occurrences of elements by a function. */
  def mapOccurs[N:Eq](f: M => N)(implicit N: AdditiveMonoid[N]): MSet[N,A] =
    new MSet(rep.mapValues(f))

  /** Scale this MSet by a value. */
  def scale(n: M)(
    implicit M: MultiplicativeSemigroup[M],
             N: AdditiveMonoid[M],
             E: Eq[M]): MSet[M,A] = mapOccurs(_ * n)

  /**
   * Pass the elements of this MSet to the given function and return the
   * results as an MSet.
   */
  def map[B](f: A => B)(
    implicit S: MultiplicativeMonoid[M],
             M: AdditiveMonoid[M],
             E: Eq[M]): MSet[M,B] = {
    implicit val additive = M.additive
    fold((a,m) => MSet.singleton[M,B](f(a)).scale(m))
  }

  /**
   * Pass the elements of this MSet to the given function and collect all
   * resulting MSets in one MSet.
   */
  def flatMap[B](f: A => MSet[M,B])(
    implicit S: MultiplicativeSemigroup[M],
             M: AdditiveMonoid[M],
             E: Eq[M]): MSet[M,B] = {
    implicit val additive = M.additive
    fold((a,m) => f(a) scale m)
  }

  /**
   * Union one MSet with another. Also called the "direct sum" of the
   * two multisets.
   */
  def sum(m: MSet[M,A])(implicit M: AdditiveMonoid[M], E: Eq[M]): MSet[M,A] = {
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
   * Subtract one MSet from another. For any `x`, the multiplicity
   * `(a difference b)(x)` will be `a(x) - b(x)`.
   *
   * For example, if `A = [3 3 1 1]` and `B = [2 3 1 1]`,
   * then `A difference B = [(2) 3]`.
   */
  def difference(m: MSet[M,A])(implicit M: MRealm[M], E: Eq[M]): MSet[M,A] =
    new MSet(m.foldLeft(rep) { case (r, (a, m)) =>
      val newm = M.monus(multiplicity(a), m)
      if (newm === M.zero) r - a else r + (a -> newm)
    })

  /**
   * Set theoretic difference. Intersects before subtracting. Not functoral,
   * and doesn't obey any interesting laws, but is included here as it's a
   * common operation on multisets.
   *
   * For example, if `A = [3 3 1 1]` and `B = [2 3 1 1]`,
   * then `A setDifference B = [3]`.
   */
  def setDifference(m: MSet[M,A])(implicit M: MRealm[M], E: Eq[M]): MSet[M,A] =
      difference(intersect(m))

  /**
   * The direct product of two MSets. The multiplicity of `(a,b)` in the
   * result will be the product of the multiplicities of `a` and `b` in
   * the inputs.
   *
   * For example, if `A = [1 3 1]` and `B = [2 3]`,
   * then `A product B = [(1,2) (1,2) (1,3) (1,3) (3,2) (3,3)]`.
   */
  def product[B](m: MSet[M,B])(
    implicit M: AdditiveMonoid[M],
             S: MultiplicativeMonoid[M],
             E: Eq[M]): MSet[M,(A,B)] = productBy(m)((_,_))

  /**
   * A product of two MSets. The multiplicity of `f(a,b)` in the
   * result will be the product of the multiplicities of `a` and `b` in
   * the inputs.
   *
   * For example, if `A = [1 3 1]` and `B = [2 3]`,
   * then `A.productBy(B)(_ + _) = [3 3 4 4 5 6]`.
   */
  def productBy[B,C](m: MSet[M,B])(f: (A,B) => C)(
    implicit M: AdditiveMonoid[M],
             S: MultiplicativeMonoid[M],
             E: Eq[M]): MSet[M,C] =
    for {
      a <- this
      b <- m
    } yield f(a,b)

  /**
   * The union of two MSets. The multiplicity of an element in the result
   * will be the join (usually this means the max) of that element in the
   * two inputs.
   *
   * For example, if `A = [2 3 1 1]` and `B = [1 3 3 1 1]`,
   * then `A union B = [2 3 3 1 1 1]`.
   */
  def union(m: MSet[M,A])(implicit L: JoinSemilattice[M],
                                   M: AdditiveMonoid[M],
                                   E: Eq[M]): MSet[M,A] = {
    val ks = toSet ++ m.toSet
    ks.foldLeft(empty[M,A]) { (nm, k) =>
      nm.insertN(k, multiplicity(k) join m(k))
    }
  }

  /**
   * The intersection of two `MSet`s. The multiplicity of an element in the
   * result will be the meet (usually this means the min) of that element in
   * the two inputs.
   *
   * For example, if `A = [2 3 1 1]` and `B = [1 3 3 1 1]`,
   * then `A intersect B = [3 1 1]`.
   */
  def intersect(m: MSet[M,A])(implicit L: MeetSemilattice[M],
                                       M: AdditiveMonoid[M],
                                       E: Eq[M]): MSet[M,A] = {
    // This is not set-theoretic intersection.
    // We need to consider all the keys, as some multiplicities may be
    // negative.
    val ks = toSet ++ m.toSet
    ks.foldLeft(empty[M,A]) { (nm, k) =>
      nm.insertN(k, multiplicity(k) meet m(k))
    }
  }

  private def normal(implicit E: Eq[M], M: AdditiveMonoid[M]): MSet[M,A] =
    new MSet(rep.filter { case (_, m) => m =!= M.zero })

  /** An mset is empty if for all m:M, multiplicity of m is zero. */
  def isEmpty(implicit M: AdditiveMonoid[M], E: Eq[M]): Boolean =
    normal.rep.isEmpty
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

  implicit def msetMonoid[M:AdditiveMonoid:Eq,A]: AdditiveMonoid[MSet[M,A]] =
    new AdditiveMonoid[MSet[M,A]] {
      def plus(a: MSet[M,A], b: MSet[M,A]) = a sum b
      val zero = MSet.empty[M,A]
    }

  implicit def msetAdditive[M:AdditiveMonoid:Eq,A]: Monoid[MSet[M,A]] =
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

  def fromSeq[M:MultiplicativeMonoid:AdditiveMonoid:Eq,A](
    s: Seq[A]): MSet[M,A] =
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
    new Realm[MSet[M,A]] {
      def join(a: MSet[M,A], b: MSet[M,A]) = a union b
      def meet(a: MSet[M,A], b: MSet[M,A]) = a intersect b
      val zero = empty[M,A]
      def plus(a: MSet[M,A], b: MSet[M,A]) = a sum b
      override def eqv(a: MSet[M,A], b: MSet[M,A]) = a === b
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
