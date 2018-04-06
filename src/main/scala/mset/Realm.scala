package mset

import algebra.lattice.DistributiveLattice
import algebra.lattice.{JoinSemilatticeFunctions, MeetSemilatticeFunctions}
import algebra.Monoid
import algebra.Order
import algebra.ring.AdditiveCommutativeGroup
import algebra.ring.AdditiveCommutativeMonoid
import algebra.ring.AdditiveGroup
import algebra.ring.AdditiveGroupFunctions
import algebra.ring.AdditiveMonoid
import algebra.ring.AdditiveMonoidFunctions
import algebra.ring.MultiplicativeMonoid
import algebra.ring.MultiplicativeMonoidFunctions
import cats.kernel.instances.set._
import cats.kernel.OrderFunctions
import scala.language.higherKinds
import spire.algebra.Eq
import spire.algebra.GCDRing
import spire.std.tuples._
import spire.syntax.all._

/**
 * A realm is an additive monoid and a distributive lattice,
 * satisfying the following:
 *
 * == Commutative laws ==
 *
 * {{{
 * m + n  ≡ n + m
 * m \/ n ≡ n \/ m
 * m /\ n ≡ n /\ m
 * }}}
 *
 * == Associative laws ==
 *
 * {{{
 * k + (m + n)   ≡ (k + m) + n
 * k \/ (m \/ n) ≡ (k \/ m) \/ n
 * k /\ (m /\ n) ≡ (k /\ m) /\ n
 * }}}
 *
 * == Distributive laws ==
 *
 * {{{
 * k + (m \/ n) ≡ (k + m) \/ (k + n)
 * k + (m /\ n) ≡ (k + m) /\ (k + n)
 * k /\ (m \/ n) ≡ (k /\ m) \/ (k /\ n)
 * k \/ (m /\ n) ≡ (k \/ m) /\ (k \/ n)
 * }}}
 *
 * == Identity laws ==
 *
 * {{{
 * zero + m ≡ m
 * zero \/ m ≡ m
 * zero /\ m ≡ mempty
 * }}}
 *
 * == Absorption laws ==
 *
 * {{{
 * m \/ (m /\ n) ≡ m
 * m /\ (m \/ n) ≡ m
 * }}}
 *
 * Idempotent laws
 *
 * {{{
 * m \/ m ≡ m
 * m /\ m ≡ m
 * }}}
 *
 * Summation law
 *
 * {{{
 * (m \/ n) + (m /\ n) ≡ m + n
 * }}}
 *
 * Some realms may additionally obey a cancellation law, and we call
 * these cancellative realms:
 *
 * {{{
 * (k + n = m + n) => (k = m)
 * }}}
 *
 * [[Realm]] extends [[spire.algebra.Order]] because any join semilattice
 * defines an order:
 *
 * {{{
 * m ≤ n ≡ m \/ n = n
 * }}}
 *
 */
abstract class Realm[A](implicit A: Eq[A])
  extends AdditiveCommutativeMonoid[A]
  with DistributiveLattice[A]
  with Order[A] {
    def compare(a: A, b: A) = {
      val j = join(a,b)
      if (A.eqv(a,b)) 0
      else if (A.eqv(j,b)) -1
      else 1
    }

    /** The join semilattice of this realm, with identity. */
    def joinMonoid: Monoid[A] = new Monoid[A] {
      def combine(x: A, y: A): A = join(x, y)
      def empty = zero
    }
  }

/**
 * A well-behaved [[Realm]] that has products will allow such products to
 * distribute over the realm operations:
 *
 * {{{
 * a * (b \/ c) ≡ (a * b) \/ (a * c)
 * a * (b /\ c) ≡ (a * b) /\ (a * c)
 * a * (b + c)  ≡ (a * b) + (a * c)
 * }}}
 */
abstract class RigRealm[A](implicit A: Eq[A]) extends Realm[A]
  with MultiplicativeMonoid[A]

abstract class RingRealm[A](implicit A: Eq[A]) extends RigRealm[A]
  with AdditiveCommutativeGroup[A]

trait RealmFunctions[R[T] <: Realm[T]] extends OrderFunctions[R]
  with AdditiveMonoidFunctions[R]
  with JoinSemilatticeFunctions[R]
  with MeetSemilatticeFunctions[R] {
}

trait RigRealmFunctions[R[T] <: RigRealm[T]] extends RealmFunctions[R]
  with MultiplicativeMonoidFunctions[R] {
}

trait RingRealmFunctions[R[T] <: RingRealm[T]] extends RigRealmFunctions[R]
  with AdditiveGroupFunctions[R] {
}

object RigRealm extends RigRealmFunctions[RigRealm] {
  @inline final def apply[A](implicit ev: RigRealm[A]): RigRealm[A] = ev
}

object RingRealm extends RingRealmFunctions[RingRealm] {
  @inline final def apply[A](implicit ev: RingRealm[A]): RingRealm[A] = ev
}

object Realm extends RealmFunctions[Realm] {

  @inline final def apply[A](implicit ev: Realm[A]): Realm[A] = ev

  def realm[A:AdditiveCommutativeMonoid:DistributiveLattice:Eq] = {
    val L = DistributiveLattice[A]
    val M = AdditiveMonoid[A]
    new Realm[A] {
      def meet(a: A, b: A) = L.meet(a,b)
      def join(a: A, b: A) = L.join(a,b)
      def plus(a: A, b: A) = {
        M.plus(a,b)
      }
      def zero = M.zero
    }
  }

  def rigRealm[A:AdditiveCommutativeMonoid
                :MultiplicativeMonoid
                :DistributiveLattice
                :Eq] = {
    val L = DistributiveLattice[A]
    val M = AdditiveMonoid[A]
    val S = MultiplicativeMonoid[A]
    new RigRealm[A] {
      def meet(a: A, b: A) = L.meet(a,b)
      def join(a: A, b: A) = L.join(a,b)
      def plus(a: A, b: A) = M.plus(a,b)
      def zero = M.zero
      def one = S.one
      def times(a: A, b: A) = S.times(a,b)
    }
  }

  def ringRealm[A:AdditiveGroup
                 :MultiplicativeMonoid
                 :DistributiveLattice
                 :Eq] = {
    val L = DistributiveLattice[A]
    val M = AdditiveGroup[A]
    val S = MultiplicativeMonoid[A]
    new RingRealm[A] {
      def meet(a: A, b: A) = L.meet(a,b)
      def join(a: A, b: A) = L.join(a,b)
      def plus(a: A, b: A) = M.plus(a,b)
      def zero = M.zero
      def one = S.one
      def times(a: A, b: A) = S.times(a,b)
      def negate(a: A) = M.negate(a)
    }
  }

  type Nat = spire.math.Natural
  val natAlgebra = spire.math.Natural.NaturalAlgebra

  val naturalLattice = new DistributiveLattice[Nat] {
    def meet(a: Nat, b: Nat) = natAlgebra.min(a,b)
    def join(a: Nat, b: Nat) = natAlgebra.max(a,b)
  }

  /** Natural numbers with addition form a realm that has products. */
  implicit val naturalRealm: Realm[Nat] = rigRealm[Nat](
    natAlgebra, natAlgebra, naturalLattice, natAlgebra)

  /** The Int min/max lattice. */
  implicit val intLattice = new DistributiveLattice[Int] {
    def meet(a: Int, b: Int) = (a:scala.runtime.RichInt) min b
    def join(a: Int, b: Int) = (a:scala.runtime.RichInt) max b
  }

  /** The Long integer min/max lattice. */
  implicit val longLattice = new DistributiveLattice[Long] {
    def meet(a: Long, b: Long) = (a:scala.runtime.RichLong) min b
    def join(a: Long, b: Long) = (a:scala.runtime.RichLong) max b
  }

  /** Integers with addition form a realm that has products and inverses. */
  implicit val intRealm = ringRealm[Int](
    spire.std.int.IntAlgebra,
    spire.std.int.IntAlgebra,
    intLattice,
    spire.std.int.IntAlgebra)

  /** Integers with addition form a realm that has products and inverses. */
  implicit val longRealm = ringRealm[Long](
    spire.std.long.LongAlgebra,
    spire.std.long.LongAlgebra,
    longLattice,
    spire.std.long.LongAlgebra)

  /** Booleans with disjunction form a realm with products and inverses. */
  implicit val booleanRealm: RingRealm[Boolean] =
    new RingRealm[Boolean]()(spire.std.boolean.BooleanStructure) {
      def meet(a: Boolean, b: Boolean) = a && b
      def join(a: Boolean, b: Boolean) = a || b
      def plus(a: Boolean, b: Boolean) = a || b
      def times(a: Boolean, b: Boolean) = a && b
      def one = true
      def zero = false
      def negate(a: Boolean) = !a
    }

  /** The trivial realm. */
  implicit val trivialRealm: RingRealm[Unit] =
    new RingRealm[Unit]()(spire.std.unit.UnitAlgebra) {
      def meet(a: Unit, b: Unit) = ()
      def join(a: Unit, b: Unit) = ()
      def plus(a: Unit, b: Unit) = ()
      def zero = ()
      def one = ()
      def times(a: Unit, b: Unit) = ()
      def negate(a: Unit) = ()
    }

  /** Realms are closed under products. */
  def realmProduct[A:Realm,B:Realm]: Realm[(A,B)] = new Realm[(A,B)] {
    def meet(a: (A,B), b: (A,B)) =
      (Realm[A].meet(a._1, b._1), Realm[B].meet(a._2, b._2))
    def join(a: (A,B), b: (A,B)) =
      (Realm[A].join(a._1, b._1), Realm[B].join(a._2, b._2))
    def plus(a: (A,B), b: (A,B)) =
      (Realm[A].plus(a._1, b._1), Realm[B].plus(a._2, b._2))
    def zero = (Realm[A].zero, Realm[B].zero)
  }

  /** Realms are closed under products. */
  def rigRealmProduct[A:RigRealm,B:RigRealm]: RigRealm[(A,B)] = {
    val R: Realm[(A,B)] = realmProduct[A,B]
    new RigRealm[(A,B)] {
      def meet(a: (A,B), b: (A,B)) = R.meet(a,b)
      def join(a: (A,B), b: (A,B)) = R.join(a,b)
      def plus(a: (A,B), b: (A,B)) = R.plus(a,b)
      def zero = R.zero
      def one = (RigRealm[A].one, RigRealm[B].one)
      def times(a: (A,B), b: (A,B)) =
        (RigRealm[A].times(a._1, b._1), RigRealm[B].times(a._2, b._2))
    }
  }

  /** Realms are closed under products. */
  def ringRealmProduct[A:RingRealm,B:RingRealm]: RingRealm[(A,B)] = {
    val R: RigRealm[(A,B)] = rigRealmProduct[A,B]
    new RingRealm[(A,B)] {
      def meet(a: (A,B), b: (A,B)) = R.meet(a,b)
      def join(a: (A,B), b: (A,B)) = R.join(a,b)
      def plus(a: (A,B), b: (A,B)) = R.plus(a,b)
      def zero = R.zero
      def one = R.one
      def times(a: (A,B), b: (A,B)) = R.times(a,b)
      def negate(a: (A,B)) =
        (RingRealm[A].negate(a._1), RingRealm[B].negate(a._2))
    }
  }

  /**
   * Rings with multiplication and division sometimes form a realm with GCD
   * and LCM as meet and join, respectively. E.g. the positive rationals and
   * probability distributions are rings in this way.
   */
  def gcdRealm[A:GCDRing:Eq]: Realm[A] = new Realm[A] {
    val A = GCDRing[A]
    def meet(a: A, b: A) = A.gcd(a,b)
    def join(a: A, b: A) = A.lcm(a,b)
    def plus(a: A, b: A) = A.times(a,b)
    def zero = A.one
  }

  /**
   * The realm of sets is a specialization of the realm of MSets with
   * the measures fixed to `Boolean`.
   */
  def setRealm[A]: Realm[Set[A]] = new Realm[Set[A]] {
    def meet(a: Set[A], b: Set[A]) = a intersect b
    def join(a: Set[A], b: Set[A]) = a union b
    def plus(a: Set[A], b: Set[A]) = a union b
    def zero = Set.empty[A]
  }

  def commutativeLaw[A:Realm](a: A, b: A): Boolean =
    ((a + b) === (b + a)) &&
    ((a ∧ b) === (b ∧ a)) &&
    ((a ∨ b) === (b ∨ a))

  def associativeLaw[A:Realm](a: A, b: A, c: A): Boolean =
    ((a + (b + c)) === ((a + b) + c)) &&
    ((a ∨ (b ∨ c)) === ((a ∨ b) ∨ c)) &&
    ((a ∧ (b ∧ c)) === ((a ∧ b) ∧ c))

  def distributiveLaw[A:Realm](a: A, b: A, c: A): Boolean =
    ((a + (b ∨ c)) === ((a + b) ∨ (a + c))) &&
    ((a + (b ∧ c)) === ((a + b) ∧ (a + c))) &&
    ((a ∧ (b ∨ c)) === ((a ∧ b) ∨ (a ∧ c))) &&
    ((a ∨ (b ∧ c)) === ((a ∨ b) ∧ (a ∨ c)))

  def identityLaw[A:Realm](a: A): Boolean =
    ((Realm[A].zero + a) === a) &&
    ((Realm[A].zero ∧ a) === Realm[A].zero) &&
    ((Realm[A].zero ∨ a) === a)

  def absorptionLaw[A:Realm](a: A, b: A): Boolean =
    (a ∨ (a ∧ b)) === a && (a ∧ (a ∨ b)) === a

  def idempotentLaw[A:Realm](a: A): Boolean =
    (a ∧ a) === a && (a ∨ a) === a

  def summationLaw[A:Realm](a: A, b: A): Boolean =
    (a ∨ b) + (a ∧ b) === a + b

  def cancellationLaw[A:Realm](a: A, b: A, c: A): Boolean =
    (a + c =!= b + c) || a === b
}
