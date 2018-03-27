package mset

import algebra.lattice.DistributiveLattice
import algebra.lattice.{JoinSemilatticeFunctions, MeetSemilatticeFunctions}
import algebra.Order
import algebra.ring.AdditiveCommutativeMonoid
import algebra.ring.AdditiveMonoid
import algebra.ring.AdditiveMonoidFunctions
import algebra.Monoid
import cats.kernel.OrderFunctions
import cats.kernel.instances.set._
import scala.language.higherKinds
import spire.algebra.Eq
import spire.algebra.GCDRing
import spire.math.Natural
import spire.std.boolean._
import spire.std.tuples._
import spire.std.unit._
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
      if (a === b) 0
      else if (j === b) -1
      else 1
    }

    /** The join semilattice of this realm, with identity. */
    def joinMonoid: Monoid[A] = new Monoid[A] {
      def combine(x: A, y: A): A = join(x, y)
      def empty = zero
    }
  }

trait RealmFunctions[R[T] <: Realm[T]] extends OrderFunctions[R]
  with AdditiveMonoidFunctions[R]
  with JoinSemilatticeFunctions[R]
  with MeetSemilatticeFunctions[R] {
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

  implicit val naturalLattice = new DistributiveLattice[Natural] {
    def meet(a: Natural, b: Natural) = a min b
    def join(a: Natural, b: Natural) = a max b
  }

  /** Natural numbers with addition form a realm. */
  val naturalRealm: Realm[Natural] = realm[Natural]

  /** Booleans with disjunction form a realm. */
  val booleanRealm: Realm[Boolean] = realm[Boolean]

  /** The trivial realm. */
  val trivialRealm: Realm[Unit] = new Realm[Unit] {
    def meet(a: Unit, b: Unit) = ()
    def join(a: Unit, b: Unit) = ()
    def plus(a: Unit, b: Unit) = ()
    def zero = ()
  }

  /** Realms are closed under products. */
  def realmProduct[A:Realm,B:Realm]: Realm[(A,B)] = new Realm[(A,B)] {
    def meet(a: (A,B), b: (A,B)) =
      (Realm[A].meet(a._1, b._1), Realm[B].meet(a._2, b._2))
    def join(a: (A,B), b: (A,B)) =
      (Realm[A].join(a._1, b._1), Realm[B].join(a._2, b._2))
    def plus(a: (A,B), b: (A,B)) =
      (Realm[A].plus(a._1, b._1), Realm[B].plus(a._2, b._2))
    def zero: (A,B) = (Realm[A].zero, Realm[B].zero)
  }

  /**
   * Rings with multiplication and division sometimes form a realm with GCD
   * and LCM as meet and join, respectively. E.g. the positive integers.
   */
  def gcdRealm[A:GCDRing:Eq]: Realm[A] = new Realm[A] {
    val A = GCDRing[A]
    def meet(a: A, b: A) = A.gcd(a,b)
    def join(a: A, b: A) = A.lcm(a,b)
    def plus(a: A, b: A) = A.times(a,b)
    def zero = A.one
  }

  /**
   * The realm of sets is really a specialization of the realm of MSets
   * with the measures fixed to `Boolean`.
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
    Realm[A].zero + a === a &&
    Realm[A].zero ∧ a === Realm[A].zero &&
    Realm[A].zero ∨ a === a

  def absorptionLaw[A:Realm](a: A, b: A): Boolean =
    (a ∨ (a ∧ b)) === a && (a ∧ (a ∨ b)) === a

  def idempotentLaw[A:Realm](a: A): Boolean =
    (a ∧ a) === a && (a ∨ a) === a

  def summationLaw[A:Realm](a: A, b: A): Boolean =
    (a ∨ b) + (a ∧ b) === a + b

  def cancellationLaw[A:Realm](a: A, b: A, c: A): Boolean =
    (a + c =!= b + c) || a === b
}
