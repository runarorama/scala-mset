package mset

import algebra.lattice.DistributiveLattice
import algebra.lattice.{JoinSemilatticeFunctions, MeetSemilatticeFunctions}
import algebra.Order
import algebra.ring.AdditiveCommutativeMonoid
import algebra.ring.AdditiveMonoid
import algebra.ring.AdditiveMonoidFunctions
import cats.kernel.OrderFunctions
import scala.language.higherKinds
import spire.math.Natural
import spire.algebra.GCDRing
import spire.algebra.Eq
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
 * k + (m \/ n) ≡ (k + m) \/ (k <> n)
 * k + (m /\ n) ≡ (k + m) /\ (k <> n)
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

trait Realm[A] extends AdditiveCommutativeMonoid[A]
  with DistributiveLattice[A]
  with Order[A]

trait RealmFunctions[R[T] <: Realm[T]] extends OrderFunctions[R]
  with AdditiveMonoidFunctions[R]
  with JoinSemilatticeFunctions[R]
  with MeetSemilatticeFunctions[R] {
}

object Realm extends RealmFunctions[Realm] {

  @inline final def apply[A](implicit ev: Realm[A]): Realm[A] = ev

  def realm[A:AdditiveCommutativeMonoid:DistributiveLattice:Order] =
    new Realm[A] {
      val L = DistributiveLattice[A]
      val O = Order[A]
      val M = AdditiveMonoid[A]
      def meet(a: A, b: A) = L.meet(a,b)
      def join(a: A, b: A) = L.join(a,b)
      def compare(a: A, b: A) = O.compare(a,b)
      def plus(a: A, b: A) = M.plus(a,b)
      def zero = M.zero
    }

  /** Natural numbers with addition form a realm. */
  implicit val naturalRealm: Realm[Natural] = realm[Natural]

  /** Booleans with disjunction form a realm. */
  implicit val booleanRealm: Realm[Boolean] = realm[Boolean]

  /** The trivial realm. */
  implicit val trivialRealm: Realm[Unit] = realm[Unit]

  /** Realms are closed under products. */
  implicit def realmProduct[A:Realm,B:Realm]: Realm[(A,B)] = realm[(A,B)]

  /**
   * Rings with multiplication and division form a realm with GCD and LCM
   * as meet and join, respectively.
   */
  def gcdRealm[A:GCDRing:Eq]: Realm[A] = new Realm[A] {
    val A = GCDRing[A]
    def meet(a: A, b: A) = A.gcd(a,b)
    def join(a: A, b: A) = A.lcm(a,b)
    def plus(a: A, b: A) = A.plus(a,b)
    def zero = A.zero
    def compare(a: A, b: A) = {
      val j = join(a,b)
      if (a === b) 0
      else if (j === b) -1
      else 1
    }
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
