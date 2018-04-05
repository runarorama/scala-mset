package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.math.Natural
import spire.syntax.eq._
import scalaz.std.string._

object RealmTests extends Scalaprops {
  import Realm._

  def realmLaws[A:Gen:Realm](s: String) = Properties.properties(s)(
    "commutativity" -> forAll(commutativeLaw[A] _),
    "absorption" -> forAll(absorptionLaw[A] _),
    "summation" -> forAll(summationLaw[A] _),
    "identity" -> forAll(identityLaw[A] _),
    "idempotence" -> forAll(idempotentLaw[A] _),
    "distributivity" -> forAll(distributiveLaw[A] _),
    "associativity" -> forAll(associativeLaw[A] _))

  def cancellativeRealmLaws[A:Gen:Realm](s: String) =
    Properties.fromProps("cancellative realm",
       realmLaws[A](s),
       Properties.single("cancellativity", forAll(cancellationLaw[A] _)))

  implicit def genNatural: Gen[Natural] = nonNegativeInt.map(Natural(_))

  val natural = realmLaws("natural")(genNatural, naturalRealm)
  val boolean = realmLaws("boolean")(genBoolean, booleanRealm)
  val trivial = realmLaws("trivial")(genUnit, trivialRealm)
  val product = realmLaws("product")(
      Gen[(Natural, Natural)],
      realmProduct[Natural,Natural](naturalRealm, naturalRealm))

  val I = spire.std.int.IntAlgebra

  val gcd = realmLaws("gcd")(choose(0,100), gcdRealm[Int](I,I))
  val set = realmLaws("set")(setGen[Int], setRealm[Int])
}

