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

  def mrealmLaws[A:Gen:MRealm](s: String) = Properties.properties(s)(
    "adjunction" -> forAll(monusLaw[A] _)
  ) product realmLaws[A](s)

  def cancellativeRealmLaws[A:Gen:Realm](s: String) =
    Properties.fromProps("cancellative realm",
       realmLaws[A](s),
       Properties.single("cancellativity", forAll(cancellationLaw[A] _)))

  implicit def genNatural: Gen[Natural] = nonNegativeInt.map(Natural(_))

  val natural = mrealmLaws("natural")(genNatural, NaturalRealm)
  val boolean = realmLaws("boolean")(genBoolean, BooleanRealm)
  val trivial = realmLaws("trivial")(genUnit, trivialRealm)
  val product = realmLaws("product")(
    Gen[(Natural, Natural)],
    realmProduct[Natural,Natural](NaturalRealm, NaturalRealm))

  val I = spire.std.int.IntAlgebra

  val gcd = realmLaws("euclidean")(choose(0,100), euclideanRealm[Int](I,I))
  val set = mrealmLaws("set")(setGen[Int], setRealm[Int])
}

