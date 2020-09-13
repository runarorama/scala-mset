package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.math.Natural

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

  def leftMrealmLaws[A:Gen:MRealm](s: String) = Properties.properties(s)(
    "left adjoint" -> forAll(leftMonusLaw[A] _)
  ) product realmLaws[A](s)

  def rightMrealmLaws[A:Gen:MRealm](s: String) = Properties.properties(s)(
    "right adjoint" -> forAll(rightMonusLaw[A] _)
  ) product realmLaws[A](s)

  def cancellativeRealmLaws[A:Gen:Realm](s: String) =
    Properties.fromProps("cancellative realm",
       realmLaws[A](s),
       Properties.single("cancellativity", forAll(cancellationLaw[A] _)))

  implicit def genNatural: Gen[Natural] = nonNegativeInt.map(Natural(_))

  val natural = leftMrealmLaws("natural")(genNatural, NaturalRealm)
  val boolean = realmLaws("boolean")(genBoolean, BooleanRealm)
  val trivial = realmLaws("trivial")(genUnit, trivialRealm)
  val product = realmLaws("product")(
    Gen[(Natural, Natural)],
    realmProduct[Natural,Natural](NaturalRealm, NaturalRealm))

  val I = spire.std.int.IntAlgebra

  val gcd = leftMrealmLaws("euclidean")(choose(0,100), euclideanRealm[Int](I,I))
  val set = leftMrealmLaws("set")(setGen[Int], setRealm[Int])
}

