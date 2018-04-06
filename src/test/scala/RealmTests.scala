package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.math.Natural
import spire.syntax.eq._
import scalaz.std.string._
import scalaprops.Properties
import spire.std.IntAlgebra

object RealmTests extends Scalaprops {
  import Realm._

  def realmLaws[A:Gen:Realm](s: String): Properties[String] = Properties.properties(s)(
    "commutativity" -> forAll(commutativeLaw[A] _),
    "absorption" -> forAll(absorptionLaw[A] _),
    "summation" -> forAll(summationLaw[A] _),
    "identity" -> forAll(identityLaw[A] _),
    "idempotence" -> forAll(idempotentLaw[A] _),
    "distributivity" -> forAll(distributiveLaw[A] _),
    "associativity" -> forAll(associativeLaw[A] _))

  def cancellativeRealmLaws[A:Gen:Realm](s: String): Properties[String] =
    Properties.fromProps("cancellative realm",
       realmLaws[A](s),
       Properties.single("cancellativity", forAll(cancellationLaw[A] _)))

  implicit def genNatural: Gen[Natural] = nonNegativeInt.map(Natural(_))

  val natural: Properties[String] = realmLaws("natural")(genNatural, naturalRealm)
  val boolean: Properties[String] = realmLaws("boolean")(genBoolean, booleanRealm)
  val trivial: Properties[String] = realmLaws("trivial")(genUnit, trivialRealm)
  val product: Properties[String] = realmLaws("product")(
    Gen[(Natural, Natural)],
    realmProduct[Natural,Natural](naturalRealm, naturalRealm))

  val I: IntAlgebra = spire.std.int.IntAlgebra

  val gcd: Properties[String] = realmLaws("gcd")(choose(0,100), gcdRealm[Int](I,I))
  val set: Properties[String] = realmLaws("set")(setGen[Int], setRealm[Int])
}

