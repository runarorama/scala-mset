package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.math.Natural
import spire.std.int._
import spire.syntax.eq._
import scalaz.std.string._

object RealmTests extends Scalaprops {
  import Realm._

  def realmLaws[A:Gen:Realm] = Properties.properties("Realm laws")(
    "commutativity" -> forAll(commutativeLaw[A] _),
    "absorption" -> forAll(absorptionLaw[A] _),
    "summation" -> forAll(summationLaw[A] _),
    "identity" -> forAll(identityLaw[A] _),
    "idempotence" -> forAll(idempotentLaw[A] _),
    "cancellativity" -> forAll(cancellationLaw[A] _),
    "distributivity" -> forAll(distributiveLaw[A] _),
    "associativity" -> forAll(associativeLaw[A] _))

  val natural = realmLaws(nonNegativeInt.map(Natural(_)), naturalRealm)
}

