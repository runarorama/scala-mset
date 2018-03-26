package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.algebra.Monoid
import spire.math.Natural
import spire.std.int._
import spire.syntax.eq._

object RealmTests extends Scalaprops {
  import Realm._

  def realmLaws[A:Gen:Realm] = forAll { (a: A, b: A) =>
    commutativeLaw(a,b) && absorptionLaw(a,b) && summationLaw(a,b)
  } and
  forAll { (a: A) =>
    identityLaw(a) && idempotentLaw(a)
  } and
  forAll { (a:A, b: A, c:A) =>
    cancellationLaw(a,b,c) && distributiveLaw(a,b,c) && associativeLaw(a,b,c)
  }

  val natural = realmLaws(genIntAll.map(Natural(_)), naturalRealm)
}

