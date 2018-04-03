package mset

import mset.Realm._
import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.Eq
import spire.algebra.PartialOrder
import spire.math.Natural
import spire.std.int._
import spire.std.tuples._
import spire.syntax.eq._

object MSetTests extends Scalaprops {

  import MSet._

  implicit def genMSet[M:Gen:AdditiveMonoid:Eq,A:Gen]: Gen[MSet[M,A]] =
    Gen[List[(A,M)]].map(MSet.fromOccurList[M,A])

  val equality = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    (m1 === m2) ==
      (m1.toSet.forall(x => m1(x) == m2(x)) &&
       m2.toSet.forall(x => m1(x) == m2(x)))
  }

  val sumHomomorphism = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    implicit val additive = implicitly[AdditiveMonoid[Int]].additive

    (m1 sum m2).fold(_ * _) == m1.fold(_ * _) + m2.fold(_ * _)
  }

  val size = forAll { (xs: List[(Int,Int)]) =>
    fromOccurList(xs).size == xs.map(_._2).sum
  }

  val toList = forAll { (xs: List[Int]) =>
    MSet.fromSeq[Natural,Int](xs).toList(identity).sorted == xs.sorted
  }

  val product = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    val p = (m1 product m2).occurList
    val as = m1.occurList.toMap
    val bs = m2.occurList.toMap
    p.forall {
      case ((a,b), m) => as(a) * bs(b) == m
    }
  }

  import RealmTests._

  val msetRealmLaws =
    cancellativeRealmLaws[MSet[Natural,Natural]]("MSet Realm")(
      genMSet(genNatural, naturalRealm, Natural.NaturalAlgebra, genNatural),
      msetRealm(naturalRealm, Natural.NaturalAlgebra))

  val poInt = PartialOrder[Int]

  val partialOrder = forAll { (m1: MSet[Int, Int], m2: MSet[Int, Int]) =>
    msetPartialOrder[Int,Int].lteqv(m1, m2) == (m1.toSet ++ m2.toSet).forall {
      k => poInt.lteqv(m1(k), m2(k))
    }
  }

  // |A + B| = |A| + |B| = |A ∪ B| + |A ∩ B|
  val sizeHomomorphism = forAll { (a: MSet[Int,Int], b: MSet[Int,Int]) =>
    (a sum b).size == (a.size + b.size) &&
    (a.size + b.size) == (a union b).size + (a intersect b).size &&
    (a.size * b.size) == (a product b).size
  }

  implicit val E = MSet.msetEq[Natural,(Natural,Natural)]
  implicit val I = naturalRealm
  implicit def P[A:Realm,B:Realm] = realmProduct[A,B]

  // Product distributes over realm operations
  val productDistributive = forAll {
    (a: MSet[Natural,Natural],
     b: MSet[Natural,Natural],
     c: MSet[Natural,Natural]) =>
      ((a product (b union c)) === ((a product b) union (a product c))) &&
      ((a product (b intersect c)) ===
        ((a product b) intersect (a product c))) &&
      ((a product (b sum c)) === ((a product b) sum (a product c)))
  }
}
