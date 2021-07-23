package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.Eq
import spire.algebra.PartialOrder
import spire.std.tuples._
import spire.syntax.all._

object MSetTests extends Scalaprops {

  import MSet._
  import Realm._

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
    MSet.fromSeq[Nat,Int](xs).toList.sorted == xs.sorted
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
    cancellativeRealmLaws[MSet[Nat,Nat]]("MSet Realm")(
      genMSet(genNatural, NaturalRealm, NaturalRealm, genNatural),
      msetRealm(NaturalRealm, NaturalRealm))

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

  implicit val E = MSet.msetEq[Nat,(Nat,Nat)]
  implicit def P[A:Realm,B:Realm] = realmProduct[A,B]

  // Product distributes over realm operations
  val productDistributive = forAll {
    (a: MSet[Nat,Nat],
     b: MSet[Nat,Nat],
     c: MSet[Nat,Nat]) =>
      ((a product (b union c)) === ((a product b) union (a product c))) &&
      ((a product (b intersect c)) ===
        ((a product b) intersect (a product c))) &&
      ((a product (b sum c)) === ((a product b) sum (a product c)))
  }

  val differenceHomomorphism = forAll {
    (a: MSet[Int,Int], b: MSet[Int,Int], x: Int) =>
      (a difference b).apply(x) == (a(x) - b(x))
  }

  // This law doesn't quite hold for finite signed integer domains,
  // as e.g. the negation of the smallest `Int` is the identity.
  val deMorgansLaws = forAll { (ax: MSet[Int,Int], bx: MSet[Int,Int]) =>
    val a = ax.mapOccurs(_.toLong)
    val b = bx.mapOccurs(_.toLong)
    ((a.negate intersect b.negate) === (a union b).negate) &&
    ((a.negate union b.negate) === (a intersect b).negate)
  }

  val isEmpty = forAll { (xs: List[Int]) =>
    MSet.multisetFromSeq(xs).isEmpty == xs.isEmpty
  }

  val filter = forAll { (xs: MSet[Int,Int], n: Int) =>
    !xs.filter(_ != n).contains(n)
  }

}
