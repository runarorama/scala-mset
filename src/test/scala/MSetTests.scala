package mset

import mset.Realm._
import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.Eq
import spire.algebra.PartialOrder
import spire.std.tuples._
import spire.syntax.all._
import algebra.Monoid

object MSetTests extends Scalaprops {

  import MSet._
  import Realm._

  implicit def genMSet[M:Gen:AdditiveMonoid:Eq,A:Gen]: Gen[MSet[M,A]] =
    Gen[List[(A,M)]].map(MSet.fromOccurList[M,A])

  val equality: Property = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    (m1 === m2) ==
      (m1.toSet.forall(x => m1(x) == m2(x)) &&
       m2.toSet.forall(x => m1(x) == m2(x)))
  }

  val sumHomomorphism: Property = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    implicit val additive: Monoid[Int] = implicitly[AdditiveMonoid[Int]].additive

    (m1 sum m2).fold(_ * _) == m1.fold(_ * _) + m2.fold(_ * _)
  }

  val size: Property = forAll { (xs: List[(Int,Int)]) =>
    fromOccurList(xs).size == xs.map(_._2).sum
  }

  val toList: Property = forAll { (xs: List[Int]) =>
    MSet.fromSeq[Nat,Int](xs).toList(identity).sorted == xs.sorted
  }

  val product: Property = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    val p = (m1 product m2).occurList
    val as = m1.occurList.toMap
    val bs = m2.occurList.toMap
    p.forall {
      case ((a,b), m) => as(a) * bs(b) == m
    }
  }

  import RealmTests._

  val msetRealmLaws: Properties[String] =
    cancellativeRealmLaws[MSet[Nat,Nat]]("MSet Realm")(
      genMSet(genNatural, naturalRealm, naturalRealm, genNatural),
      msetRealm(naturalRealm, naturalRealm))

  val poInt: PartialOrder[Int] = PartialOrder[Int]

  val partialOrder: Property = forAll { (m1: MSet[Int, Int], m2: MSet[Int, Int]) =>
    msetPartialOrder[Int,Int].lteqv(m1, m2) == (m1.toSet ++ m2.toSet).forall {
      k => poInt.lteqv(m1(k), m2(k))
    }
  }

  // |A + B| = |A| + |B| = |A ∪ B| + |A ∩ B|
  val sizeHomomorphism: Property = forAll { (a: MSet[Int,Int], b: MSet[Int,Int]) =>
    (a sum b).size == (a.size + b.size) &&
    (a.size + b.size) == (a union b).size + (a intersect b).size &&
    (a.size * b.size) == (a product b).size
  }

  implicit val E: Eq[MSet[Nat, (Nat, Nat)]] = MSet.msetEq[Nat,(Nat,Nat)]
  implicit def P[A:Realm,B:Realm]: Realm[(A, B)] = realmProduct[A,B]

  // Product distributes over realm operations
  val productDistributive: Property = forAll {
    (a: MSet[Nat,Nat],
     b: MSet[Nat,Nat],
     c: MSet[Nat,Nat]) =>
      ((a product (b union c)) === ((a product b) union (a product c))) &&
      ((a product (b intersect c)) ===
        ((a product b) intersect (a product c))) &&
      ((a product (b sum c)) === ((a product b) sum (a product c)))
  }

  val differenceHomomorphism: Property = forAll {
    (a: MSet[Int,Int], b: MSet[Int,Int], x: Int) =>
      (a difference b).apply(x) == (a(x) - b(x))
  }

  // This law doesn't quite hold for finite signed integer domains,
  // as e.g. the negation of the smallest `Int` is the identity.
  val deMorgansLaws: Property = forAll { (ax: MSet[Int,Int], bx: MSet[Int,Int]) =>
    val a = ax.mapOccurs(_.toLong)
    val b = bx.mapOccurs(_.toLong)
    ((a.negate intersect b.negate) === (a union b).negate) &&
    ((a.negate union b.negate) === (a intersect b).negate)
  }
}
