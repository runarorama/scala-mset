package mset

import scalaprops._
import scalaprops.Gen._
import scalaprops.Property._
import spire.algebra.AdditiveMonoid
import spire.algebra.AdditiveSemigroup
import spire.std.int._
import spire.syntax.eq._

object MSetTests extends Scalaprops {

  import MSet._

  implicit def genMSet[M:Gen:AdditiveSemigroup,A:Gen]: Gen[MSet[M,A]] =
    Gen[List[(A,M)]].map(MSet.fromOccursList[M,A])

  val equality = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    (m1 === m2) ==
      (m1.toSet.forall(x => m1(x) == m2(x)) &&
       m2.toSet.forall(x => m1(x) == m2(x)))
  }

  val homomorphism = forAll { (m1: MSet[Int,Int], m2: MSet[Int,Int]) =>
    implicit val additive = implicitly[AdditiveMonoid[Int]].additive

    (m1 ++ m2).fold(_ * _) == m1.fold(_ * _) + m2.fold(_ * _)
  }

}
