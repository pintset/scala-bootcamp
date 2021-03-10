package com.evolutiongaming.bootcamp.typeclass.v2

import cats.Applicative

object QAndAExamples {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    // def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
    // Returns instance of A in case A is a Semigroup
    def apply[A: Semigroup]: Semigroup[A] = implicitly // shorter

    object syntax {

      implicit class SemigroupOps[A: Semigroup](x: A) {
        def combine(y: A): A = Semigroup[A].combine(x, y) // to support x.combine(y)
      }

    }

  }

  // 1.2. Implement Semigroup for Long, String
  implicit val longSemigroup: Semigroup[Long] = (x, y) => x + y
  implicit val stringSemigroup: Semigroup[String] = (x, y) => x + y
  implicit val intSemigroup: Semigroup[Int] = (x, y) => x + y

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists

  import Semigroup.syntax._

  // combineAll(List(1, 2, 3)) == 6
  def combineAll[A: Semigroup](xs: List[A]): A = xs.reduce(_ combine _)

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists

  def combineAll[A: Semigroup](xs: List[A], startingElement: A): A = xs.foldLeft(startingElement)(_ combine _)

  // combineAll(List(1, 2, 3), 0) == 6
  // combineAll(List(), 1) == 1


  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly
  }

  // 2.2. Implement Monoid for Long, String
  implicit val longMonoid: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 0

    //override def combine(x: Long, y: Long): Long = Semigroup[Long].combine(x, y)
    override def combine(x: Long, y: Long): Long = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def empty: List[T] = Nil

    override def combine(x: List[T], y: List[T]): List[T] = x ++ y
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAll[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(Monoid[A].combine(_, _))

  // combineAll(List(1, 2, 3)) == 6

  // 2.4. Implement Monoid for Option[A]
  def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(x), Some(y)) => Some(x combine y)
      case (x, y) => x orElse y
    }
  }

  // optionMonoid[Long].combine(Some(2), None) == Some(2)
  // optionMonoid[Long].combine(None, Some(2)) == Some(2)
  //  optionMonoid[Long].combine(Some(1), optionMonoid[Long].combine(Some(2), Some(3))) ==
  //    optionMonoid[Long].combine(optionMonoid[Long].combine(Some(1), Some(2)), Some(3))

  // Закомменти сверху чтобы это работало. У нас два метода combineAll
  // combineAll(List(Some(1), None, Some(3))) == Some(4)
  // combineAll(List(None, None)) == None
  // combineAll(List()) == None

  // 2.5. Implement Monoid for Function1 (for result of the function)

  implicit def functionMonoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = a => Monoid[B].combine(x(a), y(a))
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))("123") === 126

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product[A, B](fa: F[A], fb: F[B]): F[(A, B)]`,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_] : Semigroupal]: Semigroupal[F] = implicitly

    object syntax {
      implicit class SemigroupalOps[F[_] : Semigroupal, A](fa: F[A]) {
        def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
      }
    }
  }

  import Semigroupal.syntax._

  // 4.2. Implement Semigroupal for Option
  //  val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
  //    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
  //      a <- fa
  //      b <- fb
  //    } yield  (a, b)
  //  }
  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(a, b)
      case _ => None
    }
  }

  // println(Option(1).product(Option("2")))

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]

  implicit class MapNOps[F[_] : Functor : Semigroupal, A, B](x: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = x match {
      case (fa, fb) => fa.product(fb).fmap(f.tupled)
      //      case (fa, fb) => fa.product(fb).fmap {
      //        case (a, b) => f(a, b)
      //      }
    }
  }

  // (Option(1), Option(2)).mapN(_ + _) == Some(3)
  // (Option(1), None).mapN(_ + _)      == None

  // 4.4. Implement Semigroupal for Map
  implicit def mapSemigroupal[T]: Semigroupal[Map[T, *]] = new Semigroupal[Map[T, *]] {
    override def product[A, B](fa: Map[T, A], fb: Map[T, B]): Map[T, (A, B)] = {
      fa.map { case (key, aValue) =>
        fb.get(key).map { bValue => (key, (aValue, bValue)) }
      }.map { case Some(x) => x }.toMap
    }
  }

  //  implicit def mapSemigroupal: Semigroupal[Map[Int, *]] = new Semigroupal[Map[Int, *]] {
  //    override def product[A, B](fa: Map[Int, A], fb: Map[Int, B]): Map[Int, (A, B)] = {
  //      fa.map { case (key, aValue) =>
  //        fb.get(key).map { bValue => (key, (aValue, bValue)) }
  //      }.map { case Some(x) => x }.toMap
  //    }
  //  }

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly

    object syntax {

      implicit class ApplicativeOps[F[_] : Applicative, A](a: A) {
        def pure: F[A] = Applicative[F].pure(a)
      }

    }

  }

  // 5.1. Implement Applicative for Option, Either
  implicit val eitherApplicative: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)

    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] =
      fa.flatMap { a => fb.map { b => (a, b) } }

    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa.map(f)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      fa.flatMap { a => fb.map { b => (a, b) } }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](x: A): List[A] = List(x)

    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)

    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  //  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
  //    case Nil => Some(Nil)
  //    case head :: tail => f(head).flatMap { h => traverse(tail)(f).flatMap { t => Some(h :: t) } }
  //  }

  //  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
  //    case Nil => Some(Nil)
  //    case head :: tail => for {
  //      h <- f(head)
  //      t <- traverse(tail)(f)
  //    } yield h :: t
  //  }

  def traverse[A, B, F[_] : Applicative](as: List[A])(f: A => F[B]): F[List[B]] = as match {
    case Nil => Applicative[F].pure(Nil)
    case head :: tail => f(head).product(traverse(tail)(f)).fmap { case (h, t) => h :: t }
  }

  // Traverse using fold (works)
  //  def traverse[A, B, F[_]: Applicative](as: List[A])(f: A => F[B]): F[List[B]] = {
  //    val zero: F[List[B]] = Applicative[F].pure(Nil)
  //    as.foldLeft(zero) { (tail, head) => f(head).product(tail).fmap { case (h, t) => h :: t } }
  //  }

  //   traverse(List(1, 2, 3)) { i =>
  //     Option.when(i % 2 == 1)(i)
  //   } == None
  //
  //   traverse(List(1, 2, 3)) { i =>
  //     Some(i + 1)
  //   } == Some(List(2, 3, 4))

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A])(zero: B)(folder: (B, A) => B): B
  }

  object Foldable {
    def apply[F[_] : Foldable]: Foldable[F] = implicitly

    object syntax {
      implicit class FoldableOps[F[_] : Foldable, A](fa: F[A]) {
        def foldLeft[B](zero: B)(folder: (B, A) => B): B = Foldable[F].foldLeft(fa)(zero)(folder)
      }
    }
  }

  import Foldable.syntax._

  // 6.2. Implement Foldable for List
  // Note: we can use here foldLeft from standard library
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A])(zero: B)(folder: (B, A) => B): B = fa.foldLeft(zero)(folder)
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  def traverse[A, B: Semigroup, F[_] : Applicative, M[_] : Applicative : Foldable : Monoid](as: M[A])(f: A => F[B]): F[M[B]] = {
    val zero: F[M[B]] = Applicative[F].pure(Monoid[M].empty)
    as.foldLeft(zero) { (tail, head) =>
      f(head).product(tail).fmap { case (h, t) => Applicative[M].pure(h).product(t).fmap { case (h, t) => h.combine(t) } }
    }
  }
}
