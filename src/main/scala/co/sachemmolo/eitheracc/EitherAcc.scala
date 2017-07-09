package co.sachemmolo.eitheracc

import shapeless.ops.coproduct.{Basis, Folder}
import shapeless.{:+:, CNil, Coproduct, Inl, Poly}


sealed trait EitherAcc[S <: Coproduct, A] extends Product with Serializable {
  def isError: Boolean = this match {
    case Success(_) => false
    case Err(_) => true 
  }

  def toEither: Either[S, A] = this match {
     case Success(value) => Right(value)
     case Err(err) => Left(err)
  }

  def map[B](fn : A => B):EitherAcc[S, B] = this match {
    case Success(value) => Success(fn(value))
    case Err(e) => Err(e)
  }

  def flatMap[T <: Coproduct, B](fn: A => EitherAcc[T, B])(implicit union: CoproductUnion[S, T]): EitherAcc[union.Out, B] = this match {
    case Success(value) => fn(value).widen[union.Out](union.basisRight)
    case Err(e) => Err[S, B](e).widen[union.Out](union.basisLeft)
  }

  def fold[C](err: Poly, success: A => C)(implicit folder: Folder.Aux[err.type, S, C]): C = this match {
    case Success(value) => success(value)
    case Err(e) => e.fold(err)(folder)
  }

  def widen[T <: Coproduct](implicit s:Basis[T, S]):EitherAcc[T, A] = this match {
    case Success(value) => Success(value)
    case Err(e) => Err(s.inverse(Right(e)))
  }
}
case class Success[S <: Coproduct, A](value: A) extends EitherAcc[S, A]
case class Err[S <: Coproduct, A](e: S) extends EitherAcc[S, A]

object EitherAcc {
  def pure[A, B](value:B):EitherAcc[A :+: CNil, B] = Success[A :+: CNil, B](value)
  def err[A, B](err:A):EitherAcc[A :+: CNil, B] = Err[A :+: CNil, B](Inl(err))
  def fromEither[A, B](e: Either[A, B]): EitherAcc[A :+: CNil, B] = e match {
     case Right(value) => pure(value)
     case Left(left) => err(left)
  }
}
