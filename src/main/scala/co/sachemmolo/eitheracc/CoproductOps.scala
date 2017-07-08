package co.sachemmolo.eitheracc

import shapeless.{:+:, CNil, Coproduct, DepFn2, Inl, Inr}
import shapeless.ops.coproduct.{Basis, FilterNot, Remove}

// most of this code is courtesy of Shapeless
trait CoproductUnion[L <: Coproduct, M <: Coproduct] extends DepFn2[L, M] with Serializable {
  type Out <: Coproduct

  def basisLeft: Basis[Out, L]

  def basisRight: Basis[Out, M]
}

trait LowPriorityUnion {
  type Aux[L <: Coproduct, M <: Coproduct, Out0 <: Coproduct] = CoproductUnion[L, M] {type Out = Out0}
}

object CoproductUnion extends LowPriorityUnion {
  def apply[L <: Coproduct, M <: Coproduct](implicit union: CoproductUnion[L, M]): Aux[L, M, union.Out] = union

  // let ∅ ∪ M = M
  implicit def CoproductUnion[M <: Coproduct](implicit bsr: Basis[M, M]): Aux[CNil, M, M] =
    new CoproductUnion[CNil, M] {
      type Out = M

      def apply(l: CNil, m: M): Out = m

      override def basisLeft: Basis[M, CNil] = Basis.cnilBasis

      override def basisRight: Basis[M, M] = bsr
    }

  // let (H :: T) ∪ M  =  H :: (T ∪ M) when H ∉ M
  implicit def CoproductUnion1[H, T <: Coproduct, M <: Coproduct]
  (implicit
    u: CoproductUnion[T, M],
    f: FilterNot.Aux[M, H, M]
  ): Aux[H :+: T, M, H :+: u.Out] =
    new CoproductUnion[H :+: T, M] {
      type Out = H :+: u.Out

      def apply(l: H :+: T, m: M): Out = l match {
        case Inl(head) => Inl(head)
        case Inr(tail) => Inr(u(tail, m))
      }

      override def basisLeft: Basis[:+:[H, u.Out], :+:[H, T]] = BasisOps.augBoth(u.basisLeft)

      override def basisRight: Basis[H :+: u.Out, M] = BasisOps.augSuper(u.basisRight)
    }

  // let (H :: T) ∪ M  =  H :: (T ∪ (M - H)) when H ∈ M
  implicit def CoproductUnion2[H, T <: Coproduct, M <: Coproduct, MR <: Coproduct]
  (implicit
    r: Remove.Aux[M, H, MR],
    u: CoproductUnion[T, MR]
  ): Aux[H :+: T, M, H :+: u.Out] =
    new CoproductUnion[H :+: T, M] {
      type Out = H :+: u.Out

      def apply(l: H :+: T, m: M): Out = l match {
        case Inl(head) => Inl(head)
        case Inr(tail) => Inr(u(tail, r(m).right.get))
      }

      override def basisLeft: Basis[:+:[H, u.Out], :+:[H, T]] = BasisOps.augBoth(u.basisLeft)

      override def basisRight: Basis[:+:[H, u.Out], M] = {

        val basis: Basis[H :+: u.Out, H :+: MR] = BasisOps.augBoth(u.basisRight)

        BasisOps.rewriteWithRemove(basis, r)
      }
    }
}

object BasisOps {

  def rewriteWithRemove[L<:Coproduct, MR <: Coproduct, H, M <: Coproduct](basis: Basis[L, H :+: MR], r:Remove.Aux[M, H, MR]): Basis[L, M] = new Basis[L, M] {
    override type Rest = basis.Rest

    override def inverse(e: Either[Rest, M]): L = e match {
      case Left(value) => basis.inverse(Left(value))
      case Right(value) => r.apply(value) match {
        case Left(left) => basis.inverse(Right(Inl(left)))
        case Right(right) => basis.inverse(Right(Inr(right)))
      }

    }

    override def apply(t: L): Either[Rest, M] = basis.apply(t) match {
      case Left(value) => Left(value)
      case Right(value) => value match {
        case Inl(head) => Right(r.inverse(Left(head)))
        case Inr(tail) => Right(r.inverse(Right(tail)))
      }
    }
  }

  def augBoth[L <: Coproduct, M <: Coproduct, H](basis: Basis[L, M]): Basis[H :+: L, H :+: M] = new Basis[H :+: L, H :+: M] {
    override type Rest = basis.Rest

    override def inverse(e: Either[Rest, H :+: M]): H :+: L = e match {
      case Left(value) => Inr(basis.inverse(Left(value)))
      case Right(value) => value match {
        case Inl(head) => Inl(head)
        case Inr(tail) => Inr(basis.inverse(Right(tail)))
      }
    }

    override def apply(t: H :+: L): Either[Rest, H :+: M] = t match {
      case Inl(head) => Right(Inl(head))
      case Inr(tail) => basis.apply(tail) match {
        case Left(value) => Left(value)
        case Right(value) => Right(Inr(value))
      }
    }
  }

  def augSuper[L <: Coproduct, M <: Coproduct, H](basis: Basis[L, M]): Basis[H :+: L, M] = new Basis[H :+: L, M] {
    override type Rest = H :+: basis.Rest

    override def inverse(e: Either[Rest, M]): H :+: L = e match {
      case Left(rest) => rest match {
        case Inl(head) => Inl(head)
        case Inr(tail) => Inr(basis.inverse(Left(tail)))
      }
      case Right(m) => Inr(basis.inverse(Right(m)))
    }

    override def apply(t: H :+: L): Either[Rest, M] = t match {
      case Inl(head) => Left(Inl(head))
      case Inr(tail) => basis.apply(tail) match {
        case Left(value) => Left(Inr(value))
        case Right(value) => Right(value)
      }
    }
  }

}
