package specification

import types.{U}

trait UnitNaturalTransformation[
    BTC[_, _]: Category,
    UTC[_]: [_[_]] =>> Functor[BTC, BTC, UTC]
]:

  // declared

  val unitNaturalTransformation: NaturalTransformation[BTC, BTC, U, UTC]

  // defined

  def ν[Z]: BTC[U[Z], UTC[Z]] = unitNaturalTransformation.τ
