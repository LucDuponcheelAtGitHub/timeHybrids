package specification

import types.{`o`}

trait CompositionNaturalTransformation[
    BTC[_, _]: Category,
    UTC[_]: [_[_]] =>> Functor[BTC, BTC, UTC]
]:

  // declared

  val compositionNaturalTransformation: NaturalTransformation[
    BTC,
    BTC,
    UTC `o` UTC,
    UTC
  ]

  // defined

  def μ[Z]: BTC[(UTC `o` UTC)[Z], UTC[Z]] = compositionNaturalTransformation.τ
