package implementation

import types.{CompositionEnum}

import specification.{Functor}

given compositionEnumFunctionFunctor[X]
    : Functor[Function, Function, [O] =>> CompositionEnum[X, O]] with
  def φ[Z, Y]: Function[Z, Y] => Function[
    CompositionEnum[X, Z],
    CompositionEnum[X, Y]
  ] = zφy =>
    case CompositionEnum.Atomic[X, Z](x) =>
      CompositionEnum.Atomic[X, Y](x)
    case CompositionEnum.Composed[X, Z](zs) =>
      CompositionEnum.Composed[X, Y](
        for {
          z <- zs
        } yield {
          zφy(z)
        }
      )