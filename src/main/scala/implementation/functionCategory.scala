package implementation

import specification.{Category, ActionUponFunction, Functor}

given functionCategory: Category[Function] with

  type BTC = [Z, Y] =>> Function[Z, Y]

  extension [Z, Y, X](yμx: BTC[Y, X])
    def `o`(zμy: BTC[Z, Y]): BTC[Z, X] = z => yμx(zμy(z))

  def ι[Z]: BTC[Z, Z] = z => z

given functionFunctionActionUpon: ActionUponFunction[Function] with

  type BTC = [Z, Y] =>> Function[Z, Y]

  def actionFunctor[Z]: Functor[BTC, Function, [Y] =>> Function[Z, Y]] =
    new:
      def φ[Y, X]: Function[
        BTC[Y, X],
        Function[Function[Z, Y], Function[Z, X]]
      ] = yμx => zμy => yμx `o` zμy
