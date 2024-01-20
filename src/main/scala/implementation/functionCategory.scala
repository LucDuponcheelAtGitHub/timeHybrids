package implementation

import specification.{Category}

given functionCategory: Category[Function] with

  type Morphism = [Z, Y] =>> Function[Z, Y]

  extension [Z, Y, X](yμx: Morphism[Y, X])
    def o(zμy: Morphism[Z, Y]): Morphism[Z, X] = z => yμx(zμy(z))

  def ι[Z]: Morphism[Z, Z] = z => z

// import types.{Endo}

// import specification.{Transitions}

// given functionTransitions: Transitions[Endo[Function]] = ???

// import specification.{ActionUponFunction, MorphismFunctor} 

// given functionActionUponFunction: ActionUponFunction[Function] with

//   type BTC = [Z, Y] =>> Function[Z, Y]

//   def morphismFunctor[Z]
//       : MorphismFunctor[BTC, Function, [Y] =>> Function[Z, Y]] =
//     new:
//       def μγ[Y, X]: Function[
//         BTC[Y, X],
//         Function[Function[Z, Y], Function[Z, X]]
//       ] = yμx => zμy => yμx `oμ` zμy
