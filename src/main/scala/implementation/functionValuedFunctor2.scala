package implementation

import specification.{Sets, Category, Functor}

given functionValuedFunctor2[
    Collection[_]: Sets,
    BTC[_, _]: Category,
    UTC1[_]: [_[_]] =>> Functor[BTC, Function, UTC1]
]: Functor[
  BTC,
  Function,
  [Z] =>> Collection[UTC1[Z]]
] with

  val sets: Sets[Collection] = summon[Sets[Collection]]

  val btcToFunctionFunctor = summon[Functor[BTC, Function, UTC1]]

  import types.{`o`}

  import sets.{Collection2, tuple2, collection2}

  type UTC2 = [Z] =>> (Collection2 `o` UTC1)[Z]
  def φ[Z, Y]: BTC[Z, Y] => Function[UTC2[Z], UTC2[Y]] =
    zμy =>
      val zφy = btcToFunctionFunctor.φ(zμy)
      d =>
        tuple2(d) match
          case (l, r) => collection2(zφy(l), zφy(r))
