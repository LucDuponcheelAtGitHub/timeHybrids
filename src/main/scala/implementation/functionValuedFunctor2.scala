package implementation

import specification.{Sets, Category, Functor}

given functionValuedFunctor2[
    Set[_]: Sets,
    BTC[_, _]: Category,
    UTC1[_]: [_[_]] =>> Functor[BTC, Function, UTC1]
]: Functor[
  BTC,
  Function,
  [Z] =>> Set[UTC1[Z]]
] with

  val sets: Sets[Set] = summon[Sets[Set]]

  val btcToFunctionFunctor = summon[Functor[BTC, Function, UTC1]]

  import types.{`o`}

  import sets.{Set2, tuple2, set2}

  type UTC2 = [Z] =>> (Set2 `o` UTC1)[Z]
  def φ[Z, Y]: BTC[Z, Y] => Function[UTC2[Z], UTC2[Y]] =
    zμy =>
      val zφy = btcToFunctionFunctor.φ(zμy)
      d =>
        tuple2(d) match
          case (l, r) => set2(zφy(l), zφy(r))
