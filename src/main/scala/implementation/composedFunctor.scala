package implementation

import specification.{Category, Functor}

import types.{`o`}

given composedFunctor[
    FBTC[_, _]: Category,
    MBTC[_, _]: Category,
    TBTC[_, _]: Category,
    F2MUTC[_]: [_[_]] =>> Functor[FBTC, MBTC, F2MUTC],
    M2TUTC[_]: [_[_]] =>> Functor[MBTC, TBTC, M2TUTC]
]: Functor[FBTC, TBTC, M2TUTC `o` F2MUTC] with

  type UTC = [Z] =>> (M2TUTC `o` F2MUTC)[Z]

  def φ[Z, Y]: Function[FBTC[Z, Y], TBTC[UTC[Z], UTC[Y]]] =
    summon[Functor[MBTC, TBTC, M2TUTC]].φ `o`
      summon[Functor[FBTC, MBTC, F2MUTC]].φ