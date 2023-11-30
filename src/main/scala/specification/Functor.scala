package specification

trait Functor[FBTC[_, _]: Category, TBTC[_, _]: Category, UTC[_]]:

  // declared

  def φ[Z, Y]: Function[FBTC[Z, Y], TBTC[UTC[Z], UTC[Y]]]

  // laws

  trait FunctorLaws[L[_]: Law]:

    def identity[Z]: L[TBTC[UTC[Z], UTC[Z]]] =
      val fbtc = summon[Category[FBTC]]
      val tbtc = summon[Category[TBTC]]
      φ(fbtc.ι[Z]) `=` tbtc.ι[UTC[Z]]

    def composition[Z, Y, X]
        : FBTC[Z, Y] => FBTC[Y, X] => L[TBTC[UTC[Z], UTC[X]]] =
      fzμy =>
        fyμx =>
          {
            φ(fyμx `o` fzμy)
          } `=` {
            φ(fyμx) `o` φ(fzμy)
          }
