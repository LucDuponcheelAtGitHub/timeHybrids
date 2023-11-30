package specification

trait ActingUpon[LBTC[_, _], RBTC[_, _]: Category]:

  // declared

  def actionFunctor[Z]: Functor[RBTC, Function, [Y] =>> LBTC[Z, Y]]

  // defined

  extension [Z, Y, X](lyμx: RBTC[Y, X])
    def `a`(rzμy: LBTC[Z, Y]): LBTC[Z, X] = actionFunctor.φ(lyμx)(rzμy)
