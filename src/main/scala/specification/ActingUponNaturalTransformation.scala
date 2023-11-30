package specification

trait ActingUponNaturalTransformation[
    FBTC[_, _]: Category: [_[_, _]] =>> ActingUpon[FBTC, TBTC],
    TBTC[_, _]: Category,
    FUTC[_]: [_[_]] =>> Functor[TBTC, FBTC, FUTC],
    TUTC[_]: [_[_]] =>> Functor[TBTC, TBTC, TUTC]
] extends Transformation[TBTC, FBTC, FUTC, TUTC]:

  // laws

  trait ActingUponNaturalTransformationLaws[L[_]: Law](
      transformation: NaturalTransformation[TBTC, FBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[TBTC, FBTC, FUTC]]

    val tutc = summon[Functor[TBTC, TBTC, TUTC]]

    def natural[Z, Y]: TBTC[Z, Y] => L[FBTC[FUTC[Z], TUTC[Y]]] =
      fzφy =>
        {
          transformation.τ `o` futc.φ(fzφy)
        } `=` {
          tutc.φ(fzφy) `a` transformation.τ
        }
