package specification

trait NaturalTransformation[
    FBTC[_, _],
    TBTC[_, _]: Category,
    FUTC[_]: [_[_]] =>> Functor[FBTC, TBTC, FUTC],
    TUTC[_]: [_[_]] =>> Functor[FBTC, TBTC, TUTC]
] extends Transformation[FBTC, TBTC, FUTC, TUTC]:

  // laws

  trait EqualityNaturalTransformationLaws[L[_]: Law](
      transformation: Transformation[FBTC, TBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[FBTC, TBTC, FUTC]]

    val tutc = summon[Functor[FBTC, TBTC, TUTC]]

    def natural[Z, Y](using
        equality: Equality[TBTC[FUTC[Z], TUTC[Y]]]
    ): FBTC[Z, Y] => L[Boolean] =
      fzφy =>
        {
          {
            transformation.τ `o` futc.φ(fzφy)
          } `=` {
            tutc.φ(fzφy) `o` transformation.τ
          }
        } `=` {
          true
        }

  trait OrderedNaturalTransformationLaws[L[_]: Law](
      transformation: Transformation[FBTC, TBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[FBTC, TBTC, FUTC]]

    val tutc = summon[Functor[FBTC, TBTC, TUTC]]

    def natural[Z, Y](using
        ordered: Ordered[TBTC[FUTC[Z], TUTC[Y]]]
    ): FBTC[Z, Y] => L[Boolean] =
      fzφy =>
        {
          {
            transformation.τ `o` futc.φ(fzφy)
          }
        } `<=` {
          {
            tutc.φ(fzφy) `o` transformation.τ
          }
        } `=` {
          true
        }
