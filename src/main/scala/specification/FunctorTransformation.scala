package specification

trait FunctorTransformation[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    FromMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      FromMorphed
    ],
    ToMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      ToMorphed
    ]
]:

  // declared

  def φτ[Z]: ToMorphism[FromMorphed[Z], ToMorphed[Z]]

// laws

class FunctorTransformationLawsFor[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    FromMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      FromMorphed
    ],
    ToMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      ToMorphed
    ],
    L[_]: Law
](
    t: FunctorTransformation[
      FromMorphism,
      ToMorphism,
      FromMorphed,
      ToMorphed
    ]
):

  val f_fmΦtm = summon[Functor[FromMorphism, ToMorphism, FromMorphed]]

  val t_fmΦtm = summon[Functor[FromMorphism, ToMorphism, ToMorphed]]

  import t.{φτ}

  def orderedNatural[
      Z,
      Y: [_] =>> Ordered[
        ToMorphism[FromMorphed[Z], ToMorphed[Y]]
      ]
  ]: FromMorphism[Z, Y] => L[Boolean] =
    fzμy =>
      {
        {
          φτ o f_fmΦtm.φ(fzμy)
        } <= {
          t_fmΦtm.φ(fzμy) o φτ
        }
      } `=` {
        true
      }
