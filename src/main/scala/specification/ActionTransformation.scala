package specification

trait ActorTransformation[
    ActorMorphism[_, _]: Category: [_[_, _]] =>> Action[
      ActorMorphism,
      UponMorphism
    ],
    UponMorphism[_, _]: Category,
    UponMorphed[_]: [_[_]] =>> Functor[
      ActorMorphism,
      UponMorphism,
      UponMorphed
    ],
    ActorMorphed[_]: [_[_]] =>> Functor[
      ActorMorphism,
      ActorMorphism,
      ActorMorphed
    ]
]:

  // declared

  def ατ[Z]: UponMorphism[UponMorphed[Z], ActorMorphed[Z]]

  // defined

  val amΦum = summon[Functor[ActorMorphism, UponMorphism, UponMorphed]]

  extension [Z, Y](
      amΦam: Functor[ActorMorphism, ActorMorphism, ActorMorphed]
  )
    def isNaturalFor(zμy: ActorMorphism[Z, Y]): Boolean = {
      ατ `o` amΦum.φ(zμy)
    } == {
      amΦam.φ(zμy) `a` ατ
    }
