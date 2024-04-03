package specification

trait Composition[Morphism[_, _]]:

  // declared

  extension [Z, Y, X](yμx: Morphism[Y, X])
    def o(zμy: Morphism[Z, Y]): Morphism[Z, X]

  // defined

  type Transition = [Z] =>> Morphism[Z, Z]

  extension [Z, Y, X](zτs: Seq[Transition[Z]])
    def composeAllWith(zτ: Transition[Z]): Transition[Z] =
      zτs.foldRight(zτ)(_ `o` _)

  // laws

  trait CompositionLaws[L[_]: Law]:

    def associativity[Z, Y, X, W]
        : Morphism[Z, Y] => Morphism[Y, X] => Morphism[X, W] => L[
          Morphism[Z, W]
        ] =
      zμy =>
        yμx =>
          xμw =>
            {
              (xμw `o` yμx) `o` zμy
            } `=` {
              xμw `o` (yμx `o` zμy)
            }
