package specification

trait Functor[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    Morphed[_]
]:

  // declared

  def φ[Z, Y]: Function[
    FromMorphism[Z, Y],
    ToMorphism[Morphed[Z], Morphed[Y]]
  ]

  // defined

  type FromTransition = [Z] =>> FromMorphism[Z, Z]

  type ToTransition = [Z] =>> ToMorphism[Z, Z]

  // laws

  trait FunctorLaws[L[_]: Law]:

    def identity[Z]: L[ToMorphism[Morphed[Z], Morphed[Z]]] =
      val fm = summon[Category[FromMorphism]]
      val tm = summon[Category[ToMorphism]]
      φ(fm.ι) `=` tm.ι

    def composition[Z, Y, X]: FromMorphism[Z, Y] => FromMorphism[Y, X] => L[
      ToMorphism[Morphed[Z], Morphed[X]]
    ] =
      fzμy =>
        fyμx =>
          {
            φ(fyμx `o` fzμy)
          } `=` {
            φ(fyμx) `o` φ(fzμy)
          }
