package specification

trait Isomorphism[Morphism[_, _]: Category, Z, Y]:

  val from: Morphism[Z, Y]

  val to: Morphism[Y, Z]

  // laws

  trait IsomorphismLaws[L[_]: Law]:

    val category = summon[Category[Morphism]]

    import category.{ι}

    val fromLaw: L[Morphism[Z, Z]] = {
      to o from
    } `=` {
      ι
    }

    val toLaw: L[Morphism[Y, Y]] = {
      from o to
    } `=` {
      ι
    }
