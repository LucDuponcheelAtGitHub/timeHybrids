package specification

trait Triple[BTC[_, _]: Category, UTC[_]]
    extends Functor[BTC, BTC, UTC],
      CompositionNaturalTransformation[BTC, UTC],
      UnitNaturalTransformation[BTC, UTC]:

  // delegates

  val c: Category[BTC] = summon[Category[BTC]]

  // laws

  import specification.{Law}

  trait TripleLaws[L[_]: Law]:

    import types.{`o`}

    import c.{ι}

    def associativity[Z]: L[BTC[(UTC `o` UTC `o` UTC)[Z], UTC[Z]]] = {
      μ `o` μ
    } `=` {
      μ `o` φ(μ)
    }

    def leftIdentity[Z]: L[BTC[UTC[Z], UTC[Z]]] = {
      μ `o` ν
    } `=` {
      ι
    }

    def rightIdentity[Z]: L[BTC[UTC[Z], UTC[Z]]] = {
      μ `o` φ(ν)
    } `=` {
      ι
    }
