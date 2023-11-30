package specification

trait Plus[UTC[_]] extends UtcComposition[UTC], UtcUnit[UTC]:

  // laws

  trait PlusLaws[L[_]: Law]:

    def leftZero[Z]: UTC[Z] => L[UTC[Z]] =
      utc =>
        {
          ζ `+` utc
        } `=` {
          utc
        }

    def rightZero[Z]: UTC[Z] => L[UTC[Z]] =
      utc =>
        {
          utc `+` ζ
        } `=` {
          utc
        }
