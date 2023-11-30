package specification

trait UtcComposition[UTC[_]]:

  // declared

  extension [Z](lutc: UTC[Z]) def `+`(rutc: UTC[Z]): UTC[Z]

  // laws

  trait UtcCompositionLaws[L[_]: Law]:

    def associativity[Z]: UTC[Z] => UTC[Z] => UTC[Z] => L[UTC[Z]] =
      lutc =>
        mutc =>
          rutc =>
            {
              (lutc `+` mutc) `+` rutc
            } `=` {
              lutc `+` (mutc `+` rutc)
            }
