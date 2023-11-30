package specification

trait BtcComposition[BTC[_, _]]:

  // declared

  extension [Z, Y, X](yμx: BTC[Y, X]) def `o`(zμy: BTC[Z, Y]): BTC[Z, X]

  // laws

  trait CompositionLaws[L[_]: Law]:

    def associativity[Z, Y, X, W]
        : BTC[X, W] => BTC[Y, X] => BTC[Z, Y] => L[BTC[Z, W]] =
      xμw =>
        yμx =>
          zμy =>
            {
              (xμw `o` yμx) `o` zμy
            } `=` {
              xμw `o` (yμx `o` zμy)
            }
