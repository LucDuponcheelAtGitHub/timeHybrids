package specification

trait Category[BTC[_, _]] extends BtcComposition[BTC], BtcUnit[BTC]:

  // laws

  trait CategoryLaws[L[_]: Law]:

    def leftIdentity[Z, Y]: BTC[Z, Y] => L[BTC[Z, Y]] = zμy =>
      {
        ι `o` zμy
      } `=` {
        zμy
      }

    def rightIdentity[Z, Y]: BTC[Z, Y] => L[BTC[Z, Y]] = zμy =>
      {
        zμy `o` ι
      } `=` {
        zμy
      }
