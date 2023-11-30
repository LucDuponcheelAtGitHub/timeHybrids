package specification

trait Transformation[FBTC[_, _], TBTC[_, _], FUTC[_], TUTC[_]]:

  // declared

  def τ[Z]: TBTC[FUTC[Z], TUTC[Z]]
