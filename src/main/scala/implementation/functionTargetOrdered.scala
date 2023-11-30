package implementation

import specification.{Arbitrary, Ordered}

given functionTargetOrdered[Z: Arbitrary, Y: Ordered]: Ordered[Function[Z, Y]]
with

  val az: Z = summon[Arbitrary[Z]].arbitrary

  val ordered: Ordered[Y] = summon[Ordered[Y]]

  import ordered.{`=` => `=t=`, `<` => `<t<`}

  type T = Function[Z, Y]

  extension (lt: T) def `=`(rt: T): Boolean = lt(az) `=t=` rt(az)

  extension (lt: T) def `<`(rt: T): Boolean = lt(az) `<t<` rt(az)
