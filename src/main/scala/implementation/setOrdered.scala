package implementation

import specification.{Sets, Ordered}

given setOrdered[Z, Collection[_]: Sets]: Ordered[Collection[Z]] with

  val sets: Sets[Collection] = summon[Sets[Collection]]

  import sets.{`=s=`, `<s<`}

  type T = [Z] =>> Collection[Z]

  extension (lt: T[Z]) def `=`(rt: T[Z]): Boolean = lt `=s=` rt

  extension (lt: T[Z]) def `<`(rt: T[Z]): Boolean = lt `<s<` rt
