package implementation

import specification.{Sets, Ordered}

given setOrdered[Z, Set[_]: Sets]: Ordered[Set[Z]] with

  val sets: Sets[Set] = summon[Sets[Set]]

  import sets.{`=s=`, `<s<`}

  type T = [Z] =>> Set[Z]

  extension (lt: T[Z]) def `=`(rt: T[Z]): Boolean = lt `=s=` rt

  extension (lt: T[Z]) def `<`(rt: T[Z]): Boolean = lt `<s<` rt
