package specification

trait Law[L[_]]:

  // declared

  extension [Z, Y](lly: L[Y]) def `=>`(rlz: L[Z]): L[Z]

  extension [Z](l: Z) def `=`(r: Z): L[Z]

  extension [Z](ll: L[Z]) def `&`(rl: L[Z]): L[Z]

  extension [Z](ll: L[Z]) def `|`(rl: L[Z]): L[Z]

  def all[Z]: Function[Set[L[Z]], L[Z]]
