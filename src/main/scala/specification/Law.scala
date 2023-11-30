package specification

trait Law[UTC[_]]:

  // declared

  extension [Z, Y](lly: UTC[Y]) def `=>`(rlz: UTC[Z]): UTC[Z]

  extension [Z](l: Z) def `=`(r: Z): UTC[Z]

  extension [Z](ll: UTC[Z]) def `&`(rl: UTC[Z]): UTC[Z]

  extension [Z](ll: UTC[Z]) def `|`(rl: UTC[Z]): UTC[Z]
