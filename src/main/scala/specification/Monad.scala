package specification

trait Monad[UTC[_]] extends Triple[Function, UTC]:

  // defined

  extension [Z, Y](utcz: UTC[Z])
    def map(zφy: Function[Z, Y]): UTC[Y] = φ(zφy)(utcz)

  extension [Z, Y](utcz: UTC[Z])
    def flatMap(zφutcy: Function[Z, UTC[Y]]): UTC[Y] = μ(utcz map zφutcy)
