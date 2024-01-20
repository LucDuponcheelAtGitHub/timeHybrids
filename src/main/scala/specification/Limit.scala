package specification

import implementation.{functionCategory}

case class Limit[
    Correspondence[_]: [_[_]] =>> Functor[Function, Function, Correspondence]
](clc: Correspondence[Limit[Correspondence]]) 

def limit[
    Correspondence[_]: [_[_]] =>> Functor[Function, Function, Correspondence],
    Z
]: (Correspondence[Z] => Z) => (Limit[Correspondence] => Z) =
  val fΦf = summon[Functor[Function, Function, Correspondence]]
  zcφz => cl => (zcφz o fΦf.φ(limit apply zcφz))(cl.clc)
