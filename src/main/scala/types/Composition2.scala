package types

import specification.{Sets}

enum Composition2[Set[_]: Sets, Z]:
  case Atomic[Set[_]: Sets, Z](z: Z) extends Composition2[Set, Z]
  case Composed[Set[_]: Sets, Z](cs2: Set[Composition2[Set, Z]])
      extends Composition2[Set, Z]

import Composition2.{Composed}

def composition2[Set[_]: Sets, Z]
    : Set[Composition2[Set, Z]] => Composition2[Set, Z] =
  Composed.apply

def decomposition2[Set[_]: Sets, Z]
    : Composition2[Set, Z] => Set[Composition2[Set, Z]] =

  val sets: Sets[Set] = summon[Sets[Set]]

  import sets.{set2}

  c => set2 apply (c, c)
