package types

import specification.{Sets}

enum NestedComposition[Collection[_]: Sets, Z]:
  case Atom[Collection[_]: Sets, Z](z: Z)
      extends NestedComposition[Collection, Z]
  case Composition2[Collection[_]: Sets, Z](
      ncc: Collection[NestedComposition[Collection, Z]]
  ) extends NestedComposition[Collection, Z]

import NestedComposition.{Composition2}

def composition2[Collection[_]: Sets, Z]: Collection[
  NestedComposition[Collection, Z]
] => NestedComposition[Collection, Z] =
  Composition2.apply

def decomposition2[Collection[_]: Sets, Z]
    : NestedComposition[Collection, Z] => Collection[
      NestedComposition[Collection, Z]
    ] =

  val sets: Sets[Collection] = summon[Sets[Collection]]

  import sets.{collection2}

  nc => collection2 apply (nc, nc)
