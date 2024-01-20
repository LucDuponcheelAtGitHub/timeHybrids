package types

enum CompositionEnum[Z, O]:
  case Atomic[Z, O](z: Z) extends CompositionEnum[Z, O]
  case Composed[Z, O](ls: Set[O]) extends CompositionEnum[Z, O]
