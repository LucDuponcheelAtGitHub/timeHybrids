package utilities.composition

import types.{CompositionEnum, Composition}

import utilities.set.{U}

import specification.{OneToOne, Limit, limit}

import implementation.{functionCategory, compositionEnumFunctionFunctor}

def compose[Z]: Set[Composition[Z]] => Composition[Z] =
  zfcs => Limit `apply` CompositionEnum.Composed(zfcs)

def decompose[Z]: Composition[Z] => Set[Composition[Z]] =
  limit `apply` {
    case CompositionEnum.Atomic(_) =>
      sys.error("atomic component cannot be decomposed")
    case CompositionEnum.Composed(zcss) => U(zcss)
  }

def compositionSetToCompositionOneToOne[Z]
    : OneToOne[Set[Composition[Z]], Composition[Z]] =
  new:
    val from: Set[types.Composition[Z]] => types.Composition[Z] = compose
    val to: types.Composition[Z] => Set[types.Composition[Z]] = decompose

def composeAll[Z]: Set[Set[Composition[Z]]] => Set[Composition[Z]] =
  compositionSetToCompositionOneToOne.fromAll

def decomposeAll[Z]: Set[Composition[Z]] => Set[Set[Composition[Z]]] =
  compositionSetToCompositionOneToOne.toAll
