package timehybrids.specification

import specification.{
  VirtualTopology,
  Sets,
  Category,
  ActionUponFunction,
  Functor
}

trait Universe[
    Set[_]: Sets,
    Morphism[_, _]: Category: ActionUponFunction,
    Moment: Time,
    State: [_] =>> VirtualTopology[Set, State]: [_] =>> Functor[
      [_, _] =>> Tuple2[Moment, Moment],
      Morphism,
      [_] =>> State
    ]
]:

  // Foundational delegates are defined

  val cs: Sets[Set] = summon[Sets[Set]]

  val mc: Category[Morphism] = summon[Category[Morphism]]

  val mauf: ActionUponFunction[Morphism] = summon[ActionUponFunction[Morphism]]

  // `Time` related domain delegates are defined

  val mt: Time[Moment] = summon[Time[Moment]]

  // `Time` related domain types are defined

  type MomentMorphism = Tuple2[Moment, Moment]

  // `Universe` related foundational delegates are defined.

  val svt: VirtualTopology[Set, State] =
    summon[VirtualTopology[Set, State]]

  val mmΦsm: Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] =
    summon[Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State]]

  // `Universe` related domain types are defined

  type StateMorphism = Morphism[State, State]

  // `Universe` related foundational members
  // using members of `Universe` related foundational delegates are defined.

  val mmφsm: Function[MomentMorphism, StateMorphism] = mmΦsm.φ

  val svts: Function[Set[State], State] = svt.sup
