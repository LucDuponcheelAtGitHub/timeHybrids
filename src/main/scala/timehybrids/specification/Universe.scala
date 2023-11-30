package timehybrids.specification

import specification.{
  VirtualTopology,
  Sets,
  Category,
  ActingUponFunction,
  Functor
}

trait Universe[
    Collection[_]: Sets,
    Morphism[_, _]: Category: ActingUponFunction,
    Moment: [_] =>> Time[
      Moment
    ],
    State: [_] =>> Functor[
      [_, _] =>> Tuple2[Moment, Moment],
      Morphism,
      [_] =>> State
    ]: [_] =>> VirtualTopology[
      Collection,
      State
    ]
]:

  // Foundational delegates are defined

  val cs: Sets[Collection] = summon[Sets[Collection]]

  val mc: Category[Morphism] = summon[Category[Morphism]]

  val mfa: ActingUponFunction[Morphism] = summon[ActingUponFunction[Morphism]]

  // `Time` related domain delegates are defined

  val mm: Time[Moment] = summon[Time[Moment]]

  // `Time` related domain types are defined

  type MomentMorphism = Tuple2[Moment, Moment]

  // `Universe` related foundational delegates are defined.

  val mmΦst: Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] =
    summon[Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State]]

  val svt: VirtualTopology[Collection, State] =
    summon[VirtualTopology[Collection, State]]

  // `Universe` related domain types are defined

  type StateMorphism = Morphism[State, State]

  // `Universe` related foundational members
  // using members of `Universe` related foundational delegates are defined.

  val mmφst: Function[MomentMorphism, StateMorphism] = mmΦst.φ

  val ss: Function[Collection[State], State] = svt.sup

