package timehybrids.specification

import specification.{VirtualTopology, Sets, Category, FunctionActingUpon, Functor}

trait Universe[
    Collection[_]: Sets,
    Morphism[_, _]: Category: FunctionActingUpon,
    Moment: [_] =>> Time[
      Moment
    ],
    State: [_] =>> Function[
      Moment,
      State
    ]: [_] =>> Functor[
      [_, _] =>> Option[Tuple2[Moment, Moment]],
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

  val mfa: FunctionActingUpon[Morphism] = summon[FunctionActingUpon[Morphism]]

  // `Time` related domain delegates are defined

  val mm: Time[Moment] = summon[Time[Moment]]

  // `Time` related domain types are defined

  type MomentMorphism = Option[Tuple2[Moment, Moment]]

  // `Universe` related foundational delegates are defined

  val mφs: Function[Moment, State] = summon[Function[Moment, State]]

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

  // `Universe` related foundational members
  // using `Time` foundational related members
  // and `Universe` related foundational members are defined

  import mm.{am}

  val as: State = mφs(am)
