package timehybrids.specification

import types.{Composition2, composition2, decomposition2}

import specification.{
  Arbitrary,
  Ordered,
  Sets,
  Category,
  ActingUponFunction,
  Functor,
  Transformation
}

import implementation.{
  orderedCategory,
  functionCategory,
  functionValuedFunctor2
}

trait PreThings[
    Set[_],
    Morphism[_, _],
    Moment,
    State: [_] =>> Universe[Set, Morphism, Moment, State],
    PreObject: [_] =>> Arbitrary[
      Set[Set[Composition2[Set, PreObject]]]
    ]: [_] =>> Functor[
      [_, _] =>> Tuple2[Moment, Moment],
      Function,
      [_] =>> Set[Composition2[Set, PreObject]]
    ]: [_] =>> Transformation[
      [_, _] =>> Tuple2[Moment, Moment],
      Function,
      [_] =>> Set[
        Set[
          Composition2[Set, PreObject]
        ]
      ],
      [_] =>> Set[Set[Composition2[Set, PreObject]]]
    ]: [_] =>> Function[Set[Composition2[Set, PreObject]], State]
]:

  // `Universe` related domain delegates are defined

  val su: Universe[Set, Morphism, Moment, State] =
    summon[Universe[Set, Morphism, Moment, State]]

  // `PreThings` related domain types are defined.

  import su.{cs}

  import cs.{Set2}

  type PreThing = Composition2[Set, PreObject]

  type PreThingsSet = Set[PreThing]

  type PreInteraction = Set2[PreThing]

  type PreInteractionsSet = Set[PreInteraction]

  // `PreThings` related foundational delegates are defined

  import su.{MomentMorphism}

  val pisa: Arbitrary[PreInteractionsSet] =
    summon[Arbitrary[PreInteractionsSet]]

  val mmΦptsf: Functor[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> PreThingsSet
  ] =
    summon[
      Functor[
        [_, _] =>> MomentMorphism,
        Function,
        [_] =>> PreThingsSet
      ]
    ]

  val ptss2Τpis: Transformation[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> Set2[PreThingsSet],
    [_] =>> PreInteractionsSet
  ] = summon[
    Transformation[
      [_, _] =>> MomentMorphism,
      Function,
      [_] =>> Set2[PreThingsSet],
      [_] =>> PreInteractionsSet
    ]
  ]

  val ptsφs: Function[PreThingsSet, State] =
    summon[Function[PreThingsSet, State]]

  // `PreThings` related foundational members
  // using members of `PreThings` related foundational delegates are defined.

  val apis: Set[PreInteraction] = pisa.arbitrary

  val mmφptsf: Function[
    MomentMorphism,
    Function[PreThingsSet, PreThingsSet]
  ] = mmΦptsf.φ

  val ptss2φtpis: Function[
    Set2[PreThingsSet],
    PreInteractionsSet
  ] = ptss2Τpis.τ

  // Foundational `given`s using `import`ed foundational delegates are defined

  import su.{mc, mauf}

  given Sets[Set] = cs

  given Category[Morphism] = mc

  given ActingUponFunction[Morphism] = mauf

  // `Time` related foundational `given`s are defined

  import su.{mt, mmΦsm}

  import mt.{ma, mo}

  given Arbitrary[Moment] = ma

  given Ordered[Moment] = mo

  // `Universe` related foundational `given`s are defined

  given Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] = mmΦsm

  // `PreThings` related foundational `given`s are defined

  given mmΦptss2f: Functor[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> Set2[PreThingsSet]
  ] = functionValuedFunctor2[
    Set,
    [_, _] =>> MomentMorphism,
    [_] =>> PreThingsSet
  ]

  // `PreThings` related foundational members
  // using `PreThings` related foundational `given`s are defined

  val mmφptss2f: Function[
    MomentMorphism,
    Function[
      Set2[PreThingsSet],
      Set2[PreThingsSet]
    ]
  ] = mmΦptss2f.φ

  // Composed `PreThings` related foundational members
  // using `PreThings` related foundational members
  // using `PreThings` related foundational `given`s are defined

  val pisφpts: Function[PreInteractionsSet, PreThingsSet] =
    pic =>
      for {
        pi <- pic
      } yield {
        composition2 apply pi
      }

  val ptsφpis: Function[PreThingsSet, PreInteractionsSet] =
    pts =>
      for {
        pt <- pts
      } yield {
        decomposition2 apply pt
      }

  val mmφpisf: Function[
    MomentMorphism,
    Function[PreInteractionsSet, PreInteractionsSet]
  ] = mm => ptsφpis `o` mmφptsf(mm) `o` pisφpts

  // laws

  import specification.{Law}

  import implementation.{composedFunctor}

  trait PreThingsFunctorCompositionLaws[L[_]: Law]:

    given smΦptsf: Functor[Morphism, Function, [_] =>> PreThingsSet]

    val composition2: L[
      Functor[
        [_, _] =>> MomentMorphism,
        Function,
        [_] =>> PreThingsSet
      ]
    ] = {
      mmΦptsf
    } `=` {
      composedFunctor[
        [_, _] =>> MomentMorphism,
        Morphism,
        Function,
        [_] =>> State,
        [_] =>> PreThingsSet
      ]
    }

  import implementation.{functionTargetOrdered, setOrdered}

  trait PreThingsLaws[L[_]: Law]:

    val preInteractionAsPreThingComposition
        : L[PreInteraction => PreInteraction] = {
      decomposition2 `o` composition2
    } `=` {
      identity
    }

    val preThingAsPreInteractionDecomposition: L[PreThing => PreThing] = {
      composition2 `o` decomposition2
    } `=` {
      identity
    }

    val noPreThingFromNothing: MomentMorphism => L[PreThingsSet] =
      mm =>
        import cs.{set0}
        {
          mmφptsf(mm)(set0)
        } `=` {
          set0
        }

    val unionOfSingletonPreInteractions
        : Set2[PreThingsSet] => L[PreInteractionsSet] =
      ptss2 =>
        import cs.{tuple2, set1, set2, union}
        tuple2(ptss2) match
          case (lpts, rpts) =>
            {
              union {
                for {
                  lpt <- lpts
                  rpt <- rpts
                } yield {
                  ptss2φtpis(set2(set1(lpt), set1(rpt)))
                }
              }
            } `=` {
              ptss2φtpis(ptss2)
            }

    val naturePreservingPreInteraction: MomentMorphism => L[Boolean] =
      mm =>
        {
          { mmφpisf(mm) `o` ptss2φtpis } `<=` {
            ptss2φtpis `o` mmφptss2f(mm)
          }
        }
          `=` {
            true
          }

  trait PreThingsPlacesLaws[L[_]: Law]:

    val orderPreserving: PreThingsSet => PreThingsSet => L[Boolean] =
      lpts =>
        rpts =>
          import su.{svt}
          import svt.{`<=`}
          {
            lpts `<=` rpts `=` true
          } `=>` {
            ptsφs(lpts) `<=` ptsφs(rpts) `=` true
          }

    val supremumOfAllSingletonPlaces: PreThingsSet => L[State] =
      pts =>
        import cs.{set1}
        import su.{svts}
        {
          svts {
            for {
              pt <- pts
            } yield ptsφs(set1(pt))
          }
        } `=` {
          ptsφs(pts)
        }

    val immobileAfter: MomentMorphism => L[Function[PreThingsSet, State]] =
      mm =>
        import su.{mmφsm}
        {
          ptsφs `o` mmφptsf(mm)
        } `=` {
          mmφsm(mm) `a` ptsφs
        }

    val immobileOnInterval: MomentMorphism => PreThingsSet => L[State] =
      case (bm, em) =>
        import cs.{Interval, interval, all}
        import su.{mmφsm}
        val mi: Interval[Moment] = interval apply ((bm, em))
        pts =>
          all apply {
            for {
              m <- mi
            } yield {
              {
                (ptsφs `o` mmφptsf((bm, m)))(pts)
              } `=` {
                (mmφsm((bm, m)) `a` ptsφs)(pts)
              }
            }
          }
