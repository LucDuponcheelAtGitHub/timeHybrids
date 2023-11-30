package timehybrids.specification

import types.{NestedComposition, composition2, decomposition2}

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
    Collection[_],
    Morphism[_, _],
    Moment,
    State: [_] =>> Universe[Collection, Morphism, Moment, State],
    PreObject: [_] =>> Arbitrary[
      Collection[
        Collection[
          NestedComposition[Collection, PreObject]
        ]
      ]
    ]: [_] =>> Functor[
      [_, _] =>> Tuple2[Moment, Moment],
      Function,
      [_] =>> Collection[NestedComposition[Collection, PreObject]]
    ]: [_] =>> Transformation[
      [_, _] =>> Tuple2[Moment, Moment],
      Function,
      [_] =>> Collection[
        Collection[
          NestedComposition[Collection, PreObject]
        ]
      ],
      [_] =>> Collection[
        Collection[
          NestedComposition[Collection, PreObject]
        ]
      ]
    ]: [_] =>> Function[
      Collection[NestedComposition[Collection, PreObject]],
      State
    ]
]:

  // `Universe` related domain delegates are defined

  val su: Universe[Collection, Morphism, Moment, State] =
    summon[Universe[Collection, Morphism, Moment, State]]

  // `PreThings` related domain types are defined.

  import su.{cs}

  import cs.{Collection2}

  type PreThing = NestedComposition[Collection, PreObject]

  type PreThingsCollection = Collection[PreThing]

  type PreInteraction = Collection2[PreThing]

  type PreInteractionsCollection = Collection[PreInteraction]

  // `PreThings` related foundational delegates are defined

  import su.{MomentMorphism}

  val pica: Arbitrary[PreInteractionsCollection] =
    summon[Arbitrary[PreInteractionsCollection]]

  val mmΦptcf: Functor[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> PreThingsCollection
  ] =
    summon[
      Functor[
        [_, _] =>> MomentMorphism,
        Function,
        [_] =>> PreThingsCollection
      ]
    ]

  val ptcc2Τpic: Transformation[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> Collection2[PreThingsCollection],
    [_] =>> PreInteractionsCollection
  ] = summon[
    Transformation[
      [_, _] =>> MomentMorphism,
      Function,
      [_] =>> Collection2[PreThingsCollection],
      [_] =>> PreInteractionsCollection
    ]
  ]

  val ptcφs: Function[PreThingsCollection, State] =
    summon[Function[PreThingsCollection, State]]

  // `PreThings` related foundational members
  // using members of `PreThings` related foundational delegates are defined.

  val apic: Collection[PreInteraction] = pica.arbitrary

  val mmφptcf: Function[
    MomentMorphism,
    Function[PreThingsCollection, PreThingsCollection]
  ] = mmΦptcf.φ

  val ptcc2φtpic: Function[
    Collection2[PreThingsCollection],
    PreInteractionsCollection
  ] = ptcc2Τpic.τ

  // Foundational `given`s using `import`ed foundational delegates are defined

  import su.{mc, mfa}

  given Sets[Collection] = cs

  given Category[Morphism] = mc

  given ActingUponFunction[Morphism] = mfa

  // `Time` related foundational `given`s are defined

  import su.{mm, mmΦst}

  import mm.{ma, mo}

  given Arbitrary[Moment] = ma

  given Ordered[Moment] = mo

  // `Universe` related foundational `given`s are defined

  given Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] = mmΦst

  // `PreThings` related foundational `given`s are defined

  given mmΦptcc2f: Functor[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> Collection2[PreThingsCollection]
  ] = functionValuedFunctor2[
    Collection,
    [_, _] =>> MomentMorphism,
    [_] =>> PreThingsCollection
  ]

  // `PreThings` related foundational members
  // using `PreThings` related foundational `given`s are defined

  val mmφptcc2f: Function[
    MomentMorphism,
    Function[
      Collection2[PreThingsCollection],
      Collection2[PreThingsCollection]
    ]
  ] = mmΦptcc2f.φ

  // Composed `PreThings` related foundational members
  // using `PreThings` related foundational members
  // using `PreThings` related foundational `given`s are defined

  val picφptc: Function[PreInteractionsCollection, PreThingsCollection] =
    pic =>
      for {
        pi <- pic
      } yield {
        composition2 apply pi
      }

  val ptcφpic: Function[PreThingsCollection, PreInteractionsCollection] =
    ptc =>
      for {
        pt <- ptc
      } yield {
        decomposition2 apply pt
      }

  val mmφpicf: Function[
    MomentMorphism,
    Function[PreInteractionsCollection, PreInteractionsCollection]
  ] = mm => ptcφpic `o` mmφptcf(mm) `o` picφptc

  // laws

  import specification.{Law}

  import implementation.{composedFunctor}

  trait PreThingsFunctorCompositionLaws[L[_]: Law]:

    given stΦptcf: Functor[Morphism, Function, [_] =>> PreThingsCollection]

    val composition2: L[
      Functor[
        [_, _] =>> MomentMorphism,
        Function,
        [_] =>> PreThingsCollection
      ]
    ] = {
      mmΦptcf
    } `=` {
      composedFunctor[
        [_, _] =>> MomentMorphism,
        Morphism,
        Function,
        [_] =>> State,
        [_] =>> PreThingsCollection
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

    val noPreThingFromNothing: MomentMorphism => L[PreThingsCollection] =
      mm =>
        import cs.{collection0}
        {
          mmφptcf(mm)(collection0)
        } `=` {
          collection0
        }

    val unionOfSingletonPreInteractions
        : Collection2[PreThingsCollection] => L[PreInteractionsCollection] =
      tptc =>
        import cs.{tuple2, collection1, collection2, union}
        tuple2(tptc) match
          case (lptc, rptc) =>
            {
              union {
                for {
                  lpt <- lptc
                  rpt <- rptc
                } yield {
                  ptcc2φtpic(collection2(collection1(lpt), collection1(rpt)))
                }
              }
            } `=` {
              ptcc2φtpic(tptc)
            }

    val naturePreservingPreInteraction: MomentMorphism => L[Boolean] =
      mm =>
        {
          { mmφpicf(mm) `o` ptcc2φtpic } `<=` {
            ptcc2φtpic `o` mmφptcc2f(mm)
          }
        }
          `=` {
            true
          }

  trait PreThingsPlacesLaws[L[_]: Law]:

    val orderPreserving
        : PreThingsCollection => PreThingsCollection => L[Boolean] =
      lptc =>
        rptc =>
          import su.{svt}
          import svt.{`<=`}
          {
            lptc `<=` rptc `=` true
          } `=>` {
            ptcφs(lptc) `<=` ptcφs(rptc) `=` true
          }

    val supremumOfAllSingletonPlaces: PreThingsCollection => L[State] =
      ptc =>
        import cs.{collection1}
        import su.{ss}
        {
          ss {
            for {
              pt <- ptc
            } yield ptcφs(collection1(pt))
          }
        } `=` {
          ptcφs(ptc)
        }

    val immobileAfter
        : MomentMorphism => L[Function[PreThingsCollection, State]] =
      mm =>
        import su.{mmφst}
        {
          ptcφs `o` mmφptcf(mm)
        } `=` {
          mmφst(mm) `a` ptcφs
        }

    val immobileOnInterval: MomentMorphism => PreThingsCollection => L[State] =
      case (bm, em) =>
        import cs.{Interval, interval, all}
        import su.{mmφst}
        val mi: Interval[Moment] = interval apply ((bm, em))
        ptc =>
          all apply {
            for {
              m <- mi
            } yield {
              {
                (ptcφs `o` mmφptcf((bm, m)))(ptc)
              } `=` {
                (mmφst((bm, m)) `a` ptcφs)(ptc)
              }
            }
          }
