package timehybrids.specification

import types.{Composition}

import utilities.set.{emptySet, singleton, choices, U, all}

import utilities.composition.{compose, composeAll, decomposeAll}

import specification.{
  Ordered,
  VirtualTopology,
  Category,
  Functor,
  Action,
  FunctorTransformation,
  ActorTransformation
}

import implementation.{functionCategory}

trait PreThings[Morphism[_, _], Moment, Place, PreObject]:

  given placeUniverse: Universe[Moment, Place, Morphism]

  val placeUniverseVal = placeUniverse

  import placeUniverseVal.{morphismCategory}

  import placeUniverseVal.morphismCategoryVal.{Transition}

  import placeUniverseVal.{morphismFunctionAction}

  import placeUniverseVal.momentTimeVal.{MomentInterval}

  import placeUniverseVal.{
    momentIntervalToPlaceTransitionFunction,
    placeTransitionToInitialPlaceTransitionSetFunction,
    placeTransitionToSubPlaceTransitionSetFunction
  }

  import placeUniverseVal.{placeVirtualTopology}

  val placeVirtualTopologyVal = placeVirtualTopology

  import placeVirtualTopologyVal.{V}

  type PreThing = Composition[PreObject]

  type PreInteraction = Set[PreThing]

  given preThingSetFunctor: Functor[Morphism, Function, [_] =>> Set[PreThing]]

  val preThingSetFunctorVal = preThingSetFunctor

  import preThingSetFunctorVal.{φ}

  val preThingSetSetToPreInteractionSetFunctorTransformation: FunctorTransformation[
    Morphism,
    Function,
    [_] =>> Set[Set[PreThing]],
    [_] =>> Set[PreInteraction]
  ]

  import preThingSetSetToPreInteractionSetFunctorTransformation.{φτ}

  val preThingSetToPlaceActorTransformation: ActorTransformation[
    Morphism,
    Function,
    [_] =>> Set[PreThing],
    [_] =>> Place
  ]

  import preThingSetToPlaceActorTransformation.{ατ, isNaturalFor}

  given preInteractionSetFunctor: Functor[
    Morphism,
    Function,
    [_] =>> Set[PreInteraction]
  ] = new:
    def φ[Z, Y]: Function[
      Morphism[Z, Y],
      Function[Set[PreInteraction], Set[PreInteraction]]
    ] = pt =>
      val preInteractionToPreThingSetFunction
          : Function[Set[PreInteraction], Set[PreThing]] = composeAll
      val preThingSetToPreInteractionFunction
          : Function[Set[PreThing], Set[PreInteraction]] = decomposeAll
      preThingSetToPreInteractionFunction o
        preThingSetFunctor.φ(pt) o
        preInteractionToPreThingSetFunction

  val preThingSetSetFunctor: Functor[
    Morphism,
    Function,
    [_] =>> Set[Set[PreThing]]
  ] =
    new:
      def φ[Z, Y]: Function[
        Morphism[Z, Y],
        Function[Set[Set[PreThing]], Set[Set[PreThing]]]
      ] =
        zμy =>
          ptss =>
            for {
              pts <- ptss
            } yield {
              preThingSetFunctor.φ(zμy)(pts)
            }

  extension (placeTransitionFunctor: Functor[Morphism, Morphism, [_] =>> Place])
    def isMovementAfterPlaceTransition[Z, Y](
        placeTransition: Transition[Place]
    ): Boolean =

      {
        ατ o preThingSetFunctor.φ(placeTransition)
      } == {
        placeTransitionFunctor.φ(placeTransition) a ατ
      }

      placeTransitionFunctor isNaturalFor placeTransition

  val isImmobileAfterPlaceTransition: Transition[Place] => Boolean =
    val identityPlaceTransition: Transition[Place] = morphismCategory.ι
    val constantIdentityPlaceTransitionFunctor =
      new Functor[Morphism, Morphism, [_] =>> Place]:
        def φ[Z, Y] = _ => identityPlaceTransition
    constantIdentityPlaceTransitionFunctor isMovementAfterPlaceTransition _

  val isImmobileAtPlaceTransitionInitialPlaceTransitionBased
      : Transition[Place] => Boolean =
    placeTransition =>
      all {
        for {
          initialPlaceTransition <-
            placeTransitionToInitialPlaceTransitionSetFunction(
              placeTransition
            )
        } yield {
          isImmobileAfterPlaceTransition(initialPlaceTransition)
        }
      }

  val isImmobileAtPlaceTransitionSubPlaceTransitionBased
      : Transition[Place] => Boolean =
    placeTransition =>
      all {
        for {
          subPlaceTransition <- placeTransitionToSubPlaceTransitionSetFunction(
            placeTransition
          )
        } yield {
          isImmobileAfterPlaceTransition(subPlaceTransition)
        }
      }

  extension (placeTransitionFunctor: Functor[Morphism, Morphism, [_] =>> Place])
    def isMovementAfterMomentInterval[Z, Y](
        momentInterval: MomentInterval
    ): Boolean =
      placeTransitionFunctor isMovementAfterPlaceTransition
        momentIntervalToPlaceTransitionFunction(momentInterval)

  val immobileAfterTimeInterval: MomentInterval => Boolean =
    isImmobileAfterPlaceTransition o momentIntervalToPlaceTransitionFunction

  val immobileAtTimeIntervalInitialPlaceTransitionBased
      : MomentInterval => Boolean =
    isImmobileAtPlaceTransitionInitialPlaceTransitionBased o
      momentIntervalToPlaceTransitionFunction

  val immobileAtTimeIntervalSubPlaceTransitionBased: MomentInterval => Boolean =
    isImmobileAtPlaceTransitionSubPlaceTransitionBased o
      momentIntervalToPlaceTransitionFunction

  // laws

  import specification.{Law}

  import implementation.{setOrdered}

  trait PreThingsLaws[L[_]: Law]:

    val selfPreInteraction: PreInteraction => L[Set[PreThing]] =
      preInteraction =>
        require(preInteraction.size == 1)
        val preThingSet: Set[PreThing] = preInteraction
        val preThing = compose(preThingSet)
        preInteraction `=` singleton(preThing)

    val noPreThingsFromNoPreThings: Transition[Place] => L[Set[PreThing]] =
      placeTransition =>
        {
          φ(placeTransition)(emptySet)
        } `=` {
          emptySet
        }

    val unionOfSingletonPreInteractions
        : Set[Set[PreThing]] => L[Set[PreInteraction]] =
      preThingSetSet =>
        {
          U {
            for {
              preThingSet <- choices(preThingSetSet)
            } yield {
              φτ {
                for {
                  preThing <- preThingSet
                } yield {
                  singleton(preThing)
                }
              }
            }
          }
        } `=` {
          φτ(preThingSetSet)
        }

    import specification.{FunctorTransformationLawsFor}

    val transformationLaws =
      FunctorTransformationLawsFor(
        preThingSetSetToPreInteractionSetFunctorTransformation
      )

    import transformationLaws.{orderedNatural}

    val preInteractionNaturePreserving
        : Set[PreInteraction] => Transition[Place] => L[Boolean] =
      preInteractionSet =>

        given Ordered[Function[Set[PreInteraction], Set[PreInteraction]]] with
          extension (
              leftPreInteractionSetFunction: Function[
                Set[PreInteraction],
                Set[PreInteraction]
              ]
          )
            def <(
                rightPreInteractionSetFunction: Function[
                  Set[PreInteraction],
                  Set[PreInteraction]
                ]
            ): Boolean =
              leftPreInteractionSetFunction(preInteractionSet)
                < rightPreInteractionSetFunction(preInteractionSet)

        placeTransition =>
          import preThingSetSetFunctor.φ
          {
            {
              φτ o φ(placeTransition)
            } <= {
              φ(placeTransition) o φτ
            }
          } `=` {
            true
          }

        orderedNatural apply placeTransition

    val orderPreserving: Set[PreThing] => Set[PreThing] => L[Boolean] =
      leftPreThingSet =>
        rightPreThingSet =>
          {
            leftPreThingSet <= rightPreThingSet `=` true
          } `=>` {
            ατ(leftPreThingSet) <= ατ(rightPreThingSet) `=` true
          }

    val supremumOfAllSingletonPlaces: Set[PreThing] => L[Place] =
      preThingSet =>
        {
          V {
            for {
              preThing <- preThingSet
            } yield ατ(singleton(preThing))
          }
        } `=` {
          ατ(preThingSet)
        }
