package timehybrids.specification

import specification.{
  Ordered,
  VirtualTopology,
  OneToOne,
  Category,
  Action,
  Functor
}

trait Universe[Moment, Place, Morphism[_, _]]:

  given morphismCategory: Category[Morphism]

  val morphismCategoryVal = morphismCategory

  import morphismCategoryVal.{Transition}

  given morphismFunctionAction: Action[Morphism, Function]

  given momentTime: Time[Moment]

  val momentTimeVal = momentTime

  import momentTimeVal.{MomentInterval, momentIntervalUtilities}

  import momentIntervalUtilities.{initialIntervals, subIntervals}

  given placeVirtualTopology: VirtualTopology[Place]

  given placeTransitionToMomentIntervalOneToOne: OneToOne[
    Transition[Place],
    MomentInterval
  ]

  val momentIntervalToPlaceTransitionFunction
      : Function[MomentInterval, Transition[Place]] =
    placeTransitionToMomentIntervalOneToOne.`to`

  val momentIntervalFromPlaceTransitionFunction
      : Function[Transition[Place], MomentInterval] =
    placeTransitionToMomentIntervalOneToOne.`from`

  val placeTransitionToInitialPlaceTransitionSetFunction
      : Function[Transition[Place], Set[Transition[Place]]] =
    placeTransition =>
      for {
        momentInterval <- initialIntervals(
          momentIntervalFromPlaceTransitionFunction(placeTransition)
        )
      } yield {
        momentIntervalToPlaceTransitionFunction(momentInterval)
      }

  val placeTransitionToSubPlaceTransitionSetFunction
      : Function[Transition[Place], Set[Transition[Place]]] =
    placeTransition =>
      for {
        momentInterval <- subIntervals(
          momentIntervalFromPlaceTransitionFunction(placeTransition)
        )
      } yield {
        momentIntervalToPlaceTransitionFunction(momentInterval)
      }

  // not used

  given placeTransitionCategory: Category[[_, _] =>> Transition[Place]] =
    new:
      extension [Z, Y, X](leftPlaceTransition: Transition[Place])
        def o(rightPlaceTransition: Transition[Place]): Transition[Place] =
          morphismCategory.o(leftPlaceTransition)(rightPlaceTransition)
      def ι[Z]: Transition[Place] = morphismCategory.ι

  given momentIntervalCategory: Category[[_, _] =>> MomentInterval] =
    new:
      extension [Z, Y, X](leftMomentInterval: MomentInterval)
        def o(rightMomentInterval: MomentInterval): MomentInterval =
          momentIntervalFromPlaceTransitionFunction(
            momentIntervalToPlaceTransitionFunction(
              leftMomentInterval
            ) `o` momentIntervalToPlaceTransitionFunction(rightMomentInterval)
          )

      def ι[Z]: MomentInterval =
        momentIntervalFromPlaceTransitionFunction(morphismCategory.ι)

  given placeTransitionFunctor: Functor[
    [_, _] =>> MomentInterval,
    [_, _] =>> Transition[Place],
    [_] =>> Transition[Place]
  ] =
    new:
      def φ[Z, Y]: MomentInterval => Transition[Place] =
        momentIntervalToPlaceTransitionFunction

  given momentIntervalFunctor: Functor[
    [_, _] =>> Transition[Place],
    [_, _] =>> MomentInterval,
    [_] =>> MomentInterval
  ] =
    new:
      def φ[Z, Y]: Transition[Place] => MomentInterval =
        momentIntervalFromPlaceTransitionFunction
