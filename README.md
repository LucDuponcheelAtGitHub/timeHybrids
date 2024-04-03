# Time Hybrids a new Generic Theory of Reality

This document is about the
[*Time Hybrids*](https://novapublishers.com/shop/time-hybrids-a-new-generic-theory-of-reality/) book of
[*Fred Van Oystaeyen*](https://en.wikipedia.org/wiki/Fred_Van_Oystaeyen).

Fred is an outstanding mathematician in such fields as *noncommutative algebraic geometry*, *virtual topology*
and *functor geometry*.

I dedicate this document to Fred.

Fred told me about an old Chinese legend about a poet who chiselled a small poem on a small stone with a small chisel
and threw that stone into the sea, hoping that, one day, someone would read the poem.

Fred compared his book with that stone.

I hope that this document will, somehow, increase the number of people that read his book.

The goal of this document is to illustrate the book programmatically.

## How to read this document

This document does not present its content in a linear way.

Sections contain hyperlinks to sections that can be read by need.

## Introduction

### Warning

This document, especially its introduction, is a highly opinionated one.

Many sentences start with "I think of", emphasizing the fact that I may be wrong.

I hope that my opinion about the content of the book is more or less compatible with the opinion of Fred.

### Specification and Implementations

A *specification* consists of *declarations* of *features*.

Declarations come with *laws*.

Together, features and laws are called *requirements*.

A specification may also consist of *definitions* that are defined in terms of declarations and definitions.

*Implementations* of a specification consist of *definitions* of declared features.

Definitions come with *proofs* of laws.

Note that a specification with *less* requirements allows for *more* implementations, so it is important to keep
requirements as minimal as possible.

In a way you can think of a specification as a *description*.

I think of a description as an implementation that is an *informal specification*.

For example, the picture on
[The Treachery of Images](https://en.wikipedia.org/wiki/The_Treachery_of_Images)
is a description (pun intended!) of a well known description of a pipe.

Frankly, if you would never have seen a pipe before, would you be able to make a pipe *only* from this description?

### Generic Theory

I think of a *generic theory* as a *theory consisting of a specification of theories*, where
*theories are implementations of*, a *framework theory of theories* or *template theory of theories*, where
*theories fit into as implementations*.

### Generic Theory of Reality

The *Time Hybrids* book describes a *Generic Theory of Reality*.
Hence I think of a generic theory of reality as a *theory consisting of a specification of theories of reality*, where
*theories of reality are implementations of*, a *framework theory of theories of reality* or
*template theory of theories of reality*, where *theories of reality fit into as implementations*.

### Quantum Theory and Relativity Theory

It is generally agreed upon that *quantum theory* and *relativity theory* are two *fundamental theories of reality*.

A *unifying theory* for quantum theory and relativity theory is yet to be agreed upon.

I think of the generic theory of reality of the book as a *partially unifying theory* for quantum theory and
relativity theory, concentrating on *common requirements*.

The book states that various phenomena of quantum theory and relativity theory, which, until now, have been considered
counter-intuitive, can, within its generic theory of reality, be viewed in a more intuitive way.

The book also provides some basic insight in *how* to fit quantum theory and relativity theory into its partially
unifying generic theory of reality.

### Future Research

I think that, providing *all details* on how to fit quantum theory and relativity theory into the partially
unifying generic theory of reality, and comparing it with *observed reality*, is a challenging topic for future
research.

For example, would it not be nice to be able to show that, somehow, the theory of Stephen Wolfram and Co, explained in
[how to think computationally about ai the universe and everything](https://www.ted.com/talks/stephen_wolfram_how_to_think_computationally_about_ai_the_universe_and_everything?language=en)
can be seen as an implementation of the specification of Fred Van Oystaeyen?

If, one day, all those details would be provided, and if they all correspond to observed reality so far, then that would
be an important scientific achievement.

If it is not possible to provide all those details, or if they do not all correspond to observed reality so far, then
that would be useful as well.

The reason *why* may provide valuable insight into the generic theory of Fred and/or the theory of Stephen and/or
quantum theory and/or relativity theory.

### Reality and Compositionality

I think of *compositionality* as an important aspect of reality.

*Compositionality* is about *components*.

Components can, starting from various *atomic components*, be *composed* to *composed components* in various ways.

Compositionality comes in different flavors.

- *functionality-like*
- *information-like*

### Category Theory

I think of *category theory* as a *generic theory of mathematics*, a
*theory consisting of a specification of theories of mathematics*, a *framework theory of theories of mathematics* or
*template theory of theories of mathematics*.

Of course, a far as the book is concerned, the *theories of mathematics* involved are theories that are relevant for
modeling reality.

### Why Category Theory

Category theory is an *abstract* theory.

You could argue that using an abstract theory results in a steep learning curve.

But here is the thing: *abstraction* is about *simplification* and *simplicity is the ultimate sophistication*.

When I was a first year mathematics student, professor Gevers, my professor Analytic Mechanics, said

- I am going to proceed slower in order to have covered more material at the end of the year.

Those were wise words.

Once you have built a solid foundation, agreed, first slowing down learning, you can, gradually, start accelerating,
eventually speeding up learning.

### Compositionality and Category Theory

Category theory is about *collections of objects* and *sets of morphisms*.

Every morphism is one from a *source object* to a *target object*.

Category theory is compositional.

More precisely, compositionality of category theory is *sequential compositionality of morphisms*.

As is done in most documents about category theory, this document focuses on morphisms instead of on objects.

In general, it focusses on *pointfree concepts* rather than focussing on *pointful concepts*.

More precisely, it focusses on *pointfree, closed components* rather than on *pointful, open components*.

### Programmatic notation

In this document I explain the content of the book using *programmatic notation*.

The programmatic notation defines a *domain specific language*, a.k.a. as *DSL*, that is a *library* that is written in
the [`Scala`](https://www.scala-lang.org/) *programming language*.

The DSL is a concise *formal language* that is syntactically verified by `Scala`'s powerful type system.

Both the DSL library and the `Scala` language are briefly explained by need.

The `Scala` code is not really idiomatic code.

It is code that, i.m.h.o, is very suitable for learning purposes.

The DSL is both one the *domain of the book*, *reality*, being part of *physisc*, and one for the its *foundations*,
being part of *mathematics*.

For the domain of the book, it uses notation that, more or less, corresponds to the one used in physics.

For its foundations, it uses notation that, more or less, corresponds to the one used in mathematics.

In what follows `Scala` is not explicitly mentioned any more.

### Programmatic notation for specifications

*Specifications* are, programmatically, denoted as

- *value classes*,

or,

- *type classes*,
- *unary type constructor classes*,
- *binary type constructor classes*,
- ... .

More precisely, they are denoted as

- `trait`*s without corresponding parameter* for value classes,

and,

- `trait`*s with corresponding parameter* for all other classes.

### Programmatic notation for implementations

*Implementations* are, programmatically, denoted as

- `val`*s* for value classes,

and,

- `given`*s* for all other classes.

### Implicitly denoting homogeneous sets

Types `Z` *implicitly* denote *homogeneous sets* and *values* `z` of a type `Z` *implicitly* denote
*elements of homogeneous sets*.

### Explicitly denoting homogeneous sets

The unary type constructor `Set`, constructs types `Set[Z]` for every type `Z`.

Types `Set[Z]` *explicitly* denote *homogeneous sets* and *values* `zs` of a type `Set[Z]` *explicitly* denote
*elements of homogeneous sets*.

### Programmatic naming conventions

The programmatic notation uses names with letters of the
[Greek alphabet](https://en.wikipedia.org/wiki/Greek_alphabet)
corresponding to first letters of words, according to the
[Romanization of Greek alphabet](https://en.wikipedia.org/wiki/Romanization_of_Greek).

### Analogy between Physics and Computer Science

Lets consider, for a moment, the analogy between *physics* and *computer science*, in as far as the goal of physics is
to understand what *reality* is all about, and the goal of computer science is to understand what *software* is all
about.

Agreed, understanding reality is more difficult than understanding software: software is something we invented
ourselves, while reality is, afaik, still one great mystery.

It is now generally agreed upon that computer science benefits from category theory as a partially unifying theory of
software theories into which both *effectfree software theory* and *effectful software theory* fit as implementations.

Maybe, one day, it will be generally agreed upon that physics benefits from category theory as a partially unifying
theory of physics theories into which *quantum theory* and *relativity theory* fit as implementations.

By the way, in a previous life, I was one of the researchers working on category theory as a partially unifying theory
of effectfree software theory and effectful software theory using *monads*. I did most of my work as a
"late at night hobby". I also worked two years at the University of Utrecht together with Erik Meijer, Graham Hutton and
Doaitse Swierstra. I mainly wrote and teached courses, but I also did some research. Our team studied monads as
*computations* that are *computed*. Computations are generalizations of *expression* that are *evaluated*. Computations
and expressions are *operational* components. Moreover, monads and expressions are *open*, pointful components.

Nowadays I study *morphism* as *programs* that are *run*. Morphisms are generalizations of *functions* that are
*applied*. Morphism and functions are *denotational* components. Moreover, morphism and functions are *closed*,
pointfree components. You can look at my talk about *Program Description based Programming* on
[flatMap, 8-9 May 2019, Oslo Norway](https://2019.flatmap.no/). I am refactoring the work presented in Olso, upgrading
from `Scala 2` to `Scala 3`, and changing the paradigm name to *Program Specification based Programming* for reasons
explained above.

In other words, I have been doing and I still am doing foundational work on software theories that is similar to the
foundational work Fred is doing on physics theories.

### Time Hybrids Domain

In what follows I will quote some sentences of a paper of Fred related to a presentation he gave in Almeria on his book.

*We introduce a generic model for space-time where time is just a totally ordered set ordering the states of the*
*universe at moments where over (not in) each state we define potentials, or pre-things, which are going to evolve via*
*correspondences between momentary potentials to existing things. Existing takes time and observing takes more time.*

*We will look at time as a totally ordered set T.*

*The universe U is constructed as a set of states at moments, where moments are the elements of T, say U(t) at t in T.*

*Over a state U(t) at moment t, we consider a set S(t) of pre-things or t-potentials and we look at the set PS(t) of*
*all subsets of S(t), including the empty subset and S(t) itself.* 

*For the geometry we will use a very general pre-geometry based upon non-commutative topologies in the states of the*
*universe, made dynamic via morphisms between states forming strings over time intervals.*

*We made the U(t) sets but a geometry on it need not be a geometry given by sets of points. The non-commutative*
*topology X(t) is given by a non-commutative lattice structure L(t) on the set U(t) where there is a partial order < on*
*the set and two operations, ∧ and ∨, being meet and join, generalizing the intersection and union of topological sets*
*of points.*

*We can define a “place map”, p(t): PS(t) --> U(t) where some A(t) of PS(t) is taken to an element pA(t) of the*
*non-commutative lattice L(t) giving the topology of U(t) such that p(t) respects the partial orders on PS(t) and U(t).*

This document is work in progress.

For now, let's concentrate on the following concepts:

- time moments,
- universe places,
- pre-things.

Let's explain some notation:

- Time moments are denoted as *t*.
- Universe places at time moments *t* are denoted as *U(t)*.
- The collections of all sets of pre-things at time moments *t* are denoted as *PS(t)*.
  - mathematically this collection is not a set
  - programmatically this collection is a constructive set
- Sets of pre-things at time moments *t* are denoted as *A(t)*.
- The place map *p(t)* maps sets of pre-things *A(t)* to places *pA(t)*.
- The *non-commutative lattice* on *U(t)* is denoted as *L(t)*.
- The *non-commutative topology* defined by *L(t)* is denoted as *X(t)*. 

In what follows we gradually denote the concepts involved using programmatic notation.

## Time Hybrids Domain: Specification

### Time

Back to [Universe](#universe) 

```scala
trait Time[Moment]:

  given momentOrdered: Ordered[Moment]
  
  val momentOrderedVal = momentOrdered

  type MomentInterval = momentOrderedVal.Interval

  type MomentIntervalUtilities = momentOrderedVal.IntervalUtilities

  val momentIntervalUtilities: MomentIntervalUtilities
```

`Time` is a type class for parameter `Moment`.

The requirements for `Moment` to be a `Time` type are

- `Moment` is an `Ordered` type (cfr. `given momentOrdered`).

`Ordered` is explained in [Ordered](#ordered).

Time moments are also simply called moments.

Back to [Universe](#universe) 

### Universe

Back to [PreThings](#prethings) 

```scala
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

  given placeTransitionToMomentIntervalOneToOne
      : OneToOne[
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
            ) o momentIntervalToPlaceTransitionFunction(rightMomentInterval)
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
```

`Universe` is a type class for parameter `Place`.

`Universe` has a foundational parameter `Morphism` that is required to be a `Category` binary type constructor
(cfr. `given morphismCategory`).

`Category` is explained in [Category](#category).

`Universe` requires morphisms to act upon functions (cfr. `given morphismFunctionAction`).

`Action` is explained in [Action](#action).

`Universe` has a domain parameter `Moment` that is required to be a `Time` type (cfr. `given momentTime`).

`Time` is explained in [Time](#time).

The requirements for `Place` to be a `Universe` type are

- `Place` is a `VirtualTopology` type (cfr. `given placeVirtualTopology`),
- place transitions are one to one with moment intervals (cfr `given placeTransitionToMomentIntervalOneToOne`).

The places of the virtual topology of the universe are dymanic.

On the one hand, their transitions are driven by moment intervals.

On the other hand, it even makes sense to even go one step further by considering moment intervals as being implicitly
defined by place transitions, hence the one to one.

`VirtualTopology` is explained in [VirtualTopology](#virtualtopology).

`OneToOne` is explained in [OneToOne](#onetoone).

Back to [PreThings](#prethings)

### PreThings

```scala
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

import implementation.{setOrdered, functionCategory}

trait PreThings[Moment, Place, PreObject, Morphism[_, _]]:

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

  val preThingSetToPlaceActorTransformation: ActorTransformation[
    Morphism,
    Function,
    [_] =>> Set[PreThing],
    [_] =>> Place
  ]

  import preThingSetToPlaceActorTransformation.{ατ, isNaturalFor}

  given preThingSetFunctor: Functor[Morphism, Function, [_] =>> Set[PreThing]]

  val preThingSetFunctorVal = preThingSetFunctor

  import preThingSetFunctorVal.{φ}

  val preThingSetSetToPreInteractionSetFunctorTransformation:
    FunctorTransformation[
    Morphism,
    Function,
    [_] =>> Set[Set[PreThing]],
    [_] =>> Set[PreInteraction]
  ]

  import preThingSetSetToPreInteractionSetFunctorTransformation.{φτ}

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

  trait PreThingsLaws[L[_]: Law]:

    val selfPreInteraction: PreInteraction => L[Set[PreThing]] =
      preInteraction =>
        require(preInteraction.size == 1)
        val preThingSet: Set[PreThing] = preInteraction
        val preThing = compose(preThingSet)
        preInteraction `=` singleton(preThing)

    val noPreThingsFromNoPreThingsPlaceTransitionBased: Transition[Place] => L[Set[PreThing]] =
      placeTransition =>
        {
          φ(placeTransition)(emptySet)
        } `=` {
          emptySet
        }

    val noPreThingsFromNoPreThingsMomentIntervalBased: MomentInterval => L[Set[PreThing]] =
      noPreThingsFromNoPreThingsPlaceTransitionBased `o` momentIntervalToPlaceTransitionFunction

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
      FunctorTransformationLawsFor(preThingSetSetToPreInteractionSetFunctorTransformation)

    import transformationLaws.{orderedNatural}

    val preInteractionNaturePreservingPlaceTransitionBased
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

    val preInteractionNaturePreservingMomentIntervalBased
        : Set[PreInteraction] => MomentInterval => L[Boolean] =
      preInteractionSet =>
        momentInterval =>
          preInteractionNaturePreservingPlaceTransitionBased(preInteractionSet)(
            momentIntervalToPlaceTransitionFunction(momentInterval)
          )

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
```

`PreThings` is a type class for parameter `PreObject`.

`functionCategory` is explained in [functionCategory](#functioncategory)

`PreThings` has a foundational parameter `Morphism`.

`PreThings` has a domain parameter `Moment`.

`PreThings` has a domain parameter `Place` that is required to be a `Universe[Moment, Place, Morphism]` type
(cfr. `given placeUniverse`).

`Universe` is explained in [Universe](#universe).

`PreThings` uses `type` `Composition` to define `type` `PreThing` and `type` `PreInteraction` in terms of `PreObject`.

`type` `Composition` is explained in [type Composition](#compositiontype).

`compose`, `composeAll` and  `decomposeAl` are explained in [CompositionUtilities](#compositionUtilities).

The requirements for `PreObject` to be a `PreThings` type are

- `PreObject` is a `Functor[Morphism, Function, [_] =>> Set[PreThing]]`
   type (cfr. `given preThingSetFunctor`),
- `PreObject` has a `FunctorTransformation[Morphism, Function, [_] =>> Set[Set[PreThing]], [_] =>> Set[PreInteraction]]`
   type instance (cfr. `preThingSetSetToPreInteractionSetFunctorTransformation`),
- `PreObject` has an `ActorTransformation[Morphism, Function, [_] =>> Set[PreThing], [_] =>> Place]`
  type instance (cfr. `preThingSetToPlaceActorTransformation`).

`Functor` is explained in [Functor](#functor).

`FunctorTransformation` is explained in [FunctorTransformation](#functortransformation).

`ActorTransformation` is explained in [ActorTransformation](#association).

`isMovementAfterPlaceTransition` can be defined as a `Functor[Morphism, Morphism, [_] =>> Place]`, together with

- `isImmobileAfterPlaceTransition`
- `isImmobileAtPlaceTransitionInitialPlaceTransitionBased`
- `isImmobileAtPlaceTransitionSubPlaceTransitionBased`

and, corresponding `TimeInterval` related

- `isMovementAfterMomentInterval`
- `immobileAfterTimeInterval`
- `immobileAtTimeIntervalInitialPlaceTransitionBased`
- `immobileAtTimeIntervalSubPlaceTransitionBased`

`setOrdered` is explained in [setOrdered](#setordered)

Let's concentrate on the laws.

`selfPreInteraction` refers to the following, slightly adapted, excerpt from the book
(it is taken for granted in the paper).

*In fact ,the difference between pre-things and pre-interactions is one of language only, we may just as well call a*
*pre-thing A(t) a pre-interaction i(A(t),A(t))*.

`noPreThingsFromNoPreThingsPlaceTransitionBased` refers to the following, slightly adapted, excerpt from the paper.

*Moreover the correspondence acting on the empty set is always the empty set; thus no pre-things arise as the result of*
*a correspondence of the empty set! No pre-thing comes from nothing!*

`unionOfSingletonPreInteractions` refers to the following excerpt from the paper, where [1] refers to the book.

*The pre-interaction between A(t) and B(t) in S(t) is written as I(A,B)(t), in [1] I put I(A,B)(t) equal to*
*v{ i(a(t),b(t)) for a(t) in A(t),b(t) in B(t) }.*

`preInteractionNaturePreservingPlaceTransitionBased` refers to the following excerpt from the paper (I changed < to <=).

*However there is then a logical assumption, namely that s(t,t’)(I(A,B)(t)) <= I(A,B)(t’) for t<t’, meaning that the*
*correspondences s(t,t') do not change the nature of the later realisation as an interaction. Hence the s(t,t’) respect*
*the dichotomy between pre-interactions and other potentials we will call pre-objects, both together are pre-things.*

This is. i.m.h.o., the most fundamental law of pre-things.

It does not only state that the *s(t, t')* respect the dichotomy, but also that, at *t'* there may be
pre-interations that are not of the form *s(t,t’)(I(A,B)(t))*. 

`orderPreserving` refers to the following excerpt from the paper (already mentioned in the introduction).

*We can define a “place map”, p(t): PS(t)--->U(t) where some A(t) is taken to an element pA(t) of the nc-lattice L(t)*
*giving the topology of U(t) such that p(t) respects the (inclusion) partial orders on PS(t) and U(t).*

`supremumOfAllSingletonPlaces` refers to the following excerpt from the paper, where [2] refers to the 
"Virtual topology and functor geometry book" of Fred

*In [2], I took pA(t)=V{ p({a(t)}), a(t) in A(t) } – we then say p is basic - which is harmless and seems logical for*
*the notion of “place” yet we do not use that here.*

`Law` is explained in [Law](#law).

## Mathematical Foundations: Specifications

### Law

Back to [Ordered](#ordered)

Back to [NcMeet](#ncmeet)

Back to [JoinComplete](#joincomplete)

Back to [Join](#join)

Back to [Category](#category)

Back to [Composition](#composition)

Back to [Functor](#functor)

Back to [PreThings](#prethings)

```scala
package specification

trait Law[L[_]]:

  // declared

  extension [Z, Y](lly: L[Y]) def `=>`(rlz: L[Z]): L[Z]

  extension [Z](l: Z) def `=`(r: Z): L[Z]

  extension [Z](ll: L[Z]) def `&`(rl: L[Z]): L[Z]

  extension [Z](ll: L[Z]) def `|`(rl: L[Z]): L[Z]

  def all[Z]: Function[Set[L[Z]], L[Z]]
```

`Law` is a unary type constructor class for parameter `L`.

Laws are *conditional* *equational* laws with *conjunction*, *disjunction*, and *universal quantification*.

Back to [PreThings](#prethings)

Back to [Functor](#functor)

Back to [Composition](#composition)

Back to [Category](#category)

Back to [Join](#join)

Back to [JoinComplete](#joincomplete)

Back to [NcMeet](#ncmeet)

Back to [Ordered](#ordered)

### OneToOne

Back to [Universe](#universe)

```scala
package specification

trait OneToOne[Z, Y] extends Isomorphism[Function, Z, Y]:

  val fromAll: Function[Set[Z], Set[Y]] =
    zs =>
      for {
        z <- zs
      } yield {
        from(z)
      }

  val toAll: Function[Set[Y], Set[Z]] =
    ys =>
      for {
        y <- ys
      } yield {
        to(y)
      }
```

`Isomorphism` is explained in [Isomorphism](#isomorphism).

Back to [Universe](#universe)

### SetUtilities

Back to [JoinComplete](#joincomplete)

```scala
package utilities.set

def emptySet[Z]: Set[Z] = Set.empty

def singleton[Z]: Z => Set[Z] = z => emptySet + z

def tuple2ToSet[Z]: Tuple2[Z, Z] => Set[Z] =
  (z0, z1) => singleton(z0) union singleton(z1)

def choices[Z]: Set[Set[Z]] => Set[Set[Z]] =
  zss =>
    val zsl: List[Set[Z]] = zss.toList
    zsl match
      case Nil =>
        singleton(emptySet)
      case zs :: zsl =>
        for {
          z <- zs
          zs <- choices(zsl.toSet).toList
        } yield {
          zs + z
        }

def U[Z]: Set[Set[Z]] => Set[Z] =
  zss =>
    for {
      zs <- zss
      z <- zs
    } yield {
      z
    }

def all : Set[Boolean] => Boolean =
  _.foldRight(true)(_ && _)
```

Most utilities are straightforward.

Maybe `choices` may need some explanation.

Here is an example:

```scala
scala> choices(Set(Set(1, 2), Set(3, 4, 5), Set(6, 7))).foreach{println}
Set(7, 3, 2)
Set(7, 5, 1)
Set(6, 4, 2)
Set(7, 3, 1)
Set(6, 4, 1)
Set(7, 5, 2)
Set(6, 3, 2)
Set(6, 3, 1)
Set(7, 4, 1)
Set(6, 5, 1)
Set(7, 4, 2)
Set(6, 5, 2)
```

Back to [JoinComplete](#joincomplete)

### CompositionType

Back to [PreThings](#prethings)

```scala
package types

import specification.{Limit}

type Composition[Z] = Limit[[O] =>> CompositionEnum[Z, O]]
```

`Composition` is a limit of `CompositionEnum`.

`Limit` is explained in [Limit](#limit).

`CompositionEnum` is explained in [CompositionEnum](#Compositionenum).

`Limit` is a fixed-point recursively fills the "hole" in `CompositionEnum`.

**Warning**

This is work in progress.

Composition an important part of the mathematical foundation of the book of Fred.

There is more to writesay about it than I do in this document.

I declare and define concepts programmatically "by need".

Back to [PreThings](#prethings)

### CompositionUtilities

Back to [PreThings](#prethings)

```scala
package utilities.composition

import types.{CompositionEnum, Composition}

import utilities.set.{U}

import specification.{OneToOne, Limit, limit}

import implementation.{functionCategory, compositionEnumFunctionFunctor}

def compose[Z]: Set[Composition[Z]] => Composition[Z] =
  zfcs => Limit apply CompositionEnum.Composed(zfcs)

def decompose[Z]: Composition[Z] => Set[Composition[Z]] =
  limit apply {
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
```

`compositionEnumFunctionFunctor` is explained in [compositionEnumFunctionFunctor](#compositionEnumFunctionFunctor).

Back to [PreThings](#prethings)

### Ordered

Back to [Time](#time)

Back to [NcMeet](#ncmeet)

Back to [VirtualTopology](#virtualtopology)

Back to [Join](#join)

```scala
package specification

trait Ordered[Z]:

  // declared

  extension (lz: Z) def <(rz: Z): Boolean

  // defined

  extension (lz: Z) def <=(rz: Z): Boolean = lz < rz || lz == rz

  // nested `trait`s

  trait Interval extends Set[Z]

  trait IntervalUtilities:

    extension (b: Z) def to(e: Z): Interval

    def begin: Function[Interval, Z]

    def end: Function[Interval, Z]

    val initialIntervals: Function[Interval, Set[Interval]] =
      i =>
        val b = begin(i)
        for {
          e <- i
          if (b <= e && e <= end(i))
        } yield {
          b to e
        }

    val subIntervals: Function[Interval, Set[Interval]] =
      i =>
        for {
          b <- i
          e <- i
          if (begin(i) <= b &&
            b <= e &&
            e <= end(i))
        } yield {
          b to e
        }

    // laws

    trait IntervalUtilitiesLaws[L[_]: Law]:

      val beginToEnd: Interval => L[Interval] = i =>
        {
          i
        } `=` {
          begin(i) to end(i)
        }

  // laws

  trait OrderedLaws[L[_]: Law]:

    val reflexive: Z => L[Boolean] = z =>
      {
        z <= z
      } `=` {
        true
      }

    val antiSymmetric: Z => Z => L[Boolean] = lz =>
      rz =>
        {
          lz <= rz `=` true `&` rz <= lz `=` true
        } `=>` {
          lz == rz `=` true
        }

    val transitive: Z => Z => Z => L[Boolean] = lz =>
      mz =>
        rz =>
          {
            lz <= mz `=` true `&` mz <= rz `=` true
          } `=>` {
            lz <= rz `=` true
          }

  trait TotallyOrderedLaws[L[_]: Law]:

    val stronglyConnected: Z => Z => L[Boolean] = lz =>
      rz =>
        {
          lz <= rz `|` rz <= lz
        } `=` {
          true
        }
```

`Ordered` is a type class for parameter `Z`.

See, for example, [Partially ordered sets](https://en.wikipedia.org/wiki/Partially_ordered_set).

`Law` is explained in [Law](#law).

`Interval` is a type synonym for `Set[Z]`.

`IntervalUtilities` comes with its own features and `IntervalUtilitiesLaws`.

Back to [Join](#join)

Back to [VirtualTopology](#virtualtopology)

Back to [NcMeet](#ncmeet)

Back to [Time](#time)

### VirtualTopology

Back to [Universe](#universe)

```scala
package specification

trait VirtualTopology[Z] extends Ordered[Z], JoinComplete[Z], NcMeet[Z]
```

`Ordered` is explained in [Ordered](#ordered)

`JoinComplete` is explained in [JoinComplete](#joincomplete)

`NcMeet` is explained in [NcMeet](#ncmeet)

**Warning**

This is work in progress.

Virtual topology is the most important part of the mathematical foundation of the book of Fred.

I declare and define concepts programmatically "by need".

Until now, for the time moments, universe places, and pre-things concepts and their laws, I only need `Ordered` and
`JoinComplete`.

Although I do not need `NcMeet` yet, I add it anyway to emphasize the relationship with traditional topologies.

There is even more: the non-idempotency of the `NcMeet` operator is the most important reason why virtual topology is
more general, hence more capable from a modeling point of view, than traditional pointless topologies.

*Expect this to change a lot.*

Back to [Universe](#universe)

### JoinComplete

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait JoinComplete[Z] extends Join[Z]:

  // declared

  val V: Function[Set[Z], Z]

  // defined

  import utilities.set.{tuple2ToSet}

  extension (lz: Z) def ∨(rz: Z): Z = V(tuple2ToSet(lz, rz))

  trait SupremumLaws[L[_]: Law]:

    val law: Law[L] = summon[Law[L]]

    import law.{all}

    val smallestGreaterThanAll: Z => Set[Z] => L[Boolean] =
      az =>
        zs =>
          {
            all {
              for {
                z <- zs
              } yield z <= V(zs) `=` true
            }
          } `&` {
            all {
              for {
                z <- zs
              } yield z <= az `=` true
            }
          } `=>` {
            V(zs) <= az `=` true
          }
```

`JoinComplete` is a type class for parameter `Z`.

`Join` is explained in [Join](#join).

`tuple2ToSet` is explained in [SetUtilities](#setutilities).

See, for example, [Infimum and JoinComplete](https://en.wikipedia.org/wiki/Infimum_and_supremum).

`Law` is explained in [Law](#law)

Back to [VirtualTopology](#virtualtopology)

### NcMeet

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait NcMeet[Z] extends Ordered[Z]:

  // declared

  extension (lz: Z) def ∧(rz: Z): Z

  trait NcMeetLaws[L[_]: Law]:

    val greatestSmallerThanBoth: Z => Z => Z => L[Boolean] =
      az =>
        lz =>
          rt =>
            {
              ((lz ∧ rt) <= lz) `=` true `&` ((lz ∧ rt) <= rt) `=` true
            } `&` {
              (az <= lz) `=` true `&` (az <= rt) `=` true
            } `=>` {
              az <= (lz ∧ rt) `=` true
            }
```

`Ordered` is explained in [Ordered](#ordered).

See, for example, [Join and Meet](https://en.wikipedia.org/wiki/Join_and_meet).

`Law` is explained in [Law](#law)

Back to [VirtualTopology](#virtualtopology)

### Join

Back to [JoinComplete](#joincomplete)

```scala
package specification

trait Join[Z] extends Ordered[Z]:

  // declared

  extension (lz: Z) def ∨(rz: Z): Z

  trait JoinLaws[L[_]: Law]:

    val smallestGreaterThanBoth: Z => Z => Z => L[Boolean] =
      az =>
        lz =>
          rz =>
            {
              (lz <= (lz ∨ rz)) `=` true `&` (rz <= (lz ∨ rz)) `=` true
            } `&` {
              (lz <= az) `=` true `&` (rz <= az) `=` true
            } `=>` {
              (lz ∨ rz) <= az `=` true
            }
```

`Ordered` is explained in [Ordered](#ordered).

See, for example, [Join and Meet](https://en.wikipedia.org/wiki/Join_and_meet).

`Law` is explained in [Law](#law)

Back to [JoinComplete](#joincomplete)

### CompositionEnum

Back to [type Composition](#compositiontype)

```scala
package types

enum CompositionEnum[Z, O]:
  case Atomic[Z, O](z: Z) extends CompositionEnum[Z, O]
  case Composed[Z, O](ls: Set[O]) extends CompositionEnum[Z, O]
```

`CompositionEnum` is an example of information-like compositionality.

Parameter `O` denotes a "hole" in the `CompositionEnum` information.

Back to [type Composition](#compositiontype)

### Category

Back to [Universe](#universe)

```scala
package specification

import scala.collection.immutable.Seq

trait Category[Morphism[_, _]]
    extends Composition[Morphism],
      Identity[Morphism]:

  // defined

  def composeAll[Z]: Function[Seq[Transition[Z]], Transition[Z]] =
    zτs => zτs composeAllWith ι

  // laws

  trait CategoryLaws[L[_]: Law]:

    def leftIdentity[Z, Y]: Morphism[Z, Y] => L[Morphism[Z, Y]] = zμy =>
      {
        ι o zμy
      } `=` {
        zμy
      }

    def rightIdentity[Z, Y]: Morphism[Z, Y] => L[Morphism[Z, Y]] =
      zμy =>
        {
          zμy o ι
        } `=` {
          zμy
        }
```

`Category` is a binary type constructor class for parameter `Morphism`.

Sequences of transitions can all be composed. 

`Composition` is explained in [Composition](#composition) 

`Identity` is explained in [Identity](#identity)

See, for example, [Category](https://en.wikipedia.org/wiki/Category).

`Law` is explained in [Law](#law).

Back to [Universe](#universe)

### Action

Back to [Universe](#universe)

```scala
package specification

import scala.collection.immutable.Seq

trait Action[ActorMorphism[_, _]: Category, Morphism[_, _]: Category]:

  // declared

  def actionFunctor[Z]: Functor[ActorMorphism, Function, [Y] =>> Morphism[Z, Y]]

  // defined

  extension [Z, Y, X](yμx: ActorMorphism[Y, X])
    def a(zμy: Morphism[Z, Y]): Morphism[Z, X] =
      actionFunctor.φ(yμx)(zμy)

  type ActorTransition = [Z] =>> ActorMorphism[Z, Z]

  extension [Z, Y, X](τys: Seq[ActorTransition[Y]])
    def allActUpon(zμy: Morphism[Z, Y]): Morphism[Z, Y] =
      τys.foldRight(zμy)(_ a _)
```

`Action` is a binary type constructor class for parameter `ActorMorphism` that is required to be a `Category` binary
type constructor.

`Action` has a parameter `Morphism` that is required to be a `Category` binary type constructor.

`Category` is explained in [Category](#category).

`actionFunctor[Z]`s come with functions `φ[Y, X]` from actor morphisms of type `ActorMorphism[Y, X]` to morphism
functions of type `Function[Morphism[Z, X], Morphism[Z, Y]]`.

In particular they come with functions `φ[Z, Z]` from actor transitions of type `ActorTransition[Y]` to morphism
functions of type `Function[Morphism[Z, Y], Morphism[Z, Y]]`.

Sequences of actor transitions can act upon a morphism. 

`Functor` is explained in [Functor](#functor).

Back to [Universe](#universe)

### Composition

Back to [Category](#category)

```scala
trait Composition[Morphism[_, _]]:

  // declared

  extension [Z, Y, X](yμx: Morphism[Y, X])
    def o(zμy: Morphism[Z, Y]): Morphism[Z, X] 

  // defined

  type Transition = [Z] =>> Morphism[Z, Z]

  type Transition = [Z] =>> Morphism[Z, Z]

  extension [Z, Y, X](zτs: Seq[Transition[Z]])
    def composeAllWith(zτ: Transition[Z]): Transition[Z] =
      zτs.foldRight(zτ)(_ o _)   

  // laws

  trait CompositionLaws[L[_]: Law]:

    def associativity[Z, Y, X, W]
        : Morphism[Z, Y] => Morphism[Y, X] => Morphism[X, W] => L[
          Morphism[Z, W]
        ] =
      zμy =>
        yμx =>
          xμw =>
            {
              (xμw o yμx) o zμy
            } `=` {
              xμw o (yμx o zμy)
            }
```

`Composition` is a binary type constructor class for parameter `Morphism`.

Morphisms of type `Morphism[Z, Y]` denote morphisms from homogeneous sets `Z` to homogeneous sets `Y`.

Morphisms of type `Morphism[Z, Y]` change the type of elements if `Z` and `Y` are different types. 

Transitions of type `Transition[Z]`, do not change the type of elements.

Sequences of transitions can all be composed with a transition.

`Composition` is an example of functionality-like compositionality.

`Law` is explained in [Law](#law).

Back to [Category](#category)

### Identity

Back to [Category](#category)

```scala
package specification

trait Identity[Morphism[_, _]]:

  // declared

  def ι[Z]: Morphism[Z, Z]
```

`Identity` is a binary type constructor class for parameter `Morphism`.

Back to [Category](#category)

### Isomorphism

Back to [OneToOne](#onetoone)

```scala
package specification

trait Isomorphism[Morphism[_, _]: Category, Z, Y]:

  val from: Morphism[Z, Y]

  val to: Morphism[Y, Z]

  // laws

  trait IsomorphismLaws[L[_]: Law]:

    val category = summon[Category[Morphism]]

    import category.{ι}

    val fromLaw: L[Morphism[Z, Z]] = {
      to o from
    } `=` {
      ι
    }

    val toLaw: L[Morphism[Y, Y]] = {
      from o to
    } `=` {
      ι
    }
```

Back to [OneToOne](#onetoone)

### Functor

Back to [Action](#action)

Back to [PreThings](#prethings)

```scala
package specification

trait Functor[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    Morphed[_]
]:

  // declared

  def φ[Z, Y]: Function[
    FromMorphism[Z, Y],
    ToMorphism[Morphed[Z], Morphed[Y]]
  ]

  // defined

  type FromTransition = [Z] =>> FromMorphism[Z, Z]

  type ToTransition = [Z] =>> ToMorphism[Z, Z]

  // laws

  trait FunctorLaws[L[_]: Law]:

    def identity[Z]: L[ToMorphism[Morphed[Z], Morphed[Z]]] =
      val fm = summon[Category[FromMorphism]]
      val tm = summon[Category[ToMorphism]]
      φ(fm.ι) `=` tm.ι

    def composition[Z, Y, X]: FromMorphism[Z, Y] => FromMorphism[Y, X] => L[
      ToMorphism[Morphed[Z], Morphed[X]]
    ] =
      fzμy =>
        fyμx =>
          {
            φ(fyμx o fzμy)
          } `=` {
            φ(fyμx) o φ(fzμy)
          }
```

`Functor` is a unary type constructor class for parameter `Morphed`.

`Functor` has two parameters `FromMorphism` and `ToMorphism` that are required to be `Category` binary type
constructors.

`Category` is explained in [Category](#category).

Types `Morphed[Z]`, corresponding to types `Z`, come with functions `φ[Z, Y]` from morphisms of type
`FromMorphism[Z, Y]` to morphisms of type `ToMorphism[Morphed[Z], Morphed[Y]]`.

In particular they come with functions `φ[Z, Z]` from transitions of type `FromTransition[Z]` to transitions of type
`ToTransition[Morphed[Z]`.

See, for example, [Functor](https://en.wikipedia.org/wiki/Functor).

`Law` is explained in [Law](#law).

Back to [PreThings](#prethings)

Back to [Action](#action)

### FunctorTransformation

Back to [PreThings](#prethings)

```scala
package specification

trait FunctorTransformation[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    FromMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      FromMorphed
    ],
    ToMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      ToMorphed
    ]
]:

  // declared

  def φτ[Z]: ToMorphism[FromMorphed[Z], ToMorphed[Z]]

// laws

class FunctorTransformationLawsFor[
    FromMorphism[_, _]: Category,
    ToMorphism[_, _]: Category,
    FromMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      FromMorphed
    ],
    ToMorphed[_]: [_[_]] =>> Functor[
      FromMorphism,
      ToMorphism,
      ToMorphed
    ],
    L[_]: Law
](
    t: FunctorTransformation[
      FromMorphism,
      ToMorphism,
      FromMorphed,
      ToMorphed
    ]
):

  val f_fmΦtm = summon[Functor[FromMorphism, ToMorphism, FromMorphed]]

  val t_fmΦtm = summon[Functor[FromMorphism, ToMorphism, ToMorphed]]

  import t.{φτ}

  def orderedNatural[
      Z,
      Y: [_] =>> Ordered[
        ToMorphism[FromMorphed[Z], ToMorphed[Y]]
      ]
  ]: FromMorphism[Z, Y] => L[Boolean] =
    fzμy =>
      {
        {
          φτ o f_fmΦtm.φ(fzμy)
        } <= {
          t_fmΦtm.φ(fzμy) o φτ
        }
      } `=` {
        true
      }
```

`FunctorTransformation` is a value class.

`orderedNatural` is a kind of natural transformation law with `==` replaced by `<=`.

Back to [PreThings](#prethings)

### ActorTransformation

Back to [PreThings](#prethings)

```scala
package specification

trait ActorTransformation[
    ActorMorphism[_, _]: Category: [_[_, _]] =>> Action[
      ActorMorphism,
      UponMorphism
    ],
    UponMorphism[_, _]: Category,
    UponMorphed[_]: [_[_]] =>> Functor[
      ActorMorphism,
      UponMorphism,
      UponMorphed
    ],
    ActorMorphed[_]: [_[_]] =>> Functor[
      ActorMorphism,
      ActorMorphism,
      ActorMorphed
    ]
]:

  // declared

  def ατ[Z]: UponMorphism[UponMorphed[Z], ActorMorphed[Z]]

  // defined

  val amΦum = summon[Functor[ActorMorphism, UponMorphism, UponMorphed]]

  extension [Z, Y](
      amΦam: Functor[ActorMorphism, ActorMorphism, ActorMorphed]
  )
    def isNaturalFor(zμy: ActorMorphism[Z, Y]): Boolean = {
      ατ o amΦum.φ(zμy)
    } == {
      amΦam.φ(zμy) a ατ
    }
```

`ActorTransformation` is a value class.

`isNaturalFor` is defined using a kind of natural transformation law with a composition `o` replaced by an action `a`.

Back to [PreThings](#prethings)

### Limit

Back to [type Composition](#compositiontype)

```scala
package specification

import implementation.{functionCategory}

case class Limit[
    Morphed[_]: [_[_]] =>> Functor[Function, Function, Morphed]
](clc: Morphed[Limit[Morphed]]) 

def limit[
    Morphed[_]: [_[_]] =>> Functor[Function, Function, Morphed],
    Z
]: (Morphed[Z] => Z) => (Limit[Morphed] => Z) =
  val fΦf = summon[Functor[Function, Function, Morphed]]
  zcφz => cl => (zcφz o fΦf.φ(limit apply zcφz))(cl.clc)
```

`functionCategory` is explained in 

`Limit` denotes general information-like recursion.

`limit` denotes general functionality-like recursion.

Back to [type Composition](#compositiontype)

## Mathematical Foundations Domain: Implementations

### functionCategory

Back to [PreThings](#prethings)

Back to [Limit](#limit)

```scala
package implementation

import specification.{Category}

given functionCategory: Category[Function] with

  type Morphism = [Z, Y] =>> Function[Z, Y]

  extension [Z, Y, X](yμx: Morphism[Y, X])
    def o(zμy: Morphism[Z, Y]): Morphism[Z, X] = z => yμx(zμy(z))

  def ι[Z]: Morphism[Z, Z] = z => z
```

Back to [Limit](#limit)

Back to [PreThings](#prethings)

### setOrdered

Back to [PreThings](#prethings)

```scala
package implementation

import specification.{Ordered}

given setOrdered[Z]: Ordered[Set[Z]] with

  extension (lzs: Set[Z]) def `=`(rzs: Set[Z]): Boolean = lzs == rzs

  extension (lzs: Set[Z]) def <(rzs: Set[Z]): Boolean = 
    (lzs subsetOf rzs) && (lzs != rzs)
```

Back to [PreThings](#prethings)

### compositionEnumFunctionFunctor

Back to [CompositionUtilities](#compositionutilities)

```scala
package implementation

import types.{CompositionEnum}

import specification.{Functor}

given compositionEnumFunctionFunctor[X]
    : Functor[Function, Function, [O] =>> CompositionEnum[X, O]] with
  def φ[Z, Y]: Function[Z, Y] => Function[
    CompositionEnum[X, Z],
    CompositionEnum[X, Y]
  ] = zφy =>
    case CompositionEnum.Atomic[X, Z](x) =>
      CompositionEnum.Atomic[X, Y](x)
    case CompositionEnum.Composed[X, Z](zs) =>
      CompositionEnum.Composed[X, Y](
        for {
          z <- zs
        } yield {
          zφy(z)
        }
      )
```

Back to [CompositionUtilities](#compositionutilities)
