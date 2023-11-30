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

## How to read this document

This document does not present its content in a linear way.

Sections contain hyperlinks to sections that can be read by need.

## Introduction

### Warning

This document, especially its introduction, is a highly opinionated one.

Many sentences start with "I tend to think of", emphasizing the fact that I may be wrong.

I hope that my opinion about the book is more or less compatible with the opinion of Fred.

### Specification and Implementations

A *specification* consists of *declarations* of *features*.

Declarations come with *laws*.

Together, features and laws are called *requirements*.

*Implementations* of a specification consist of *definitions* of declared features.

Definitions come with *proofs* of laws.

Note that a specification with *less* requirements allows for *more* implementations, so it is important to keep
requirements as minimal as possible.

In a way you can think of a specification as a *description*.

I tend to think of a description as an implementation that is an *informal specification*.

For example, the picture below is a description (pun intended!) of a well known description of a pipe.

<img src="https://github.com/LucDuponcheelAtGitHub/time-hibrids/blob/main/pipeDescription.png" width=50% height=50%>

Frankly, if you would never have seen a pipe before, would you be able to make a pipe *only* from this description?

### Generic Theory

I tend to think of a *generic theory* as a *theory consisting of specifications of theories*,
a *framework theory of theories* or *template theory of theories*, where *theories fit into as implementations*.

### Generic Theory of Reality

The *Time Hybrids* book describes a *Generic Theory of Reality*.

Hence I tend to think of a generic theory of reality as a *theory consisting of specifications of theories of reality*,
a *framework theory of theories of reality* or *template theory of theories of reality*, where
*theories of reality fit into as implementations*.

### Quantum Theory and Relativity Theory

It is generally agreed upon that *quantum theory* and *relativity theory* are two *fundamental theories of reality*.

A *unifying theory* for quantum theory and relativity theory is yet to be agreed upon.

I tend to think of the generic theory of reality of the book as a *partially unifying theory* for quantum theory and
relativity theory, concentrating on *common requirements*.

The book states that various phenomena of quantum theory and relativity theory, which, until now, have been considered
counter-intuitive, can, within its generic theory of reality, be viewed in a more intuitive way.

The book also provides some basic insight in *how* to fit quantum theory and relativity theory into its partially
unifying generic theory of reality.

### Future Research

I tend to think that, providing *all details* on how to fit quantum theory and relativity theory into the partially
unifying generic theory of reality, and comparing it with *observed reality*, is a challenging topic for future
research.

For example, would it not be nice to be able to show that, somehow, the theory of Stephen Wolfram and Co, explained in
[https://www.ted.com/talks/stephen_wolfram_how_to_think_computationally_about_ai_the_universe_and_everything?language=en]
can be seen as an implementation of the specification of Fred Van Oystaeyen?

If, one day, all those details would be provided, and if they all correspond to observed reality so far, then that would
be an important scientific achievement.

If it is not possible to provide all those details, or if they do not all correspond to observed reality so far, then
that would be useful as well.

The reason *why* may provide valuable insight into the generic theory of Fred and/or the theory of Stephen and/or
quantum theory and/or relativity theory.

### Reality and Compositionality

I tend to think of *compositionality* as an important aspect of reality.

*Compositionality* is about *components*.

Components can, starting from various *atomic components* be *composed* to *composed components* in various ways.

### Category Theory

I tend to think of *category theory* as a *generic theory of mathematics*, a
*theory consisting of specifications of theories of mathematics*, a *framework theory of theories of mathematics* or
*template theory of theories of mathematics*, where *theories of mathematics fit into as implementations*.

Of course, a far as the book is concerned, the *theories of mathematics* involved are theories that are relevant for
modeling reality.

### Why Category Theory

Category theory is an *abstract* theory.

You could argue that using an abstract theory results in a steep learning curve.

But here is the thing: *abstraction* is about *simplification* and *simplicity is the ultimate sophistication*.

When I was a first year mathematics student, professor Gevers, my professor mechanics, said

- I am going to proceed slower in order to have covered more material at the end of the year.

Those were wise words.

Once you have built a solid foundation, agreed, first slowing down learning, you can, gradually, start accelerating,
eventually speeding up learning.

### Compositionality and Category Theory

Category theory is about *collections of objects* and *sets of morphisms*.

Collections are not necessarily sets.

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

For the domain of the book, it uses verbose notation that, more or less, corresponds to the one used in physics.

For its foundations, it uses verbose notation that, more or less, corresponds to the one used in mathematics.

In what follows `Scala` is not explicitly mentioned any more.

#### Programmatic notation for specifications

*Specifications* are, programmatically, denoted as

- *value classes*,
and,
- *type classes*,
- *unary type constructor classes*,
- *binary type constructor classes*,
- ... .

More precisely, they are denoted as

- `trait`*s without corresponding parameter* for value classes,
and,
- `trait`*s with corresponding parameter* for all other classes.

#### Programmatic notation for implementations

*Implementations* are, programmatically, denoted as

- `val`*s* for value classes,
and,
- `given`*s* for all other classes.

#### Programmatic naming conventions

The programmatic notation uses names that are abbreviations using first letters of (sequences of (parts of)) words.

The programmatic notation uses names with letters of the
[Greek alphabet](https://en.wikipedia.org/wiki/Greek_alphabet)
corresponding to first letters of words, according to the
[Romanization of Greek alphabet](https://en.wikipedia.org/wiki/Romanization_of_Greek).

The programmatic notation uses *symbolic operator names between backticks*.

### Analogy between Physics and Computer Science

Lets consider, for a moment, the analogy between *physics* and *computer science*, in as far as the goal of physics is
to understand what *reality* is all about, and the goal of computer science is to understand what *software* is all
about.

Agreed, understanding reality is so much more difficult than understanding software: software is something we invented
ourselves, while reality is, afaik, still one great mystery.

It is now generally agreed upon that computer science benefits from category theory as a partially unifying theory of
software theories into which *effectfree software theory* and *effectful software theory* fit as implementations.

Maybe, one day, it will be generally agreed upon that physics benefits from category theory as a partially unifying
theory of physics theories into which *quantum theory* and *relativity theory* fit as implementations.

By the way, in a previous life, I was one of the researchers working on category theory as a partially unifying theory
of effectfree software theory and effectful software theory using *monads*. I did most of my work as a
"late at night hobby". I also worked two years at the University of Utrecht together with Erik Meijer, Graham Hutton and
Doaitse Swierstra. I mainly wrote and teached courses, but I also did some research. Our group looked at monads as
*computations* that are *computed*. Computations are generalizations of *expression* that are *evaluated*. Computations
and expressions are *operational* components. Moreover, monads and expressions are *open*, pointful components.

Nowadays I look at *morphism* as *programs* that are run. Morphisms are generalizations of *functions*. Morphism and
functions are *denotational* components. Moreover, morphism and functions are *closed*, pointfree components. You can
look at my talk about *Program Description based Programming* on 
[flatMap, 8-9 May 2019, Oslo Norway](https://2019.flatmap.no/). I am refactoring the work presented in Olso, upgrading
from `Scala2` to `Scala3`, and changing the paradigm name to *Program Specification based Programming* for reasons
explained above.

In other words, I have been doig and I still am doing foundational work on software theories that is similar to the
foundational work Fred is doing on physics theories.

## Time Hybrids Domain: Specifications

Recently I attended a lecture of Fred on its book in Almeria. The lecture was accompanied with an informal paper.

Let's start with the first sentences of the abstract of the paper.

*We introduce a generic model for space-time where time is just a totally ordered sets ordering the states of the*
*universe at moments where over (not in) each state we define potentials or pre-things which are going to evolve via*
*correspondences between the momentary potentials to existing things. Existing takes time and observing takes more time.*

The following concepts are involved

- time moments,
- universe states,
- pre-things,
- space,
- things.

Moreover

- pre-things are momentary (they do not really exist),
- things exist (which takes time)
- observing takes more time.

This document is work in progress.

For now only time moments, universe states and pre-things are dealt with.

Let's continue with another sentence of the content of the paper.

*We can define a “place map”, p(t): PS(t)--->U(t) where some A(t) is taken to an element pA(t) of the nc-lattice L(t)*
*giving the topology of U(t) such that p(t) respects the (inclusion) partial orders on PS(t) and U(t).*

This sentence uses notation that needs some explanation.

- Time moments are denoted as *t*.
- The universe state at time moment *t* is denoted as *U(t)*.
- The collection of sets of pre-things at time moment *t* is denoted as *PS(t)*.
- Sets of pre-things are denoted as *A(t)*.
- The "place map" *p(t)* maps *A(t)* to *pA(t)*.
- The *non-commutative lattice* on *U(t)* is denoted as *L(t)*.

The non-commutative *L(t)* lattice defines a *virtual topology* on *U(t)*.

*p(t)* respects the non-commutative lattice order on *PS(t)* and the subset order on *U(t)*.

Note that the statements above can be seen as requirements.

In what follows we denote them using programmatic notation.

### Time

```scala
package timehybrids.specification

import specification.{Arbitrary, Ordered}

trait Time[Moment: Arbitrary: Ordered]:

// ...
```

`Time` is a type class for parameter `Moment`.

The requirements for `Moment` to be a `Time` type are

- `Moment` is an `Arbitrary` type,
- `Moment` is a `Ordered` type.

`Arbitrary` is fully explained in [Arbitrary](#arbitrary).

`Ordered` is fully explained in [Ordered](#ordered).

A types *implicitly* denotes a *sets* and a *value* of a type *implicitly* denotes an *element* of a sets.

Recall that we are building a DSL for reality and its foundations.

The requirements above allow us to, programmatically,

- write statements involving arbitrary time moments,
- state that one time moment is before another one.

Time moments are also simply called *moments*.

Recall that the code is not really the most idiomatic one, it is idiomatic for the DSL we are defining.

Instead of using the *"is-a" Object Oriented Programming* idiom we use the *"has-a" Functional Programming* idiom.

In other words, instead of using *inheritance* we use *delegation*.

Moreover, delegation is *implicit*.

*Delegates* need to be defined *explicitly* by `summon`*ing* them.

They can then be `import`*ed* by need and made available as `given`*s* by need.

Agreed, all this is somewhat verbose, but, the beauty of programmatic notation like this comes from the combination of
*compactness* and *conciseness* of `trait` *declarations* like

- `trait Time[Moment: Arbitrary: Ordered]`,

stating that, for `Moment` to be a `Time` type, `Moment` is required to be an `Arbitrary` type and an `Ordered` type.

```scala
  // ...

  val ma: Arbitrary[Moment] = summon[Arbitrary[Moment]]

  val mo: Ordered[Moment] = summon[Ordered[Moment]]

  // ...
```

`Time` related foundational delegates are defined.

```scala
  // ...

  val am: Moment = ma.arbitrary
```

`Time` related foundational members using members of `Time` related foundational delegates are defined.

You may wonder why similar members using `mo` are not defined.

This is because those members are `extension`s that are globally available.

### Universe

```scala
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

  // ...
```

Using the `type` definitions below you can read the `Universe` definition above as

```scala
trait Universe[
    Collection[_]: Sets,
    Morphism[_, _]: Category: ActingUponFunction,
    Moment: [_] =>> Time[
      Moment
    ],
    State: [_] =>> Functor[
      [_, _] =>> MomentMorphism,
      Morphism,
      [_] =>> State
    ]: [_] =>> VirtualTopology[
      Collection,
      State
    ]
]
```

`Universe` is a type class for parameter `State`.

`Universe` also has a foundational parameter `Collection` that is required to be a `Sets` unary type constructor.

`Sets` is fully explained in [Sets](#sets).

`Sets` *explicitly* denotes *the realm of all sets*.

Mathematically this is not a set. 

Programmatically it is a constructive set (recall that, programmatically, a set is implicitly denoted by a type).

`Universe` also has a foundational parameter `Morphism` that is required to be a `Category` binary type constructor and
a `ActingUponFunction` binary type constructor.

`Category` is fully explained in [Category](#category).

`ActingUponFunction` is fully explained in [ActingUponFunction](#actinguponfunction).

`Universe` also has a domain parameter `Moment` that is required to be a `Time` type.

Type `Tuple2[T, T]`, somewhat abusively, denotes an ordered set implicitly denoted by type `T`.

- A value `(l, r)`, somewhat abusively, denotes ``{ l `<=` r } `=` { true }``.

Using the `type` definitions below the requirements for `State` to be a `Universe` type are

- `State` is a `Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State]` type,
- `State` is a `VirtualTopology[Collection, State]` type.

`Functor` is fully explained in [Functor](#functor).

`VirtualTopology` is fully explained in [VirtualTopology](#virtualtopology).

```scala
  // ...

  val cs: Sets[Collection] = summon[Sets[Collection]]

  val mc: Category[Morphism] = summon[Category[Morphism]]

  val mfa: ActingUponFunction[Morphism] = summon[ActingUponFunction[Morphism]]

  // ...
```

Foundational delegates are defined.

```scala
  // ...

  val mm: Time[Moment] = summon[Time[Moment]]

  // ...
```

`Time` related domain delegates are defined.

```scala
  // ...

  type MomentMorphism = Tuple2[Moment, Moment]

  // ...
```

`Time` related domain types are defined.

```scala
  // ...

  val mmΦst: Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] =
    summon[Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State]]

  val svt: VirtualTopology[Collection, State] =
    summon[VirtualTopology[Collection, State]]

  // ...
```

`Universe` related foundational delegates are defined.

```scala
  // ...

  type StateMorphism = Morphism[State, State]

  // ...
```

`Universe` related domain types are defined.

```scala
  // ...

  val mmφst: Function[MomentMorphism, StateMorphism] = mmΦst.φ

  val ss: Function[Collection[State], State] = svt.sup

  // ...
```

`Universe` related foundational members using members of `Universe` related foundational delegates are defined.

### NestedComposition

```scala
package types

import specification.{Sets}

enum NestedComposition[Collection[_]: Sets, Z]:
  case Atom[Collection[_]: Sets, Z](z: Z)
      extends NestedComposition[Collection, Z]
  case Composition2[Collection[_]: Sets, Z](
      ncc: Collection[NestedComposition[Collection, Z]]
  ) extends NestedComposition[Collection, Z]

import NestedComposition.{Composition2}

def composition2[Collection[_]: Sets, Z]: Collection[
  NestedComposition[Collection, Z]
] => NestedComposition[Collection, Z] =
  Composition2.apply

def decomposition2[Collection[_]: Sets, Z]
    : NestedComposition[Collection, Z] => Collection[
      NestedComposition[Collection, Z]
    ] =

  val sets: Sets[Collection] = summon[Sets[Collection]]

  import sets.{collection2}

  nc => collection2 apply (nc, nc)
```

`NestedComposition` is an example of *recursive structural compositionality*.

Note that the `Collection[NestedComposition[Collection, Z]]` is a `Collection2[NestedComposition[Collection, Z]]`.

### PreThings

```scala
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

  // ...
```

Using the `type` definitions below you can read the `PreThings` definition above as

```scala
trait PreThings[
    Collection[_],
    Morphism[_, _],
    Moment,
    State: [_] =>> Universe[Collection, Morphism, Moment, State],
    PreObject: [_] =>> Arbitrary[
      PreInteractionsCollection
    ]: [_] =>> Functor[
      [_, _] =>> MomentMorphism,
      Function,
      [_] =>> PreThingsCollection
    ]: [_] =>> Transformation[
      [_, _] =>> MomentTransition,
      Function,
      [_] =>> Collection2[PreThingsCollection],
      [_] =>> PreInteractionsCollection
    ]: [_] =>> Function[
      PreThingsCollection,
      State
    ]
]:
```

`PreThings` is a type class for parameter `PreObject`.

`PreThings` also has a foundational parameter `Collection`.

`PreThings` also has a foundational parameter `Morphism`.

`PreThings` also has a domain parameter `Moment`.

`PreThings` also has a domain parameter `State` that is required to be a
`Universe[Collection, Morphism, Moment, State]` type.

Using the `type` definitions below the requirements for `PreObject` to be a `PreThings` type are

- `PreObject` is an `Arbitrary[PreInteractionsCollection]` type.

- `PreObject` is a `Functor[[_, _] =>> MomentMorphism, Function, [_] =>> PreThingsCollection]` type.

- `PreObject` is a
  `Transformation[`
  `[_, _] =>> MomentMorphism,`
  ` Function,`
  `[_] =>> Collection2[PreThingsCollection],`
  `[_] =>> PreInteractionsCollection]` type.

`Transformation` is fully explained in [Transformation](#transformation).

`functionValuedFunctor2`, is fully explained in [functionValuedFunctor2](#functionValuedFunctor2)

`orderedCategory`, is fully explained in [orderedCategory](#orderedCategory)

`functionCategory`, is fully explained in [functionCategory](#functionCategory)


```scala
  // ..

  val su: Universe[Collection, Morphism, Moment, State] =
    summon[Universe[Collection, Morphism, Moment, State]]

  // ...
```

`Universe` related domain delegates are defined.

```scala
  // ...

  import su.{cs}

  import cs.{Collection2}

  type PreThing = NestedComposition[Collection, PreObject]

  type PreThingsCollection = Collection[PreThing]

  type PreInteraction = Collection2[PreThing]

  type PreInteractionsCollection = Collection[PreInteraction]

  // ...
```

`PreThings` related domain types are defined.

```scala
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
  
  // ...
```

`PreThings` related foundational delegates are defined.

```scala
  // ...

  val apic: Collection[PreInteraction] = pica.arbitrary

  val mmφptcf: Function[
    MomentMorphism,
    Function[PreThingsCollection, PreThingsCollection]
  ] = mmΦptcf.φ

  val ptcc2φtpic: Function[
    Collection2[PreThingsCollection],
    PreInteractionsCollection
  ] = ptcc2Τpic.τ

  // ...
```

`PreThings` related foundational members using members of `PreThings` related foundational delegates are defined.

```scala
  import su.{mc, mfa}

  given Sets[Collection] = cs

  given Category[Morphism] = mc

  given ActingUponFunction[Morphism] = mfa
```

Foundational `given`s using `import`ed foundational delegates are defined.

```scala
  // ...

  import su.{mm, mmΦst}

  import mm.{ma, mo}

  given Arbitrary[Moment] = ma

  given Ordered[Moment] = mo

  // ...
```

`Time` related foundational `given`s are defined.

```scala
  given Functor[[_, _] =>> MomentMorphism, Morphism, [_] =>> State] = mmΦst

  // ...
```

`Universe` related foundational `given`s are defined.

```scala
  // ...

  given mmΦptcc2f: Functor[
    [_, _] =>> MomentMorphism,
    Function,
    [_] =>> Collection2[PreThingsCollection]
  ] = functionValuedFunctor2[
    Collection,
    [_, _] =>> MomentMorphism,
    [_] =>> PreThingsCollection
  ]

  // ...
```

`PreThings` related foundational `given`s are defined.

```scala
  // ...

  val mmφptcc2f: Function[
    MomentMorphism,
    Function[
      Collection2[PreThingsCollection],
      Collection2[PreThingsCollection]
    ]
  ] = mmΦptcc2f.φ

  // ...
```

`PreThings` related foundational members using `PreThings` related foundational `given`s are defined

```scala
  // ...

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

  // ...
```

Composed `PreThings` related foundational members using `PreThings` related foundational members using `PreThings`
related foundational `given`s are defined.

The laws of `PreThingsRealityLaws`, `PreThingsRealityLaws` are fully explained in
[PreThingsRealityLaws](#prethingslaws).

## Time Hybrids Domain: Laws

### PreThingsLaws

Back to [PreThings](#prethings)

```scala
  // ...

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

    // ...
```

This law refers to the following excerpts from the paper.

*Thus the total order of Time is just the total order of the states of the universe and we may think of U as a book*
*with pages U(t) indexed by elements of T.*

*The pages will be glued together by some maps f(t,t’) for t<t’ in T, where < is the order of T*

*We let s(t,t’) denote a map from PS(t) to PS(t’) which may be viewed as a correspondence from S(t) to S(t’), this*
*mathematical concept is just a map from subsets of S(t) to subsets of S(t’)*

Although it is nowhere mentioned in the book or paper, this optional law relating *f(t,t’)* and *s(t,t’)* looks natural
to me.

```scala
  // ...

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

    // ...
```

This law refers to the following, slightly adapted, excerpt from the book (it is taken for granted in the paper).

*In fact ,the difference between pre-things and pre-interactions is one of language only, we may just as well call a
pre-thing A(t) a pre-interaction i(A(t),A(t))*.

```scala
  // ...

    val noPreThingFromNothing: MomentMorphism => L[PreThingsCollection] =
      mm =>
        import cs.{collection0}
        {
          mmφptcf(mm)(collection0)
        } `=` {
          collection0
        }

  // ...
```

This law refers to the following, slightly adapted, excerpt from the paper.

*Moreover the correspondence acting on the empty set is always the empty set; thus no pre-things arise as the result of*
*a correspondence of the empty set! No pre-thing comes from nothing!*

```scala
  // ...

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

  // ...
```

This law refers to the following excerpt from the paper, where [1] refers to the book.

*The pre-interaction between A(t) and B(t) in S(t) is written as I(A,B)(t), in [1] I put I(A,B)(t) equal to*
*v{ i(a(t),b(t)) for a(t) in A(t),b(t) in B(t) }.*

```scala
  // ...

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

  // ...
```

This law refers to the following excerpt from the paper (I changed < to <=).

*However there is then a logical assumption, namely that s(t,t’)(I(A,B)(t)) <= I(A,B)(t’) for t<t’, meaning that the*
*correspondences s(t,t') do not change the nature of the later realisation as an interaction. Hence the s(t,t’) respect*
*the dichotomy between pre-interactions and other potentials we will call pre-objects, both together are pre-things.*

This is. i.m.h.o., really the most fundamental law of the realm of pre-things.

```scala
  // ...

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

  // ...
```

This law refers to the following excerpt from the paper (already mentioned in the introduction).

*We can define a “place map”, p(t): PS(t)--->U(t) where some A(t) is taken to an element pA(t) of the nc-lattice L(t)*
*giving the topology of U(t) such that p(t) respects the (inclusion) partial orders on PS(t) and U(t).*

```scala
  // ...

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

  // ...
```

This law refers to the following excerpt from the paper, where [2] refers to the 
"Virtual topology and functor geometry book" of Fred

*In [2], I took pA(t)=V{ p({a(t)}), a(t) in A(t) } – we then say p is basic - which is harmless and seems logical for*
*the notion of “place” yet we do not use that here.*

```scala
  // ...

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
```

This law refers to the following, slightly adapted, excerpt from the paper.

*A string of correspondences, over an interval I=[t,t’], starting with A(t) then yields places p(A(t”)) with t” in I.*
*If f(t,t”)p(A(t))=p(A(t”)) for all t” in I, then we say the pre-thing A is immobile on I.*

`composedFunctor`, is fully explained in [composedFunctor](#composedfunctor)

`functionTargetOrdered`, is fully explained in [functionCategory](#functionCategory)

`setOrdered`, is fully explained in [functionValuedFunctor2](#functionValuedFunctor2)

Back to [PreThings](#prethings)

## Mathematical Foundations Domain: Specifications

### Types

Back to [TripleLaws](#triplelaws)

Back to [CompositionNaturalTransformation](#compositionnaturaltransformation)

Back to [UnitNaturalTransformation](#unitnaturaltransformation)

```scala
package types

type `o` = [G[_], F[_]] =>> [T] =>> G[F[T]]

type U = [T] =>> T
```

`` `o` `` defines unary type constructor composition.

`U` defines the unary type constructor unit.

Back to [UnitNaturalTransformation](#unitnaturaltransformation)

Back to [CompositionNaturalTransformation](#compositionnaturaltransformation)

Back to [TripleLaws](#triplelaws)

### Arbitrary

Back to [Time](#time)

```scala
package specification

trait Arbitrary[T]:

  // ...
```

`Arbitrary` is a type class for parameter `T`.

```scala
  // ...

  // declared

  def arbitrary: T
```

`Arbitrary` features are declared:

`Arbitrary` does not come with laws.

Recall that we are building a DSL for reality and its foundations.

The features above allow us to, programmatically,

- write statements involving *arbitrary elements*.

Note that `arbitrary` is declared as a `def`.

Two `arbitrary` values are not necessarily equal.

Back to [Time](#time)

### Ordered

Back to [Time](#time)

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait Ordered[T] extends Equality[T]:

  // ...
```

`Ordered` is a type class for parameter `T`.

`Equality` is fully explained in [Equality](#equality).

```scala
  // ...

  // declared

  extension (lt: T) def `<`(rt: T): Boolean

  // ...
```

`Ordered` features are declared.

```scala
  // ...

  // defined

  extension (lt: T) def `<=`(rt: T): Boolean = lt `<` rt || lt `=` rt

  // ...
```

Extra `Ordered` features are defined.

Recall that we are building a DSL for reality and its foundations.

The features above allow us to, programmatically,

- state that *one element is less than another one*,
- state that *one element is less than or equal to another one*.

The laws of `Ordered`, `OrderedLaws` resp. `TotallyOrderedLaws` are fully explained in [OrderedLaws](#orderedlaws),
resp. [TotallyOrderedLaws](#totallyorderedlaws).

Back to [VirtualTopology](#virtualtopology)

Back to [Time](#time)

### Equality

Back to [Ordered](#ordered).

```scala
package specification

trait Equality[T]:

  // ...
```

`Equality` is a type class for parameter `T`.

```scala
  // ...

  // declared

  extension (lt: T) def `=`(rt: T): Boolean

  // ...
```

`Equality` features are declared.

Recall that we are building a DSL for reality and its foundations.

The features above allow us to, programmatically,

- state that *one element is equal to another one*.

The laws of `Equality`, `EqualityLaws` are fully explained in [EqualityLaws](#equalitylaws).

Back to [Ordered](#ordered).

### VirtualTopology

Back to [Universe](#universe)

```scala
package specification

trait VirtualTopology[Collection[_]: Sets, T]
    extends Ordered[T],
      Meet[T],
      Join[T],
      Supremum[Collection, T]
```

`VirtualTopology` is a type class for parameter `T`.

`Sets` is fully explained in [Sets](#sets).

`Ordered` is fully explained in [Ordered](#ordered).

`Meet` is fully explained in [Meet](#meet).

`Join` is fully explained in [Join](#join).

`Supremum` is fully explained in [Supremum](#supremum).

Back to [Universe](#universe)

### Meet

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait Meet[T: Ordered: Arbitrary]:

  // ...
```

`Meet` is a type class for parameter `T`.

```scala
  // ...

  // declared

  extension (l: T) def ∧(r: T): T

  // ...
```
`Meet` features are declared.

The laws of `Meet`, `MeetLaws` are fully explained in [MeetLaws](#meetlaws).

Back to [VirtualTopology](#virtualtopology)

### Join

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait Join[T: Ordered: Arbitrary]:

  // ...
```
`Join` is a type class for parameter `T`.

```scala
  // ...

  // declared

  extension (l: T) def ∨(r: T): T

  // ...
```

`Join` features are declared.

The laws of `Join`, `JoinLaws` are fully explained in [JoinLaws](#joinlaws).

Back to [VirtualTopology](#virtualtopology)

### Supremum

Back to [VirtualTopology](#virtualtopology)

```scala
package specification

trait Supremum[Collection[_]: Sets, T: Ordered: Arbitrary]:
```

`Supremum` is a type class for parameter `T`.

`Sets` is fully explained in [Sets](#sets).

```scala
  //

  val sup: Function[Collection[T], T]

  // ...
```

`Supremum` features are declared.

The laws of `Supremum`, `SupremumLaws` are fully explained in [SupremumLaws](#supremumlaws).

Back to [VirtualTopology](#virtualtopology)

### Sets

Back to [Universe](#universe)

Back to [VirtualTopology](#virtualtopology)

Back to [Supremum](#supremum)

```scala
package specification

trait Sets[Collection[_]] extends MonadPlus[Collection]:

  // ...
```

`Sets` is a unary type constructor class for parameter `Collection`.

`MonadPlus` is fully explained in [MonadPlus](#monadplus).

```scala
  // ...

  // types

  type Collection0 = [Z] =>> Collection[Z]

  type Collection1 = [Z] =>> Collection[Z]

  type Collection2 = [Z] =>> Collection[Z]

  type Interval = [Z] =>> Collection[Z]

  // ...
```

`Sets` related types are defined.

```scala
  // ...

  // declared

  extension [Z](lc: Collection[Z]) def `=s=`(rc: Collection[Z]): Boolean

  extension [Z](lc: Collection[Z]) def `<s<`(rc: Collection[Z]): Boolean

  def tuple2[Z]: Collection2[Z] => Tuple2[Z, Z]

  // declared related to Ordered (apply needed at use site)

  def interval[Z: Ordered]: Option[Tuple2[Z, Z]] => Collection[Z]

  // declared related to Law (apply needed at use site)

  def all[Z, L[_]: Law]: Collection[L[Z]] => L[Z]

  // ...  
```

`Sets` features are declared.

```scala
  // ...

  // defined

  def collection0[Z]: Collection0[Z] = ζ

  def collection1[Z]: Z => Collection1[Z] = ν

  extension [Z](lc: Collection[Z])
    def ∪(rc: Collection[Z]): Collection[Z] =
      lc `+` rc

  def collection2[Z]: Tuple2[Z, Z] => Collection2[Z] = (l, r) =>
    collection1(l) ∪ collection1(r)

  def union[Z]: Collection[Collection[Z]] => Collection[Z] = μ

  extension [Z](lc: Collection[Z])
    def `<=s<=`(rc: Collection[Z]): Boolean = lc `<s<` rc || lc `=s=` rc

  // ...  
```

`Sets` members are defined.

The laws of `Sets`, `SetLaws`, are fully explained in [SetLaws](#setlaws).

Back to [Supremum](#supremum)

Back to [VirtualTopology](#virtualtopology)

Back to [Universe](#universe)

### Functor

Back to [Universe](#universe)

Back to [ActingUpon](#actingupon)

Back to [Triple](#triple)

Back to [NaturalTransformation](#naturaltransformation)

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

```scala
package specification

trait Functor[FBTC[_, _]: Category, TBTC[_, _]: Category, UTC[_]]:
```

`Functor` is a unary type constructor class for parameter `UTC`.

`Functor` has two parameters `FBTC` and `TBTC` that are required to be `Category` binary type constructors.

`Category` is fully explained in [Category](#category).

```scala
  // ...

  // declared

  def φ[Z, Y]: Function[FBTC[Z, Y], TBTC[UTC[Z], UTC[Y]]]
```

`Functor` features are declared.

The laws of `Functor`, `FunctorLaws` are fully defined in [FunctorLaws](#functorlaws).

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

Back to [NaturalTransformation](#naturaltransformation)

Back to [Triple](#triple)

Back to [ActingUpon](#actingupon)

Back to [Universe](#universe)

### MonadPlus

Back to [Sets](#sets)

```scala
package specification

trait MonadPlus[UTC[_]] extends Monad[UTC], Plus[UTC]:

  // ...
```

`MonadPlus` is a unary type constructor class for `UTC`.

`Monad` is fully explained in [Monad](#monad).

`Plus` is fully explained in [Plus](#plus).

The laws of `MonadPlus`, `MonadPlusLaws`, are fully explained in
[MonadPlusLaws](#monadpluslaws).

Back to [Sets](#sets)

### Monad

Back to [MonadPlus](#monadplus)

```scala
package specification

trait Monad[UTC[_]] extends Triple[Function, UTC]:

  // ...
```

`Monad` is a unary type constructor class for `UTC`.

`Triple` is fully explained in [Triple](#triple).

```scala
  // ...

  // defined

  extension [Z, Y](utcz: UTC[Z])
    def map(zφy: Function[Z, Y]): UTC[Y] = φ(zφy)(utcz)

  extension [Z, Y](utcz: UTC[Z])
    def flatMap(zφutcy: Function[Z, UTC[Y]]): UTC[Y] = μ(utcz map zφutcy)

  // ...
```

`Monad` members are defined.

`map` and `flatMap` support *powerful *`for`*-iteration notation*.

<img src="https://github.com/LucDuponcheelAtGitHub/time-hibrids/blob/main/flatMap.png" width=50% height=50%>

Back to [MonadPlus](#monadplus)

### Plus

Back to [MonadPlus](#monadplus)

```scala
package specification

trait Plus[UTC[_]] extends UtcComposition[UTC], UtcUnit[UTC]:

  // ...
```

`Plus` is a unary type constructor class for `UTC`.

`UtcComposition` is fully explained in [UtcComposition](#utccomposition).

`UtcUnit` is fully explained in [UtcUnit](#utcunit).

The laws of `Plus`, `PlusLaws` are fully explained in [PlusLaws](#pluslaws).

Back to [MonadPlus](#monadplus)

### Triple

Back to [Monad](#monad)

```scala
package specification

trait Triple[BTC[_, _]: Category, UTC[_]]
    extends Functor[BTC, BTC, UTC],
      CompositionNaturalTransformation[BTC, UTC],
      UnitNaturalTransformation[BTC, UTC]:

  // ...
```

`Triple` is a unary type constructor class for `UTC`.

`Triple` has a parameter `BTC` that is required to be a `Category` binary type constructor.

`Category` is fully explained in [Category](#category).

`Functor` is fully explained in [Functor](#functor).

`CompositionNaturalTransformation` is fully explained in
[CompositionNaturalTransformation](#compositionnaturaltransformation).

`UnitNaturalTransformation` is fully explained in [UnitNaturalTransformation](#unitnaturaltransformation).

```scala
  // ...

  // delegates

  val c: Category[BTC] = summon[Category[BTC]]

  // ...
```

`Triple` delegates are defined.

The laws of `Triple`, `TripleLaws` are fully explained in [TripleLaws](#triplelaws).

Back to [Monad](#monad)

### UtcComposition

Back to [Plus](#plus)

```scala
package specification

trait UtcComposition[UTC[_]]:

  // ...
```

`UtcComposition` is a unary type constructor class for `UTC`.

```scala
  // ...

  // declared

  extension [Z](lutc: UTC[Z]) def `+`(rutc: UTC[Z]): UTC[Z]

  // ...
```

`UtcComposition` features are declared.

The laws of `UtcComposition`, `UtcCompositionLaws` are fully explained in [UtcCompositionLaws](#utccompositionlaws).

Back to [Plus](#plus)

### UtcUnit

Back to [Plus](#plus)

```scala
package specification

trait UtcUnit[UTC[_]]:

  // ...
```

`UtcUnit` is a unary type constructor class for `UTC`.

```scala
  // ...

  // declared

  def ζ[Z]: UTC[Z]
```

`UtcUnit` features are declared.

Back to [Plus](#plus)

### Category

Back to [Universe](#universe)

Back to [Functor](#functor)

Back to [ActingUpon](#actingupon)

Back to [Triple](#triple)

Back to [NaturalTransformation](#naturaltransformation)

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

```scala
package specification

trait Category[BTC[_, _]] extends BtcComposition[BTC], BtcUnit[BTC]:
  // ...
```

`Category` is a binary type constructor class for parameter `BTC`.

`BtcComposition` is fully explained in [BtcComposition](#btccomposition)

`BtcUnit` is fully explained in [BtcUnit](#btcunit)

The laws of `Category`, `CategoryLaws` are fully defined in [CategoryLaws](#categorylaws).

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

Back to [NaturalTransformation](#naturaltransformation)

Back to [Triple](#triple)

Back to [ActingUpon](#actingupon)

Back to [Functor](#functor)

Back to [Universe](#universe)

### BtcComposition

Back to [Category](#category)

```scala
package specification

trait BtcComposition[BTC[_, _]]:
```

`BtcComposition` is a binary type constructor class for parameter `BTC`.

```scala
  // declared

  extension [Z, Y, X](yμx: BTC[Y, X]) def `o`(zμy: BTC[Z, Y]): BTC[Z, X]
```

`BtcComposition` features are declared.

The laws of `BtcComposition`, `BtcCompositionLaws` are fully defined in [BtcCompositionLaws](#btccompositionlaws).

Back to [Category](#category)

### BtcUnit

Back to [Category](#category)

```scala
package specification

trait BtcUnit[BTC[_, _]]:

  // ...

```

`BtcUnit` is a binary type constructor class for parameter `BTC`.

```scala
  // ...

  // declared

  def ι[Z]: BTC[Z, Z]

```

`BtcUnit` features are declared.

Back to [Category](#category)

### ActingUponFunction

Back to [Universe](#universe)

```scala
package specification

type ActingUponFunction = [BTC[_, _]] =>> ActingUpon[Function, BTC]
```

`ActingUpon` is fully explained in [ActingUpon](#actingupon).

Back to [Universe](#universe)

### ActingUpon

Back to [ActingUponFunction](#actinguponfunction)

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

```scala
package specification

trait ActingUpon[LBTC[_, _], RBTC[_, _]: Category]:

  // ...
```

`ActingUpon` is a binary type constructor class for `LBTC`.

`ActingUpon` has a parameter that is required to be a `Category`binary type constructor.

`Category` is fully explained in [Category](#category).

```scala
  // ...

  // declared

  def actionFunctor[Z]: Functor[RBTC, Function, [Y] =>> LBTC[Z, Y]]

  // ...
```

`ActingUpon` features are declared.

```scala
  // ...

  // defined

  extension [Z, Y, X](lyμx: RBTC[Y, X])
    def `a`(rzμy: LBTC[Z, Y]): LBTC[Z, X] = actionFunctor.φ(lyμx)(rzμy)

  // ...
```

`ActingUpon` members are defined.

`Functor` is fully explained in [Functor](#functor).

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

Back to [ActingUponFunction](#actinguponfunction)

### CompositionNaturalTransformation

Back to [Triple](#triple)

```scala
package specification

import types.{`o`}

trait CompositionNaturalTransformation[
    BTC[_, _]: Category,
    UTC[_]: [_[_]] =>> Functor[BTC, BTC, UTC]
]:

  // ...
```

`` `o` `` is fully explained in [Types](#types).

`CompositionNaturalTransformation` is a unary type constructor class for `UTC`.

```scala
  // ...

  // declared

  val compositionNaturalTransformation: NaturalTransformation[
    BTC,
    BTC,
    UTC `o` UTC,
    UTC
  ]

  // ...
```

`CompositionNaturalTransformation` features are declared.

`NaturalTransformation` is fully explained in [NaturalTransformation](#naturaltransformation)

```scala
  // ...

  // defined

  def μ[Z]: BTC[(UTC `o` UTC)[Z], UTC[Z]] = compositionNaturalTransformation.τ

```

`CompositionNaturalTransformation` members are defined.

Back to [Triple](#triple)

### UnitNaturalTransformation

Back to [Triple](#triple)

```scala
package specification

import types.{U}

trait UnitNaturalTransformation[
    BTC[_, _]: Category,
    UTC[_]: [_[_]] =>> Functor[BTC, BTC, UTC]
]:
```

`U` is fully explained in [Types](#types).

`UnitNaturalTransformation` is a unary type constructor class for `UTC`.

```scala
  // ...

  // declared

  val unitNaturalTransformation: NaturalTransformation[BTC, BTC, U, UTC]

  // ...
```

`UnitNaturalTransformation` features are declared.

`NaturalTransformation` is fully explained in [NaturalTransformation](#naturaltransformation)

```scala
  // ...

  // defined

  def ν[Z]: BTC[U[Z], UTC[Z]] = unitNaturalTransformation.τ
```

`UnitNaturalTransformation` members are defined.

Back to [Triple](#triple)

### NaturalTransformation

Back to [CompositionNaturalTransformation](#compositionnaturaltransformation)

Back to [UnitNaturalTransformation](#unitnaturaltransformation)

```scala
package specification

trait NaturalTransformation[
    FBTC[_, _],
    TBTC[_, _]: Category,
    FUTC[_]: [_[_]] =>> Functor[FBTC, TBTC, FUTC],
    TUTC[_]: [_[_]] =>> Functor[FBTC, TBTC, TUTC]
] extends Transformation[FBTC, TBTC, FUTC, TUTC]:

  // ...
```

`NaturalTransformation` is a value class.

`NaturalTransformation` has two parameters, `FBTC` and `TBTC` that are required to be `Category` binary type
constructors.

`NaturalTransformation` has two parameters, `FUTC` and `TUTC` that are required to be `Functor` unary type constructors.

`Category` is fully explained in [Category](#category)

`Functor` is fully explained in [Functor](#functor)

`Transformation` is fully explained in [Transformation](#transformation)

The laws of `NaturalTransformation`, `NaturalTransformationLaws` are fully defined in
[NaturalTransformationLaws](#naturaltransformationlaws).

Back to [UnitNaturalTransformation](#unitnaturaltransformation)

Back to [CompositionNaturalTransformation](#compositionnaturaltransformation)

### ActingUponNaturalTransformation

```scala
package specification

trait ActingUponNaturalTransformation[
    FBTC[_, _]: Category: [_[_, _]] =>> ActingUpon[FBTC, TBTC],
    TBTC[_, _]: Category,
    FUTC[_]: [_[_]] =>> Functor[TBTC, FBTC, FUTC],
    TUTC[_]: [_[_]] =>> Functor[TBTC, TBTC, TUTC]
] extends Transformation[TBTC, FBTC, FUTC, TUTC]:

  // ...
```

`ActingUponNaturalTransformation` is a value class.

`ActingUponNaturalTransformation` has a parameter, `FBTC` that is required to be `Category` binary type constructor and
an `ActingUpon` binary type constructor.

`ActingUponNaturalTransformation` has a parameter, `TBTC` that is required to be `Category` binary type constructor

`NaturalTransformation` has two parameters, `FUTC` and `TUTC` that are required to be `Functor` unary type constructors.

`Category` is fully explained in [Category](#category)

`ActingUpon` is fully explained in [ActingUpon](#actingupon)

`Functor` is fully explained in [Functor](#functor)

`Transformation` is fully explained in [Transformation](#transformation)

The laws of `ActedUpoNaturalTransformation`, `ActingUponNaturalTransformationLaws` are fully defined in
[ActingUponNaturalTransformationLaws](#actinguponnaturaltransformationlaws).

### Transformation

Back to [PreThings](#prethings)

Back to [NaturalTransformation](#naturaltransformation)

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

```scala
package specification

trait Transformation[FBTC[_, _], TBTC[_, _], FUTC[_], TUTC[_]]:
```

`Transformation` is a value class.

```scala
  // declared

  def τ[Z]: TBTC[FUTC[Z], TUTC[Z]]
```

`Transformation` features are declared.

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

Back to [NaturalTransformation](#naturaltransformation)

Back to [PreThings](#prethings)

## Mathematical Foundations Domain: Laws

### Law

Back to [OrderedLaws](#orderedlaws)

Back to [TotallyOrderedLaws](#totallyorderedlaws)

Back to [EqualityLaws](#equalitylaws)

```scala
package specification

trait Law[UTC[_]]:

  // ...
```

`Law` is a unary type constructor class for parameter `UTC`.


```scala
  // ...

  // declared

  extension [Z, Y](lly: UTC[Y]) def `=>`(rlz: UTC[Z]): UTC[Z]

  extension [Z](l: Z) def `=`(r: Z): UTC[Z]

  extension [Z](ll: UTC[Z]) def `&`(rl: UTC[Z]): UTC[Z]

  extension [Z](ll: UTC[Z]) def `|`(rl: UTC[Z]): UTC[Z]
```

`Law` features are declared.

Laws are *conditional* *equational* laws with *conjunction* and *disjunction*.

Back to [EqualityLaws](#equalitylaws)

Back to [TotallyOrderedLaws](#totallyorderedlaws)

Back to [OrderedLaws](#orderedlaws)

### OrderedLaws

Back to [Ordered](#ordered)

```scala
  // ...

  // laws

  trait OrderedLaws[L[_]: Law]:

    val reflexive: T => L[Boolean] = t =>
      {
        t `<=` t
      } `=` {
        true
      }

    val antiSymmetric: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `<=` rt `=` true `&` rt `<=` lt `=` true
        } `=>` {
          lt `=` rt `=` true
        }

    val transitive: T => T => T => L[Boolean] = lt =>
      mm =>
        rt =>
          {
            lt `<=` mm `=` true `&` mm `<=` rt `=` true
          } `=>` {
            lt `<=` rt `=` true
          }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Partially ordered sets](https://en.wikipedia.org/wiki/Partially_ordered_set).

`Law` is fully explained in [Law](#law).

Back to [Ordered](#ordered)

### TotallyOrderedLaws

Back to [Ordered](#ordered)

```scala
  // ...

  // laws

  trait TotallyOrderedLaws[L[_]: Law]:

    val stronglyConnected: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `<=` rt `|` rt `<=` lt
        } `=` {
          true
        }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Total order](https://en.wikipedia.org/wiki/Total_order) for more details.

`Law` is fully explained in [Law](#law).

Back to [Ordered](#ordered)

### EqualityLaws

Back to [Equality](#equality)

```scala
  // ...

  // laws

  trait EqualityLaws[L[_]: Law]:

    val reflexive: T => L[Boolean] = t =>
      {
        t `=` t
      } `=` {
        true
      }

    val symmetric: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `=` rt `=` true
        } `=>` {
          rt `=` lt `=` true
        }

    val transitive: T => T => T => L[Boolean] = lt =>
      mm =>
        rt =>
          {
            lt `=` mm `=` true `&` mm `=` rt `=` true
          } `=>` {
            lt `=` rt `=` true
          }
```

Hopefully the laws do not surprise you.

`Law` is fully explained in [Law](#law).

Back to [Equality](#equality)

### MeetLaws

Back to [Meet](#meet)

```scala
  // ...

  // laws

  trait MeetLaws[L[_]: Law]:

    val at = summon[Arbitrary[T]].arbitrary

    val greatestSmallerThanBoth: T => T => L[Boolean] =
      l =>
        r =>
          {
            ((l ∧ r) `<=` l) `=` true `&` ((l ∧ r) `<=` r) `=` true
          } `&` {
            (at `<=` l) `=` true `&` (at `<=` r) `=` true
          } `=>` {
            at `<=` (l ∧ r) `=` true
          }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Meet](https://en.wikipedia.org/wiki/Join_and_meet).

Back to [Meet](#meet)

### JoinLaws

```scala
  // ...

  // laws

  trait JoinLaws[L[_]: Law]:

    val at = summon[Arbitrary[T]].arbitrary

    val smallestGreaterThanBoth: T => T => L[Boolean] =
      l =>
        r =>
          {
            (l `<=` (l ∨ r)) `=` true `&` (r `<=` (l ∨ r)) `=` true
          } `&` {
            (l `<=` at) `=` true `&` (r `<=` at) `=` true
          } `=>` {
            (l ∨ r) `<=` at `=` true
          }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Join](https://en.wikipedia.org/wiki/Join_and_meet).

Back to [Join](#join)

### SupremumLaws

Back to [Supremum](#supremum)


```scala
  // ...

 // laws

  trait SupremumLaws[L[_]: Law]:

    val sets: Sets[Collection] = summon[Sets[Collection]]

    import sets.{map, all}

    val at = summon[Arbitrary[T]].arbitrary

    val smallestGreaterThanAll: Collection[T] => L[Boolean] =
      c =>
        {
          all apply {
            for {
              t <- c
            } yield t `<=` sup(c) `=` true
          }
        } `&` {
          all apply {
            for {
              t <- c
            } yield t `<=` at `=` true
          }
        } `=>` {
          sup(c) `<=` at `=` true
        }

    given join: Join[T]

    val joinAsSupremum: Tuple2[T, T] => L[T] =
      val sets = summon[Sets[Collection]]
      import sets.{collection2}
      (lt, rt) =>
        {
          sup(collection2(lt, rt))
        } `=` {
          lt ∨ rt
        }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Supremum](https://en.wikipedia.org/wiki/Join_and_meet).

Back to [Supremum](#supremum)

### SetLaws

Back to [Sets](#sets)

```scala

  // ...

  // laws

  trait SetLaws[L[_]: Law]:

    trait Collection2Related:

      def unordered[Z]: Tuple2[Z, Z] => L[Collection2[Z]] =
        (l, r) =>
          {
            collection2(l, r)
          } `=` {
            collection2(r, l)
          }

      import implementation.{functionCategory}

      def iso2[Z]: L[Function[Tuple2[Z, Z], Tuple2[Z, Z]]] = {
        identity[Tuple2[Z, Z]]
      } `=` {
        tuple2 `o` collection2
      }

      def osi2[Z]: L[Function[Collection2[Z], Collection2[Z]]] = {
        identity[Collection2[Z]]
      } `=` {
        collection2 `o` tuple2
      }

    trait EqualityRelated:

      def reflexive[Z]: Collection[Z] => L[Boolean] = c =>
        {
          c `=s=` c
        } `=` {
          true
        }

      def symmetric[Z]: Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          rc =>
            {
              lc `=s=` rc `=` true
            } `=>` {
              rc `=s=` lc `=` true
            }

      def transitive[Z]
          : Collection[Z] => Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          mc =>
            rc =>
              {
                { lc `=s=` mc `=` true } `&` { mc `=s=` rc `=` true }
              } `=>` {
                lc `=s=` rc `=` true
              }

    trait OrderedRelated:

      def reflexive[Z]: Collection[Z] => L[Boolean] = c =>
        {
          c `<=s<=` c
        } `=` {
          trueF
        }

      def antiSymmetric[Z]: Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          rc =>
            {
              { lc `<=s<=` rc } `=` true `&` { rc `<=s<=` lc } `=` true
            } `=>` {
              lc `=s=` rc `=` true
            }

      def transitive[Z]
          : Collection[Z] => Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          mc =>
            rc =>
              {
                { lc `<=s<=` mc } `=` true `&` { mc `<=s<=` rc } `=` true
              } `=>` {
                { lc `<=s<=` rc } `=` true
              }
```

Hopefully the laws do not surprise you.

`Law` is fully explained in [Law](#law).

Back to [Sets](#sets)

### FunctorLaws

Back to [Functor](#functor)

```scala
  // ...

  // laws

  trait FunctorLaws[L[_]: Law]:

    def identity[Z]: L[TBTC[UTC[Z], UTC[Z]]] =
      val fbtc = summon[Category[FBTC]]
      val tbtc = summon[Category[TBTC]]
      φ(fbtc.ι[Z]) `=` tbtc.ι[UTC[Z]]

    def composition[Z, Y, X]
        : FBTC[Z, Y] => FBTC[Y, X] => L[TBTC[UTC[Z], UTC[X]]] =
      fzμy =>
        fyμx =>
          {
            φ(fyμx `o` fzμy)
          } `=` {
            φ(fyμx) `o` φ(fzμy)
          }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Functor](https://en.wikipedia.org/wiki/Functor).

Back to [Functor](#functor)

### MonadPlusLaws

Back to [MonadPlus](#monadplus)

```scala
  // ...

  // laws

  trait MonadPlusLaws[L[_]: Law]:

    def mappingOverZero[Z, Y]: Function[Z, Y] => L[UTC[Y]] =
      zφy =>
        {
          for {
            z <- ζ[Z]
          } yield {
            zφy(z)
          }
        } `=` {
          ζ
        }

    def mappingOverPlus[Z, Y]: Function[Z, Y] => UTC[Z] => UTC[Z] => L[UTC[Y]] =
      zφy =>
        lutc =>
          rutc =>
            {
              for {
                z <- lutc `+` rutc
              } yield {
                zφy(z)
              }
            } `=` {
              {
                for {
                  lz <- lutc
                } yield {
                  zφy(lz)
                }
              } `+` {
                for {
                  lz <- rutc
                } yield {
                  zφy(lz)
                }
              }
            }

    def flatMappingWithIdentityOverZero[Z]: L[UTC[Z]] = {
      for {
        utcz <- ζ[UTC[Z]]
        z <- utcz
      } yield {
        identity(z)
      }
    } `=` {
      ζ
    }

    def flatMappingWithPlus[Z]: UTC[UTC[Z]] => UTC[UTC[Z]] => L[UTC[UTC[Z]]] =
      lutcutcz =>
        rutcutcz =>
          {
            for {
              lutcz <- lutcutcz
              rutcz <- rutcutcz
            } yield {
              lutcz `+` rutcz
            }
          } `=` {
            lutcutcz `+` rutcutcz
          }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Monad](https://en.wikipedia.org/wiki/Monad_(functional_programming))

Back to [MonadPlus](#monadplus)

### PlusLaws

Back to [Plus](#plus)

```scala
  // ...

  // laws

  trait PlusLaws[L[_]: Law]:

    def leftZero[Z]: UTC[Z] => L[UTC[Z]] =
      utc =>
        {
          ζ `+` utc
        } `=` {
          utc
        }

    def rightZero[Z]: UTC[Z] => L[UTC[Z]] =
      utc =>
        {
          utc `+` ζ
        } `=` {
          utc
        }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Monad](https://en.wikipedia.org/wiki/Monad_(functional_programming))

Back to [Plus](#plus)

### TripleLaws

Back to [Triple](#triple)

```scala
  // ...

  // laws

  import specification.{Law}

  trait TripleLaws[L[_]: Law]:

    import types.{`o`}

    import c.{ι}

    def associativity[Z]: L[BTC[(UTC `o` UTC `o` UTC)[Z], UTC[Z]]] = {
      μ `o` μ
    } `=` {
      μ `o` φ(μ)
    }

    def leftIdentity[Z]: L[BTC[UTC[Z], UTC[Z]]] = {
      μ `o` ν
    } `=` {
      ι
    }

    def rightIdentity[Z]: L[BTC[UTC[Z], UTC[Z]]] = {
      μ `o` φ(ν)
    } `=` {
      ι
    }
```

`` `o` `` is fully explained in [Types](#types).

Hopefully the requirements do not surprise you.

Anyway, see, for example, [Monad (category theory)](https://en.wikipedia.org/wiki/Monad_(category_theory)) for more
details.

Back to [Triple](#triple)

### UtcCompositionLaws

Back to [UtcComposition](#utccomposition)

```scala
   // ...

  // laws

  trait UtcCompositionLaws[L[_]: Law]:

    def associativity[Z]: UTC[Z] => UTC[Z] => UTC[Z] => L[UTC[Z]] =
      lutc =>
        mutc =>
          rutc =>
            {
              (lutc `+` mutc) `+` rutc
            } `=` {
              lutc `+` (mutc `+` rutc)
            }
```

Back to [UtcComposition](#utccomposition)

### CategoryLaws

Back to [Category](#category)

```scala
  // ...

  // laws

  trait CategoryLaws[L[_]: Law]:

    def leftIdentity[Z, Y]: BTC[Z, Y] => L[BTC[Z, Y]] = zμy =>
      {
        ι `o` zμy
      } `=` {
        zμy
      }

    def rightIdentity[Z, Y]: BTC[Z, Y] => L[BTC[Z, Y]] = zμy =>
      {
        zμy `o` ι
      } `=` {
        zμy
      }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Category](https://en.wikipedia.org/wiki/Category).

Back to [Category](#category)

### BtcCompositionLaws

Back to [BtcComposition](#btccomposition)

```scala
  // ...

  // laws

  trait CompositionLaws[L[_]: Law]:

    def associativity[Z, Y, X, W]
        : BTC[X, W] => BTC[Y, X] => BTC[Z, Y] => L[BTC[Z, W]] =
      xμw =>
        yμx =>
          zμy =>
            {
              (xμw `o` yμx) `o` zμy
            } `=` {
              xμw `o` (yμx `o` zμy)
            }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Category](https://en.wikipedia.org/wiki/Category).

Back to [BtcComposition](#btccomposition)

### NaturalTransformationLaws

Back to [NaturalTransformation](#naturaltransformation)

```scala
  // laws

  trait EqualityNaturalTransformationLaws[L[_]: Law](
      transformation: Transformation[FBTC, TBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[FBTC, TBTC, FUTC]]

    val tutc = summon[Functor[FBTC, TBTC, TUTC]]

    def natural[Z, Y](using
        equality: Equality[TBTC[FUTC[Z], TUTC[Y]]]
    ): FBTC[Z, Y] => L[Boolean] =
      fzφy =>
        {
          {
            transformation.τ `o` futc.φ(fzφy)
          } `=` {
            tutc.φ(fzφy) `o` transformation.τ
          }
        } `=` {
          true
        }

  trait OrderedNaturalTransformationLaws[L[_]: Law](
      transformation: Transformation[FBTC, TBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[FBTC, TBTC, FUTC]]

    val tutc = summon[Functor[FBTC, TBTC, TUTC]]

    def natural[Z, Y](using
        ordered: Ordered[TBTC[FUTC[Z], TUTC[Y]]]
    ): FBTC[Z, Y] => L[Boolean] =
      fzφy =>
        {
          {
            transformation.τ `o` futc.φ(fzφy)
          }
        } `<=` {
          {
            tutc.φ(fzφy) `o` transformation.τ
          }
        } `=` {
          true
        }
```

Hopefully the laws do not surprise you.

Anyway, see, for example, [Natural Transformation](https://en.wikipedia.org/wiki/Natural_transformation).

Back to [BtcComposition](#btccomposition)

Back to [NaturalTransformation](#naturaltransformation)

### ActingUponNaturalTransformationLaws

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

```scala
  // ...

  // laws

  trait ActingUponNaturalTransformationLaws[L[_]: Law](
      transformation: NaturalTransformation[RBTC, LBTC, FUTC, TUTC]
  ):

    val futc = summon[Functor[RBTC, LBTC, FUTC]]

    val tutc = summon[Functor[RBTC, RBTC, TUTC]]

    def natural[Z, Y]: RBTC[Z, Y] => L[LBTC[FUTC[Z], TUTC[Y]]] =
      fzφy =>
        {
          transformation.τ `o` futc.φ(fzφy)
        } `=` {
          tutc.φ(fzφy) `a` transformation.τ
        }
```

Back to [ActingUponNaturalTransformation](#actinguponnaturaltransformation)

## Mathematical Foundations Domain: Implementations

### functionValuedFunctor2

Back to [PreThings](#prethings)

```scala
package implementation

import specification.{Sets, Category, Functor}

given functionValuedFunctor2[
    Collection[_]: Sets,
    BTC[_, _]: Category,
    UTC1[_]: [_[_]] =>> Functor[BTC, Function, UTC1]
]: Functor[
  BTC,
  Function,
  [Z] =>> Collection[UTC1[Z]]
] with

  val sets: Sets[Collection] = summon[Sets[Collection]]

  val btcToFunctionFunctor = summon[Functor[BTC, Function, UTC1]]

  import types.{`o`}

  import sets.{Collection2, tuple2, collection2}

  type UTC2 = [Z] =>> (Collection2 `o` UTC1)[Z]
  def φ[Z, Y]: BTC[Z, Y] => Function[UTC2[Z], UTC2[Y]] =
    zμy =>
      val zφy = btcToFunctionFunctor.φ(zμy)
      d =>
        tuple2(d) match
          case (l, r) => collection2(zφy(l), zφy(r))
```

Back to [PreThings](#prethings)

### orderedCategory

Back to [PreThings](#prethings)

```scala
package implementation

import specification.{Arbitrary, Ordered, Sets, Category}

given orderedCategory[Collection[_]: Sets, T: Arbitrary: Ordered]
    : Category[[_, _] =>> Tuple2[T, T]] with

  type BTC = [_, _] =>> Tuple2[T, T]

  extension [Z, Y, X](yμx: BTC[Y, X])
    def `o`(zμy: BTC[Z, Y]): BTC[Z, X] =
      (yμx, zμy) match
        case ((llt, lrt), (rlt, rrt)) =>
          require(llt `<=` lrt && lrt == rlt && rlt `<=` rrt)
          (llt, rrt)

  def ι[Z]: BTC[Z, Z] =
    val at = summon[Arbitrary[T]].arbitrary
    (at, at)
```

Back to [PreThings](#prethings)

### functionCategory

Back to [PreThings](#prethings)

```scala
package implementation

import specification.{Category, ActingUponFunction, Functor}

given functionCategory: Category[Function] with

  type BTC = [Z, Y] =>> Function[Z, Y]

  extension [Z, Y, X](yμx: BTC[Y, X])
    def `o`(zμy: BTC[Z, Y]): BTC[Z, X] = z => yμx(zμy(z))

  def ι[Z]: BTC[Z, Z] = z => z

given functionFunctionActingUpon: ActingUponFunction[Function] with

  type BTC = [Z, Y] =>> Function[Z, Y]

  def actionFunctor[Z]: Functor[BTC, Function, [Y] =>> Function[Z, Y]] =
    new:
      def φ[Y, X]: Function[
        BTC[Y, X],
        Function[Function[Z, Y], Function[Z, X]]
      ] = yμx => zμy => yμx `o` zμy
```

Back to [PreThings](#prethings)

### composedFunctor

Back to [PreThingsLaws](#prethingslaws)

```scala
package implementation

import specification.{Category, Functor}

import types.{`o`}

given composedFunctor[
    FBTC[_, _]: Category,
    MBTC[_, _]: Category,
    TBTC[_, _]: Category,
    F2MUTC[_]: [_[_]] =>> Functor[FBTC, MBTC, F2MUTC],
    M2TUTC[_]: [_[_]] =>> Functor[MBTC, TBTC, M2TUTC]
]: Functor[FBTC, TBTC, M2TUTC `o` F2MUTC] with

  type UTC = [Z] =>> (M2TUTC `o` F2MUTC)[Z]

  def φ[Z, Y]: Function[FBTC[Z, Y], TBTC[UTC[Z], UTC[Y]]] =
    summon[Functor[MBTC, TBTC, M2TUTC]].φ `o`
      summon[Functor[FBTC, MBTC, F2MUTC]].φ
```

Back to [PreThingsLaws](#prethingslaws)

### functionTargetOrdered

Back to [PreThingsLaws](#prethingslaws)

```scala
package implementation

import specification.{Arbitrary, Ordered}

given functionTargetOrdered[Z: Arbitrary, Y: Ordered]: Ordered[Function[Z, Y]]
with

  val az: Z = summon[Arbitrary[Z]].arbitrary

  val ordered: Ordered[Y] = summon[Ordered[Y]]

  import ordered.{`=` => `=t=`, `<` => `<t<`}

  type T = Function[Z, Y]

  extension (lt: T) def `=`(rt: T): Boolean = lt(az) `=t=` rt(az)

  extension (lt: T) def `<`(rt: T): Boolean = lt(az) `<t<` rt(az)
```

Back to [PreThingsLaws](#prethingslaws)

### setOrdered

Back to [PreThingsLaws](#prethingslaws)

```scala
package implementation

import specification.{Sets, Ordered}

given setOrdered[Z, Collection[_]: Sets]: Ordered[Collection[Z]] with

  val sets: Sets[Collection] = summon[Sets[Collection]]

  import sets.{`=s=`, `<s<`}

  type T = [Z] =>> Collection[Z]

  extension (lt: T[Z]) def `=`(rt: T[Z]): Boolean = lt `=s=` rt

  extension (lt: T[Z]) def `<`(rt: T[Z]): Boolean = lt `<s<` rt
```

Back to [PreThingsLaws](#prethingslaws)
