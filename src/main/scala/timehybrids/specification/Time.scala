package timehybrids.specification

import specification.{Arbitrary, Ordered}

trait Time[Moment: Arbitrary: Ordered]:

  // `Time` related foundational delegates are defined

  val ma: Arbitrary[Moment] = summon[Arbitrary[Moment]]

  val mo: Ordered[Moment] = summon[Ordered[Moment]]

  // `Time` related foundational members
  // using members of `Time` related foundational delegates are defined

  val am: Moment = ma.arbitrary
