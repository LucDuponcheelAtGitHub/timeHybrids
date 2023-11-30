package specification

trait Meet[T: Ordered: Arbitrary]:

  // declared

  extension (l: T) def ∧(r: T): T

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
