package specification

trait Meet[T: Ordered: Arbitrary]:

  // declared

  extension (lt: T) def ∧(rt: T): T

  // laws

  trait MeetLaws[L[_]: Law]:

    val at = summon[Arbitrary[T]].arbitrary

    val greatestSmallerThanBoth: T => T => L[Boolean] =
      lt =>
        rt =>
          {
            ((lt ∧ rt) `<=` lt) `=` true `&` ((lt ∧ rt) `<=` rt) `=` true
          } `&` {
            (at `<=` lt) `=` true `&` (at `<=` rt) `=` true
          } `=>` {
            at `<=` (lt ∧ rt) `=` true
          }
