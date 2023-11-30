package specification

trait Join[T: Ordered: Arbitrary]:

  // declared

  extension (l: T) def ∨(r: T): T

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
